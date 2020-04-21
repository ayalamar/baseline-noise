## ANALYZE ALIGNED REACHES AND NO CURSORS WITH PROPS
## AND MAKE SOME PLOTS

setwd("/Users/mayala/Desktop/baseline noise")

library(dplyr)
library(tidyr)
library(TOSTER)

colorset <- list()

colorset[['youngactS']] <- '#e51636ff' # "York red"
colorset[['youngactT']] <- '#e516362f'
colorset[['youngpasS']] <- '#ff8200ff' # orange
colorset[['youngpasT']] <- '#ff82002f'

colorset[['agingactS']] <- '#005de4ff' # blue
colorset[['agingactT']] <- '#005de42f'
colorset[['agingpasS']] <- '#2ab2f2ff' # lighter blue
colorset[['agingpasT']] <- '#2ab2f22f'

colorset[['extra1S']] <- '#c400c4ff' # purple
colorset[['extra1T']] <- '#c400c42f'

# STEPS
# get data from output of dataCollection.R from Baseline project
# these have labels in this file of the experiment and subject id
# correct the arc shift for each pp (will differ per pp)
# get mean active localization for each pp
# get mean passive localization for each pp
# run exclusion criteria (note - differs for 60 deg group) - ALSO DO IT WITHOUT EXCLUDE
# detrend data -- NOTE -- THIS IS WHERE IDs got replaced

# loading the localization data
Raging <- read.csv('localization data/Raging.csv', header = TRUE)
aging <- read.csv('localization data/aging.csv', header = TRUE)
Rcntrl <- read.csv('localization data/Rcntrl.csv', header = TRUE)
cntrl <- read.csv('localization data/cntrl.csv', header = TRUE)

# loading the aligned reaches and no cursors
collapsed_df <- read.csv('combined/GROUP COMBINES/collapsed.csv', header = TRUE)

tasks <- unique(collapsed_df$task)

# summarize the data per participant to set it up for doing statistics
aligned_cursor.summary <- collapsed_df %>% 
  filter(task == tasks[1]) %>%
  filter(trial > 46) %>% #remove first 45 familiarization aligned trials
  group_by(subject) %>%
  summarise(mean_error_pv = mean(pv_angle, na.rm = TRUE), 
            sd_error_pv = sd(pv_angle, na.rm = TRUE),
            mean_error_ep = mean(ep_angle, na.rm = TRUE), 
            sd_error_ep = sd(ep_angle, na.rm = TRUE))

aligned_no_cursor.summary <- collapsed_df %>%
  filter(task == tasks[2]) %>%
  group_by(subject) %>%
  summarise(mean_error_pv = mean(pv_angle, na.rm = TRUE), 
            sd_error_pv = sd(pv_angle, na.rm = TRUE),
            mean_error_ep = mean(ep_angle, na.rm = TRUE), 
            sd_error_ep = sd(ep_angle, na.rm = TRUE))

# are there difference bw aligned cursor and nc
t.test(aligned_cursor.summary$sd_error_pv, aligned_no_cursor.summary$sd_error_pv, paired = TRUE)
TOSTpaired.raw(n=length(aligned_cursor.summary$sd_error_pv),
               m1=mean(aligned_cursor.summary$sd_error_pv),
               m2=mean(aligned_no_cursor.summary$sd_error_pv),
               sd1=sd(aligned_cursor.summary$sd_error_pv),
               sd2=sd(aligned_no_cursor.summary$sd_error_pv),
               cor(aligned_cursor.summary$sd_error_pv,aligned_no_cursor.summary$sd_error_pv),
               low_eqbound = -.5,      # lower bound in Cohen's d's
               high_eqbound = .5)      # upper bound in Cohen's d's
#plot(aligned_cursor.summary$sd_error_ep, aligned_no_cursor.summary$sd_error_ep, asp = 1)

# correlate motor noise and baseline localization variability?
# with exclusion criteria 
aging <- aging %>%
  select(agegroup, group, trial, active_bool, localization_deg, ID)
cntrl <- cntrl %>%
  select(agegroup, group, trial, active_bool, localization_deg, ID)
ncdf <- rbind(cntrl, aging) # contains ALL localizations 

# summarize prop data for doing statistics
ncdf.summary <- ncdf %>% 
  group_by(ID, active_bool) %>%
  summarise(mean_loc = mean(localization_deg, na.rm = TRUE), 
            sd_loc = sd(localization_deg, na.rm = TRUE))

ncdf.summary$subject <- ncdf.summary$ID

ncdf.summary_0 <- ncdf.summary %>%
  filter(active_bool == 0)
ncdf.summary_1 <- ncdf.summary %>%
  filter(active_bool == 1)

#### DO REGRESSIONS WITH ALIGNED CURSOR trials
aligned_cursor.summary <- aligned_cursor.summary %>%
  semi_join(ncdf.summary, by ='subject') %>%
  mutate(task_name = "Aligned cursor")
# this keeps only aligned_cursor data for those w/ matching localizations (to match exclusion criteria N)

# predict passive loc with aligned error sd 
plot(aligned_cursor.summary$sd_error_ep, ncdf.summary_0$sd_loc, asp = 1)
cor.test(aligned_cursor.summary$sd_error_ep, ncdf.summary_0$sd_loc) # no relationship 
modeldf1 <- left_join(aligned_cursor.summary, ncdf.summary_0, by = "subject")
mod1 <- lm(sd_loc ~ sd_error_ep, data = modeldf1)
summary(mod1) 

# predict active loc with aligned error sd 
plot(aligned_cursor.summary$sd_error_ep, ncdf.summary_1$sd_loc, asp = 1)
cor.test(aligned_cursor.summary$sd_error_ep, ncdf.summary_1$sd_loc) # no relationship
modeldf2 <- left_join(aligned_cursor.summary, ncdf.summary_1, by = "subject")
mod2 <- lm(sd_loc ~ sd_error_ep, data = modeldf2)
summary(mod2) 

#### DO REGRESSIONS WITH ALIGNED NO CURSORS
aligned_no_cursor.summary <- aligned_no_cursor.summary %>%
  semi_join(ncdf.summary, by ='subject') %>%
  mutate(task_name = "Aligned no-cursor")

# predict passive loc with aligned NO CURSOR error sd 
plot(aligned_no_cursor.summary$sd_error_ep,ncdf.summary_0$sd_loc, asp = 1)
cor.test(aligned_no_cursor.summary$sd_error_ep, ncdf.summary_0$sd_loc) # no relationship  p-value =0.07123
modeldf3 <- left_join(aligned_no_cursor.summary, ncdf.summary_0, by="subject")
mod3 <- lm(sd_loc ~ sd_error_ep, data = modeldf3)
summary(mod3) # no p = 0.4516

# predict active loc with aligned NO CURSOR error sd 
plot(aligned_no_cursor.summary$sd_error_ep, ncdf.summary_1$sd_loc, asp = 1)
cor.test(aligned_no_cursor.summary$sd_error_ep, ncdf.summary_1$sd_loc) # no relationship  p-value = 0.3346, r= -.07
modeldf4 <- left_join(aligned_no_cursor.summary, ncdf.summary_1, by="subject")
mod4 <- lm(sd_loc ~ sd_error_ep, data = modeldf4)
summary(mod4) # no p = 0.699 

# decided the bounds
# powerTOSTr(alpha=0.05, statistical_power=0.9, N = 226)
# equivalence test
chy <- TOSTr(n = 226,
             r = 0.09332826,
             low_eqbound_r=-0.5, 
             high_eqbound_r=0.5,
             alpha=0.05)
chy[['TOST_p2']]

rsq <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- lm(formula, data=d)
  return(summary(fit)$r.square)
}
results <- boot(data=modeldf4, statistic=rsq,R=1000, formula=sd_loc~sd_error_ep)
boot.ci(results, type="bca")

################################
## BIG PLOT
## USING previous style
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE),
       widths=c(2,2,2,2), heights=c(1,1,1,1))

plot(modeldf1$sd_error_pv, modeldf1$sd_loc,
     main=sprintf('Aligned cursor - passive localization'),
     xlim=c(0,30),
     ylim=c(0,60),
     axes=F,
     xlab='motor error variability [σ]',
     ylab='localization variability [σ]')

regr <- lm(modeldf1$sd_loc~modeldf1$sd_error_ep)
#abline(coef=regr$coefficients,col=rgb(0.5,0.5,0.5))
cortest <- cor.test(modeldf1$sd_error_ep,modeldf1$sd_loc)

text(25,25,sprintf('r=%0.3f\np=%0.3f\nn=%d',
                   cortest$estimate,
                   cortest$p.value,
                   length(modeldf1$sd_error_ep)),pos=4)
axis(side=1,at=c(0,5,10,15,20,25,30))
axis(side=2,at=c(0,10,20,30,40,50,60))

plot(modeldf2$sd_error_pv, modeldf2$sd_loc,
     main=sprintf('Aligned cursor - active localization'),
     xlim=c(0,30),
     ylim=c(0,60),
     axes=F,
     xlab='motor error variability [σ]',
     ylab='localization variability [σ]')
regr <- lm(modeldf2$sd_loc~modeldf2$sd_error_ep)
#abline(coef=regr$coefficients,col=rgb(0.5,0.5,0.5))
cortest <- cor.test(modeldf2$sd_error_ep,modeldf2$sd_loc)
text(25,25,sprintf('r=%0.3f\np=%0.3f\nn=%d',cortest$estimate,cortest$p.value,length(modeldf2$sd_error_ep)),pos=4)
axis(side=1,at=c(0,5,10,15,20,25,30))
axis(side=2,at=c(0,10,20,30,40,50,60))

plot(modeldf3$sd_error_ep, modeldf3$sd_loc,
     main=sprintf('Aligned no-cursor - passive localization'),
     xlim=c(0,30),
     ylim=c(0,60),
     axes=F,
     xlab='motor error variability [σ]',
     ylab='localization variability [σ]')
regr <- lm(modeldf3$sd_loc~modeldf3$sd_error_ep)
#abline(coef=regr$coefficients,col=rgb(0.5,0.5,0.5))
cortest <- cor.test(modeldf3$sd_error_ep,modeldf3$sd_loc)
text(25,25,sprintf('r=%0.3f\np=%0.3f\nn=%d',cortest$estimate,cortest$p.value,length(modeldf3$sd_error_ep)),pos=4)
axis(side=1,at=c(0,5,10,15,20,25,30))
axis(side=2,at=c(0,10,20,30,40,50,60))

plot(modeldf4$sd_error_ep, modeldf4$sd_loc,
     main=sprintf('Aligned no-cursor - active localization'),
     xlim=c(0,30),
     ylim=c(0,60),
     axes=F,
     xlab='motor error variability [σ]',
     ylab='localization variability [σ]')
regr <- lm(modeldf4$sd_loc~modeldf4$sd_error_ep)
#abline(coef=regr$coefficients,col=rgb(0.5,0.5,0.5))
cortest <- cor.test(modeldf4$sd_error_ep,modeldf4$sd_loc)
text(25,25,sprintf('r=%0.3f\np=%0.3f\nn=%d',cortest$estimate,cortest$p.value,length(modeldf4$sd_error_ep)),pos=4)
axis(side=1,at=c(0,5,10,15,20,25,30))
axis(side=2,at=c(0,10,20,30,40,50,60))

experiment_Ns <- collapsed_df %>%
  group_by(exp_group) %>%
  distinct(subject, exp_group) %>%
  tally()
  


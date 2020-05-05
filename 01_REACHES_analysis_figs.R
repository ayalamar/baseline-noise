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

cntrl <- read.csv('young_varianceCIs.csv', header = TRUE)
aging <- read.csv('aging_varianceCIs.csv', header = TRUE)

# put ids back
id_df <- read.csv('id_key.csv', header = TRUE)

aging <- aging %>%
  full_join(id_df, by = "participant") %>%
  drop_na(group)
cntrl <- cntrl %>%
  full_join(id_df, by = "participant") %>%
  drop_na(group) 
  
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
# aging <- aging %>%
#   select(agegroup, group, trial, active_bool, localization_deg, localization_sd, ID)
# cntrl <- cntrl %>%
#   select(agegroup, group, trial, active_bool, localization_deg, ID)
ncdf <- rbind(cntrl, aging) # contains ALL localizations 

# summarize prop data for doing statistics
ncdf.summary <- ncdf %>% 
  select(group, id, a500, p500) 
  # group_by(ID, active_bool) %>%
  # summarise(mean_loc = mean(localization_deg, na.rm = TRUE), 
  #           sd_loc = sd(localization_deg, na.rm = TRUE))

ncdf.summary$subject <- ncdf.summary$id
# 
# ncdf.summary_0 <- ncdf.summary %>%
#   filter(active_bool == 0)
# ncdf.summary_1 <- ncdf.summary %>%
#   filter(active_bool == 1)

#### DO REGRESSIONS WITH ALIGNED CURSOR trials
aligned_cursor.summary <- aligned_cursor.summary %>%
  semi_join(ncdf.summary, by ='subject') %>%
  mutate(task_name = "Aligned cursor")
# this keeps only aligned_cursor data for those w/ matching localizations (to match exclusion criteria N)

# predict passive loc with aligned error sd 
plot(aligned_cursor.summary$sd_error_ep, ncdf.summary$p500, asp = 1)
cor.test(aligned_cursor.summary$sd_error_ep, ncdf.summary$p500) # no relationship 
modeldf1 <- left_join(aligned_cursor.summary, ncdf.summary, by = "subject")
mod1 <- lm(p500 ~ sd_error_ep, data = modeldf1)
summary(mod1) 

# predict active loc with aligned error sd 
plot(aligned_cursor.summary$sd_error_ep, ncdf.summary$a500, asp = 1)
cor.test(aligned_cursor.summary$sd_error_ep, ncdf.summary$a500) # no relationship
modeldf2 <- left_join(aligned_cursor.summary, ncdf.summary, by = "subject")
mod2 <- lm(a500~ sd_error_ep, data = modeldf2)
summary(mod2) 

#### DO REGRESSIONS WITH ALIGNED NO CURSORS
aligned_no_cursor.summary <- aligned_no_cursor.summary %>%
  semi_join(ncdf.summary, by ='subject') %>%
  mutate(task_name = "Aligned no-cursor")

# predict passive loc with aligned NO CURSOR error sd 
plot(aligned_no_cursor.summary$sd_error_ep,ncdf.summary$p500, asp = 1)
cor.test(aligned_no_cursor.summary$sd_error_ep, ncdf.summary$p500) # no relationship  p-value =0.07123
modeldf3 <- left_join(aligned_no_cursor.summary, ncdf.summary, by="subject")
mod3 <- lm(p500 ~ sd_error_ep, data = modeldf3)
summary(mod3)

# predict active loc with aligned NO CURSOR error sd 
plot(aligned_no_cursor.summary$sd_error_ep, ncdf.summary$a500, asp = 1)
cor.test(aligned_no_cursor.summary$sd_error_ep, ncdf.summary$a500) # no relationship  p-value = 0.3346, r= -.07
modeldf4 <- left_join(aligned_no_cursor.summary, ncdf.summary, by="subject")
mod4 <- lm(a500 ~ sd_error_ep, data = modeldf4)
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

plot(modeldf1$sd_error_pv, sqrt(modeldf1$p500),
     main=sprintf('Aligned cursor - passive localization'),
     xlim=c(0,30),
     ylim=c(0,15),
     axes=F,
     xlab='motor error variability [σ]',
     ylab='localization variability [σ]')

regr <- lm(sqrt(modeldf1$p500)~modeldf1$sd_error_ep)
#abline(coef=regr$coefficients,col=rgb(0.5,0.5,0.5))
cortest <- cor.test(modeldf1$sd_error_ep,sqrt(modeldf1$p500))

text(25,25,sprintf('r=%0.3f\np=%0.3f\nn=%d',
                   cortest$estimate,
                   cortest$p.value,
                   length(modeldf1$sd_error_ep)),pos=4)
axis(side=1,at=c(0,5,10,15,20,25,30))
axis(side=2,at=c(0,10,20,30,40,50,60))

plot(modeldf2$sd_error_pv, sqrt(modeldf2$a500),
     main=sprintf('Aligned cursor - active localization'),
     xlim=c(0,30),
     ylim=c(0,15),
     axes=F,
     xlab='motor error variability [σ]',
     ylab='localization variability [σ]')
regr <- lm(sqrt(modeldf2$a500)~modeldf2$sd_error_ep)
#abline(coef=regr$coefficients,col=rgb(0.5,0.5,0.5))
cortest <- cor.test(modeldf2$sd_error_ep,sqrt(modeldf2$a500))
text(25,25,sprintf('r=%0.3f\np=%0.3f\nn=%d',
                   cortest$estimate,
                   cortest$p.value,
                   length(modeldf2$sd_error_ep)),pos=4)
axis(side=1,at=c(0,5,10,15,20,25,30))
axis(side=2,at=c(0,10,20,30,40,50,60))

plot(modeldf3$sd_error_ep, sqrt(modeldf3$p500),
     main=sprintf('Aligned no-cursor - passive localization'),
     xlim=c(0,30),
     ylim=c(0,15),
     axes=F,
     xlab='motor error variability [σ]',
     ylab='localization variability [σ]')
regr <- lm(sqrt(modeldf3$p500)~modeldf3$sd_error_ep)
#abline(coef=regr$coefficients,col=rgb(0.5,0.5,0.5))
cortest <- cor.test(modeldf3$sd_error_ep,sqrt(modeldf3$p500))
text(25,25,sprintf('r=%0.3f\np=%0.3f\nn=%d',
                   cortest$estimate,
                   cortest$p.value,
                   length(modeldf3$sd_error_ep)),pos=4)
axis(side=1,at=c(0,5,10,15,20,25,30))
axis(side=2,at=c(0,10,20,30,40,50,60))

plot(modeldf4$sd_error_ep, sqrt(modeldf4$a500),
     main=sprintf('Aligned no-cursor - active localization'),
     xlim=c(0,30),
     ylim=c(0,15),
     axes=F,
     xlab='motor error variability [σ]',
     ylab='localization variability [σ]')
regr <- lm(sqrt(modeldf4$a500)~modeldf4$sd_error_ep)
#abline(coef=regr$coefficients,col=rgb(0.5,0.5,0.5))
cortest <- cor.test(modeldf4$sd_error_ep,sqrt(modeldf4$a500))
text(25,25,sprintf('r=%0.3f\np=%0.3f\nn=%d',
                   cortest$estimate,
                   cortest$p.value,
                   length(modeldf4$sd_error_ep)),pos=4)
axis(side=1,at=c(0,5,10,15,20,25,30))
axis(side=2,at=c(0,10,20,30,40,50,60))

experiment_Ns <- collapsed_df %>%
  group_by(exp_group) %>%
  distinct(subject, exp_group) %>%
  tally()
  


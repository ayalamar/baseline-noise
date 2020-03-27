setwd("/Users/mayala/Desktop/baseline noise")

library(dplyr)
library(TOSTER)
library(ggplot2)

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
# Macintosh HD⁩ ▸ ⁨Users⁩ ▸ ⁨mayala⁩ ▸ ⁨Dropbox⁩ ▸ ⁨Baseline⁩ ▸ ⁨data
# these have labels in this file of the experiment and subject id
# correct the arc shift for each pp (will differ per pp)
# get mean active localization for each pp
# get mean passive localization for each pp
# run exclusion criteria (note - differs for 60 deg group) - ALSO DO IT WITHOUT EXCLUDE
# detrend data -- NOTE -- THIS IS WHERE IDs got replaced

Raging <- read.csv('localization data/Raging.csv', header = TRUE)
aging <- read.csv('localization data/aging.csv', header = TRUE)
Rcntrl <- read.csv('localization data/Rcntrl.csv', header = TRUE)
cntrl <- read.csv('localization data/cntrl.csv', header = TRUE)

collapsed_df <- read.csv('combined/GROUP COMBINES/collapsed.csv', header = TRUE)

tasks <- unique(collapsed_df$task)

aligned_cursor.summary <- collapsed_df %>% 
  filter(task == tasks[1]) %>%
  filter(trial > 46) %>% #remove first 45 familiarization aligned trials
  group_by(subject) %>%
  summarise(mean_error = mean(pv_angle, na.rm = TRUE), 
            sd_error = sd(pv_angle, na.rm = TRUE))

aligned_no_cursor.summary <- collapsed_df %>%
  filter(task == tasks[2]) %>%
  group_by(subject) %>%
  summarise(mean_error = mean(pv_angle, na.rm = TRUE), 
            sd_error = sd(pv_angle, na.rm = TRUE))

t.test(aligned_cursor.summary$sd_error, aligned_no_cursor.summary$sd_error, paired = TRUE)
plot(aligned_cursor.summary$sd_error, aligned_no_cursor.summary$sd_error, asp = 1)

# correlate motor noise and baseline localization variability?
# with exclusion criteria - N < 200
aging <- aging %>%
  select(agegroup, group, trial, active_bool, localization_deg, ID)
cntrl <- cntrl %>%
  select(agegroup, group, trial, active_bool, localization_deg, ID)
ncdf <- rbind(cntrl, aging) # contains ALL localizations 

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
plot(aligned_cursor.summary$sd_error, ncdf.summary_0$sd_loc, asp = 1)
cor.test(aligned_cursor.summary$sd_error, ncdf.summary_0$sd_loc) # no relationship 
modeldf1 <- left_join(aligned_cursor.summary, ncdf.summary_0, by = "subject")
mod1 <- lm(sd_loc ~ sd_error, data = modeldf1)
summary(mod1) 

# predict active loc with aligned error sd 
plot(aligned_cursor.summary$sd_error, ncdf.summary_1$sd_loc, asp = 1)
cor.test(aligned_cursor.summary$sd_error, ncdf.summary_1$sd_loc) # no relationship
modeldf2 <- left_join(aligned_cursor.summary, ncdf.summary_1, by = "subject")
mod2 <- lm(sd_loc ~ sd_error, data = modeldf2)
summary(mod2) 

#### DO REGRESSIONS WITH ALIGNED NO CURSORS
aligned_no_cursor.summary <- aligned_no_cursor.summary %>%
  semi_join(ncdf.summary, by ='subject') %>%
  mutate(task_name = "Aligned no-cursor")

# predict passive loc with aligned NO CURSOR error sd 
plot(aligned_no_cursor.summary$sd_error,ncdf.summary_0$sd_loc, asp = 1)
cor.test(aligned_no_cursor.summary$sd_error, ncdf.summary_0$sd_loc) # no relationship  p-value =0.07123
modeldf3 <- left_join(aligned_no_cursor.summary, ncdf.summary_0, by="subject")
mod3 <- lm(sd_loc ~ sd_error, data = modeldf3)
summary(mod3) # no p = 0.4516

# predict active loc with aligned NO CURSOR error sd 
plot(aligned_no_cursor.summary$sd_error, ncdf.summary_1$sd_loc, asp = 1)
cor.test(aligned_no_cursor.summary$sd_error, ncdf.summary_1$sd_loc) # no relationship  p-value = 0.3346, r= -.07
modeldf4 <- left_join(aligned_no_cursor.summary, ncdf.summary_1, by="subject")
mod4 <- lm(sd_loc ~ sd_error, data = modeldf4)
summary(mod4) # no p = 0.699 

# equivalence test
TOSTr(n = 189,
      r = 0.09332826,
      low_eqbound_r=-0.5,
      high_eqbound_r=0.5,
      alpha=0.05)


# big plot
plotdf <- rbind(modeldf1, modeldf2, modeldf3, modeldf4)
plotdf <- plotdf %>%
  mutate(loc_task = ifelse(active_bool==0, "Passive", "Active"))

ggplot(plotdf, aes(sd_error, sd_loc, colour = factor(active_bool))) +
  geom_point(shape = 21, size = 1, fill = "white", stroke = 1) +
  labs(y = "Localization variability (σ)",
       x = "Motor error variability (σ)") +
  facet_grid(loc_task ~ task_name) +
  theme_classic() +
  theme(strip.background = element_blank(),
        legend.position = "none") +
  xlim(0, 20) +
  ylim(0, 50)



## previous style
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE),
       widths=c(3,3,2), heights=c(1,1))
color <- colorset[[sprintf('%s%sS',group,substr(task,1,3))]]

plot(plotdf$sd_error,plotdf$sd_loc,
     main=sprintf('%s - %s',group,task),
     # xlim=c(0,15),
     # ylim=c(-10,30),
     axes=F,
     xlab='motor error variability [σ]',
     ylab='localization variability [σ]',
     col=color)
regr <- lm(plotdf$sd_loc~plotdf$sd_error)
abline(coef=regr$coefficients,col=rgb(0.5,0.5,0.5))

cortest <- cor.test(plotdf$sd_error,plotdf$sd_loc)
text(11,25,sprintf('r=%0.3f\np=%0.3f\nn=%d',cortest$estimate,cortest$p.value,length(plotdf$sd_error)),pos=4)

axis(side=1,at=c(0,5,10,15))
axis(side=2,at=c(-10,0,10,20,30))

  
  
  
  

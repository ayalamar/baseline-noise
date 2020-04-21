# get some descriptive stats to report
library(dplyr)

## baseline localizations
aging_loc <- read.csv("data/aging_varianceCIs.csv", header = TRUE)
young_loc <- read.csv("data/young_varianceCIs.csv", header = TRUE)

all_loc <- rbind(aging_loc, young_loc)

# logP500 <- sqrt(all_loc$p500)
# logA500 <- sqrt(all_loc$a500)

# active
 median(all_loc$a500) 
 median(all_loc$a025)
 median(all_loc$a975)
 
 # passive
 median(all_loc$p500) 
 median(all_loc$p025)
 median(all_loc$p975)
 
 
 ## comparing bw age cohorts
 source('R/variance.R')
 source('R/figures.R')
 # active
 TOSTtwo(m1=mean(dfs[["young"]][["a500"]]),
         m2=mean(dfs[["aging"]][["a500"]]),
         sd1=sd(dfs[["young"]][["a500"]]),
         sd2=sd(dfs[["aging"]][["a500"]]),
         n1=length(dfs[["young"]][["a500"]]),
         n2=length(dfs[["aging"]][["a500"]]),
         low_eqbound_d=-0.5,
         high_eqbound_d=0.5,
         alpha = 0.05,
         var.equal=FALSE)
 # passive
 TOSTtwo(m1=mean(dfs[["young"]][["p500"]]),
         m2=mean(dfs[["aging"]][["p500"]]),
         sd1=sd(dfs[["young"]][["p500"]]),
         sd2=sd(dfs[["aging"]][["p500"]]),
         n1=length(dfs[["young"]][["p500"]]),
         n2=length(dfs[["aging"]][["p500"]]),
         low_eqbound_d=-0.5,
         high_eqbound_d=0.5,
         alpha = 0.05,
         var.equal=FALSE)

 ## localization shifts 
aging_shifts <- read.csv("data/aging_rotated_localization.csv", header = TRUE)
young_shifts <- read.csv("data/young_rotated_localization.csv", header = TRUE)

aging_shifts %>%
  group_by(active_bool) %>%
  summarise(median_rot_loc = mean(localizationshift_deg))
 
young_shifts %>%
  group_by(active_bool) %>%
  filter(group %in% c("30implicit","30explicit", "cursorjump", "60implicit", "60explicit","sEDS" )) %>%
  summarise(median_rot_loc = mean(localizationshift_deg))


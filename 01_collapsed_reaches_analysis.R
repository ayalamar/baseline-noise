## ANALYZING MOTOR EXECUTION NOISE (ALIGNED CURSOR AND NO-CURSOR) FOR BASELINE DATA
## PART II
# run these functions in order!


collapsed_analysis <- function() {
  
  library(dplyr)
  library(tidyr)
  
  setwd('~/Desktop/baseline noise/combined/GROUP COMBINES/')
  load_files <- list.files()
  load_files <- load_files[-c(14:15)]
  
  collapsed_df <- NA
    
  for (exp in load_files){
    
    print(exp)
    groupdf <- read.csv(exp, header = TRUE)
    groupdf <- groupdf %>% mutate(exp_group = exp)
    
    if (is.data.frame(groupdf) == TRUE) {
      
      collapsed_df <- rbind(collapsed_df, groupdf)
      
    } else {
      
      collapsed_df <- groupdf
      
    }
    
  }
  
  collapsed_df <- collapsed_df %>%
    drop_na() %>%
    filter(subject != 'ms_0129') #this participant has v few trials
  
  write.csv(collapsed_df, 'collapsed.csv', row.names = FALSE)
  
  tasks <- unique(collapsed_df$task)
  
  aligned_cursor <- collapsed_df %>% 
    filter(task == tasks[1]) %>%
    filter(trial > 46) #remove first 45 familiarization aligned trials
    
  aligned_no_cursor <- collapsed_df %>%
    filter(task == tasks[2])
  
  aligned_cursor.summary <- aligned_cursor %>% 
    group_by(subject) %>%
    summarise(mean_error = mean(pv_angle, na.rm = TRUE), 
              sd_error = sd(pv_angle, na.rm = TRUE))
  
  aligned_no_cursor.summary <- aligned_no_cursor %>% 
    group_by(subject) %>%
    summarise(mean_error = mean(pv_angle, na.rm = TRUE), 
              sd_error = sd(pv_angle, na.rm = TRUE))
  
  t.test(aligned_cursor.summary$sd_error, aligned_no_cursor.summary$sd_error, paired = TRUE)
  plot(aligned_cursor.summary$sd_error, aligned_no_cursor.summary$sd_error, asp = 1)
  
  
  library(ggplot2)
  plotvals = data.frame(aligned_cursor.summary$sd_error, aligned_no_cursor.summary$sd_error)
  ggplot(plotvals, aes(aligned_cursor.summary.sd_error,aligned_no_cursor.summary.sd_error))+
    geom_point() +
    labs(title = "",
         x="aligned cursor variability", y = "aligned no cursor variability")+
    theme_classic()+
    xlim(0,18) + 
    ylim(0,18)
}

collapsed_corr <- function() {
  
  #get exp group names
  setwd('~/Desktop/baseline noise/combined')
  load_files <- list.files()
  load_files <- load_files[which(load_files != 'GROUP COMBINES')]
  
  setwd('GROUP COMBINES/group localizations')
  young_localizations <- read.csv('young_varianceCIs.csv', header = TRUE)
  young_localizations$subject_no <- young_localizations$participant
  aging_localizations <- read.csv('aging_varianceCIs.csv', header = TRUE)
  aging_localizations$subject_no <- aging_localizations$participant
  
  #get IDs
  young_IDs <- read.csv('IDs/participants_sheet_younger.csv', header = TRUE)
  aging_IDs <- read.csv('IDs/participants_sheet_older.csv', header = TRUE)
  
}

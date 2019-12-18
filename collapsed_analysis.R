## ANALYZING MOTOR EXECUTION NOISE (ALIGNED CURSOR AND NO-CURSOR) FOR BASELINE DATA
## PART II
# run these functions in order!


collapsed_analysis <- function() {
  
  library(dplyr)
  library(tidyr)
  
  setwd('~/Desktop/baseline data/baseline noise/combined/GROUP COMBINES/')
  load_files <- list.files()
  
  collapsed_df <- NA
    
  for (exp in load_files){
    
    groupdf <- read.csv(exp, header = TRUE)
    
    if (is.data.frame(groupdf) == TRUE) {
      
      collapsed_df <- rbind(collapsed_df, groupdf)
      
    } else {
      
      collapsed_df <- groupdf
      
    }
    
  }
  
#write.csv(collapsed_df, 'collapsed.csv', row.names = FALSE)
  
  collapsed_df <- collapsed_df %>% drop_na()
  tasks <- unique(collapsed_df$task)
  
  aligned_cursor <- collapsed_df %>% 
    filter(task == tasks[1])
  
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
  
}

# NOTES
# make a function where you combine all exp files and then do the big comparison
# MAYBE ADD EXPERIMENT LABEL ON THE FILE ??
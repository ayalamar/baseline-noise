## ANALYZING MOTOR EXECUTION NOISE (ALIGNED CURSOR AND NO-CURSOR) FOR BASELINE DATA
# run these functions in order!

groups <- list.files("~/Desktop/baseline noise/data")

##################
##################
## COLLECT DATA PER PP
collect_files <- function(group){

library(dplyr)

setwd('~/Desktop/baseline noise/')

folder_name <- sprintf('data/%s', group)
setwd(folder_name)
print(folder_name)

#create path for output
new_path <- sprintf('~/Desktop/baseline noise/combined/%s', group)
dir.create(new_path)

#which participant files to open?
pp <- list.files()

#how many tasks per pp?
tempfilepath <- sprintf('~/Desktop/baseline noise/%s/%s', group, pp[1])
print(tempfilepath)
list.files(tempfilepath) #note - we only want the aligned cursors and no-cursors

#these task tags stay the same for all experiments here
tasks <- c('align_explicit_train_selected', 'align_explicit_no_cursor_selected')

for (ppno in pp) {
  
  pp_id <- ppno
  
  ppdf <- NA
  
  for (taskno in tasks) {
    
    task <- taskno
    
    filename <- sprintf('%s/%s_%s.txt', pp_id, pp_id, task)
    print(filename)
    
    taskdf <- read.table(filename, header = FALSE)
    
    columnnames <- c("trial", "target","Xcursor","Ycursor","Xrobot","Yrobot",
                          "XscreenO","YscreenO","Xhome","Yhome","XTarget","Ytarget",
                          "blocknumber","rotation","time","trialselected","sampleselected",
                          "sampleinterpolated","maxvelocity")
    
    #note - some data were selected when the "unsure" column was yet to be implemented
    if (dim(taskdf)[2] == 19) {
      colnames(taskdf) <- columnnames
    }
    if (dim(taskdf)[2] == 20) {
      colnames(taskdf) <- c(columnnames, 'unsure')
    }
  
    taskdf$participant <- pp_id
    taskdf$task <- task
    
    if (is.data.frame(ppdf) == TRUE) {
      
      ppdf <- rbind(ppdf, taskdf)
      
    } else {
      
      ppdf <- taskdf
      
    }
    
  }
  
  #make a file with all of them in there
  outfile_name <- sprintf('~/Desktop/baseline noise/combined/%s/combined_%s_ALL.csv', group, pp_id)
  print(outfile_name)
  write.csv(ppdf, file = outfile_name, row.names = FALSE)  
  
}

}

##################
##################
## PREPROCESS DATA
# After having been combined, this function subsets the variable(s) of interest and 
# creates an output w/ a single sample per trial per participant
preprocess_data <- function(group) {
  
  library(dplyr)
  
  ##get pp IDs again
  folder_name <- sprintf('~/Desktop/baseline noise/data/%s', group)
  print(folder_name)
  
  #setwd('~/Desktop/baseline noise/data')
  
  pp <- list.files(folder_name)
  pp <- pp[which(pp != "combined")]
  print(pp)
  ##go to previously combined data ^
  folder_path <- sprintf('~/Desktop/baseline noise/combined/%s', group)
  setwd(folder_path)
  
  for (ppno in pp) {
    
    pp_id <- ppno
    
    filename <- sprintf('combined_%s_ALL.csv', pp_id)
    print(filename)
    
    taskdf <- read.csv(filename, header = TRUE)
    
    ## For collecting ANGLE AT MAX VELOCITY & SELECTED ONLY
    pvsamples <- taskdf %>%
      filter(maxvelocity == 1) %>%
      filter(trialselected == 1)
    
    ## For collecting ENDPOINT ANGLE & SELECTED ONLY
    epsamples <- taskdf %>%
      group_by(trial) %>%
      filter(time == max(time)) %>%
      filter(trialselected == 1)

    Yhome <- abs(unique(taskdf$Yhome))
    
    if (Yhome != 0) {
      
      # calculate new target Y because (0,0) is not the origin/home, then store it
      pvsamples$new_targety <- pvsamples$Ytarget + Yhome
      pvsamples$new_target_angle <- (atan2(pvsamples$new_targety, pvsamples$XTarget))/(pi/180)
      
      epsamples$new_targety <- epsamples$Ytarget + Yhome
      epsamples$new_target_angle <- (atan2(epsamples$new_targety, epsamples$XTarget))/(pi/180)
      
      # calculate new cursor Y because (0,0) is not the origin/home, then store it
      pvsamples$relative_cursory <- pvsamples$Ycursor + Yhome
      pvsamples$pv_angle_OG <- (atan2(pvsamples$relative_cursory, pvsamples$Xcursor))/(pi/180)
      
      epsamples$relative_cursory <- epsamples$Ycursor + Yhome
      epsamples$ep_angle_OG <- (atan2(epsamples$relative_cursory, epsamples$Xcursor))/(pi/180)
      
      # calculate angle at peak velocity relative to target location, then store it
      pvsamples$pv_angle <- pvsamples$target - pvsamples$pv_angle_OG # positive angles to the right of target; negative to left
      
      epsamples$ep_angle <- epsamples$target - epsamples$ep_angle_OG 
      
    } else {
      
      # no need to adjust Y, just calculate angle at peak velocity relative to target location & store it
      pvsamples$pv_angle_OG <- (atan2(pvsamples$Ycursor, pvsamples$Xcursor))/(pi/180)
      pvsamples$pv_angle <- pvsamples$target - pvsamples$pv_angle_OG
      
      epsamples$ep_angle_OG <- (atan2(epsamples$Ycursor, epsamples$Xcursor))/(pi/180)
      epsamples$ep_angle <- epsamples$target - epsamples$ep_angle_OG
      
    }
    
    #tag outliers per participant
    
    # ## pv angle outliers
    # boxplot(pv_angle~task, ylab='angle at peak velocity', xlab='rotation', data = pvsamples)
    # outlier_values_pv <- boxplot.stats(pvsamples$pv_angle)$out
    # pvsamples$isoutlier <- FALSE
    # pvsamples$isoutlier[which(pvsamples$pv_angle %in% outlier_values_pv)] <- TRUE
    # pvsamples$pv_angle[which(pvsamples$isoutlier == TRUE)] <- NA
    # 
    # ## ep angle outliers
    # boxplot(ep_angle~task, ylab='angle at endpoint', xlab='rotation', data = epsamples)
    # outlier_values_ep <- boxplot.stats(epsamples$ep_angle)$out
    # epsamples$isoutlier <- FALSE
    # epsamples$isoutlier[which(epsamples$ep_angle %in% outlier_values_ep)] <- TRUE
    # epsamples$ep_angle[which(epsamples$isoutlier == TRUE)] <- NA
    # 
    #add ID for later merge
    pvsamples <- pvsamples %>% 
      mutate(subject = ppno) %>%
      left_join(select(epsamples, ep_angle, trial), by = "trial") %>%
      select(subject, task, blocknumber, trial, pv_angle, ep_angle )
      #select(-c(isoutlier, pv_angle_OG))
    
    #output trial data
    outfile_name <- sprintf('trialdata_%s.csv', pp_id)
    write.csv(pvsamples, file = outfile_name, row.names = FALSE)
    
  }
  
}

##################
##################
## COMBINE ALL PARTICIPANTS TOGETHER PER EXP
analyze_data <- function(group){
  
  ##get pp IDs again
  folder_name <- sprintf('~/Desktop/baseline noise/data/%s', group)
  print(folder_name)
  pp <- list.files(folder_name)
  pp <- pp[which(pp != "combined")]
  
  folder_name <- sprintf('~/Desktop/baseline noise/combined/%s', group)
  setwd(folder_name)
  groupdf <- NA
  
  for (ppno in pp) {
    
    ppid <- ppno
    
    filename <- sprintf('trialdata_%s.csv', ppid)
    print(filename)
    
    ppdf <- read.csv(filename, header = TRUE)
    
    if (is.data.frame(ppdf) == TRUE) {
      
      groupdf <- rbind(groupdf, ppdf)
      
    } else {
      
      groupdf <- ppdf
      
    }
    
  }
  
  groupdf <- groupdf[-c(1),] #note : this has a first row of NAs
  
  # #save a copy
  #outfile_name <- sprintf('allTaggedData_%s_n%s.csv', group, length(pp))
  outfile_name <- sprintf('/Users/mayala/Desktop/baseline noise/combined/GROUP COMBINES/allTaggedData_%s_n%s.csv', group, length(pp))
  write.csv(groupdf, file = outfile_name, row.names = FALSE) 

  ## ANALYZE FOR EACH EXP
  library(dplyr)
  library(tidyr)
  
  tasks <- unique(groupdf$task)
  
  aligned_cursor <- groupdf %>% 
    filter(task == tasks[1])
  
  aligned_no_cursor <- groupdf %>%
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

##################
##################
## COMBINE ALL EXPS INTO ONE GIANT FILE FOR DOING STATS ON
collapsed_analysis <- function() {
  
  # combine all trials from all participants and groups
  library(dplyr)
  library(tidyr)
  
  setwd('~/Desktop/baseline noise/combined/GROUP COMBINES/')
  load_files <- (list.files())[-c(14:15)]
  
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
  
  collapsed_df <- collapsed_df[-c(1),] # remove empty first row
  
  collapsed_df <- collapsed_df %>%
    filter(subject != 'ms_0129') #this participant has v few trials
  
  write.csv(collapsed_df, 'collapsed.csv', row.names = FALSE)
  
  ## quick look
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




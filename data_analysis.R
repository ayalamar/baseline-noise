## ANALYZING MOTOR EXECUTION NOISE (ALIGNED CURSOR AND NO-CURSOR) FOR BASELINE DATA
## PART I
# run these functions in order!

##################
##################
## COLLECT DATA 
collect_files <- function(group){

setwd('~/Desktop/baseline data/baseline noise/')

folder_name <- sprintf('data/%s', group)
setwd(folder_name)
print(folder_name)

#create path for output
new_path <- sprintf('~/Desktop/baseline data/baseline noise/combined/%s', group)
dir.create(new_path)

#which participant files to open?
pp <- list.files()

#how many tasks per pp?
tempfilepath <- sprintf('~/Desktop/baseline data/baseline noise/%s/%s', group, pp[1])
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
  outfile_name <- sprintf('~/Desktop/baseline data/baseline noise/combined/%s/combined_%s_ALL.csv', group, pp_id)
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
  
  ##get pp IDs again
  folder_name <- sprintf('~/Desktop/baseline data/baseline noise/data/%s', group)
  print(folder_name)
  
  setwd('~/Desktop/baseline data/baseline noise/data')
  
  pp <- list.files(folder_name)
  pp <- pp[which(pp != "combined")]
  
  ##go to previously combined data ^
  folder_path <- sprintf('combined/%s', group )
  setwd(folder_path)
  
  for (ppno in pp) {
    
    pp_id <- ppno
    
    filename <- sprintf('combined_%s_ALL.csv', pp_id)
    print(filename)
    
    taskdf <- read.csv(filename, header = TRUE)
    
    ## ANGLE AT MAX VELOCITY ONLY
    # create subset where you only select samples that occur at peak velocity
    pvsamples <- taskdf[ which(taskdf$maxvelocity == 1), ]
    # and only selected data
    pvsamples <- pvsamples[ which(pvsamples$trialselected == 1), ]

    Yhome <- abs(unique(taskdf$Yhome))
    
    if (Yhome != 0) {
      
      # calculate new target Y because (0,0) is not the origin/home, then store it
      pvsamples$new_targety <- pvsamples$Ytarget + Yhome
      pvsamples$new_target_angle <- (atan2(pvsamples$new_targety, pvsamples$XTarget))/(pi/180)
      
      # calculate new cursor Y because (0,0) is not the origin/home, then store it
      pvsamples$relative_cursory <- pvsamples$Ycursor + Yhome
      pvsamples$pv_angle_OG <- (atan2(pvsamples$relative_cursory, pvsamples$Xcursor))/(pi/180)
      
      # calculate angle at peak velocity relative to target location, then store it
      pvsamples$pv_angle <- pvsamples$target - pvsamples$pv_angle_OG # positive angles to the right of target; negative to left
      
    } else {
      
      # no need to adjust Y, just calculate angle at peak velocity relative to target location & store it
      pvsamples$pv_angle_OG <- (atan2(pvsamples$Ycursor, pvsamples$Xcursor))/(pi/180)
      pvsamples$pv_angle <- pvsamples$target - pvsamples$pv_angle_OG # positive angles to the right of target; negative to left
      
    }
    
    #tag outliers per participant
    boxplot(pv_angle~task, ylab='angle at peak velocity', xlab='rotation', data = pvsamples)
    outlier_values <- boxplot.stats(pvsamples$pv_angle)$out
    pvsamples$isoutlier <- FALSE
    pvsamples$isoutlier[which(pvsamples$pv_angle %in% outlier_values)] <- TRUE
    pvsamples$pv_angle[which(pvsamples$selection_1 != 1)] <- NA
    
    #select only non-outliers
    pvsamples <- pvsamples[ which(pvsamples$isoutlier == FALSE), ]
    
    #add ID for later merge
    pvsamples$subject <- ppno
    
    #output trial data
    outfile_name <- sprintf('trialdata_%s.csv', pp_id)
    write.csv(pvsamples, file = outfile_name, row.names = FALSE)
    
  }
  
}

##################
##################
## COMBINE ALL PARTICIPANTS TOGETHER AND ANALYZE 
analyze_data <- function(group){
  
  ##get pp IDs again
  folder_name <- sprintf('~/Desktop/baseline data/baseline noise/%s', group)
  print(folder_name)
  pp <- list.files(folder_name)
  pp <- pp[which(pp != "combined")]
  
  folder_name <- sprintf('~/Desktop/baseline data/baseline noise/combined/%s', group)
  setwd(folder_name)
  groupdf <- NA
  
  for (ppno in pp) {
    
    ppid <- ppno
    
    filename <- sprintf('trialdata_%s.csv', ppid)
    print(filename)
    
    ppdf <- read.csv(filename, header = TRUE)
    
    #getting rid of extra col for now
    if (dim(ppdf)[2] == 29) {
      
      ppdf <- ppdf[,-20]
      
    }
    
    if (is.data.frame(ppdf) == TRUE) {
      
      groupdf <- rbind(groupdf, ppdf)
      
    } else {
      
      groupdf <- ppdf
      
    }
    
  }
  
  groupdf <- groupdf[-c(1),] #note : this has a first row of NAs
  
  # #save a copy
  outfile_name <- sprintf('allTaggedData_%s_n%s.csv', group, length(pp))
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





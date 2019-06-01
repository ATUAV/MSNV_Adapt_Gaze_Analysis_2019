
# Gnerate trigger (intervention) seg files for AdaptMSNV study 
#
# Author: Dereck Toker and Kristy Siu
# Last Update: May 13, 2019

library(data.table)
library(stringr)
library(bit64)

setwd("/Users/kristys/Documents/MSNV_Adapt_Gaze_Analysis_2019/Eye_Tracking_Processing")

users <- fread("study2_user_list.csv")
msnv_ids <- c(27, 60, 11, 30, 62, 72, 28, 74,  5, 20, 76, 66,  9,  3)

triggers_ignored <- c("5_ref_109_rule", "5_ref_110_rule", "5_ref_106_rule", "30_ref_102_rule", "62_ref_102_rule")

setwd("/Users/kristys/Documents/MSNV_Adapt_Gaze_Analysis_2019")

for (user in users$user_id) {
  trigger_segs <- data.table()
  
  segs <- fread(paste("Eye_Tracking_Processing/segs/", user, ".seg", sep=""), sep="\t") 

  gaze_export <- fread(paste("Tobii Export/AdaptiveMSNV_Bar_Study_New test_Rec ", user, ".tsv", sep=""), sep="\t") 
  first_eyetracker_time <- gaze_export[1]$EyeTrackerTimestamp
  first_recording_time <- gaze_export[1]$RecordingTimestamp
  
  for (msnv in msnv_ids) {
    my_data <- data.table(read.delim(paste0("Platform Logs/AdaptiveMSNV_log_user_", user, "_task_", msnv, ".sql")))
    names(my_data) <- "data"
    my_data$data <- as.character(my_data$data)
    my_data <- my_data[grepl("rule_state V", data),]
    my_data$data <- str_extract(my_data$data, "\\(.*\\)")
    my_data$data <- gsub("\\(|\\)", "", my_data$data)
    my_data <- my_data[!duplicated(my_data$data)]
    
    for (rule_state in my_data$data) {
      rule_state <- unlist(strsplit(rule_state, split=",")) #rule_state is now a chr vector 
      start_time <- rule_state[2]
      start_time <- round((as.numeric(start_time)-first_eyetracker_time)/1000) + first_recording_time
      rule <- gsub("'", "", rule_state[1])
      
      # check if the trigger should be removed 
      if (rule %in% triggers_ignored) {
        print(paste0("Ignored ", rule))
        next
      }
      
      task <- unlist(strsplit(rule, split="_"))[1]
      seg_start <- segs[V1==task]$V3
      seg_end <- segs[V1==task]$V4
      
      # check if trigger start_time is between seg start and end 
      if ((start_time < seg_start) | (start_time > seg_end)) {
        print(paste0("incorrect start or end time of user ", user, " 's segment ", msnv))
        print(paste0("rule:", rule))
        print(paste0("trigger time:", start_time))
      }
      
      temp <- data.table(scene = rule, seg = rule, start = start_time, end = seg_end)
      trigger_segs <- rbind(trigger_segs, temp)
    }
  }
  
  write_tsv(trigger_segs, paste("Eye_Tracking_Processing/trigger_segs/", user,"_trigger.seg", sep = ""), col_names = FALSE)
} 

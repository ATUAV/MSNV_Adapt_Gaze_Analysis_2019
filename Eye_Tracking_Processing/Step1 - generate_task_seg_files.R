
# Generate seg files in study
#
# Author: Dereck Toker and Kristy Siu
# Last Update: May 9, 2019

library(data.table)
library(readr)

setwd("/Users/kristys/Documents/MSNV_Adapt_Gaze_Analysis_2019/Eye_Tracking_Processing")

mmd_order <- fread("User_data.csv")
mmd_order <- subset(mmd_order, sel = -c(date_created))

#Master user_list
users <- fread("study2_user_list.csv")

mmd_performance <- fread("MMD_performance.csv")

#for each user in users, grab the MMD order
loop_table <- merge(users, mmd_order)

length <- nrow(loop_table)

for (i in 1:length) {
  
  a_user <- as.character(loop_table[i,1])
  an_order <- loop_table[i,2]    # mmd_order 
  an_order <- substring(an_order, 2, nchar(an_order)-1)
  an_order <- as.list(strsplit(an_order, ", "))[[1]]
  
  #need to read in that user's gaze data file here:
  setwd("/Users/kristys/Documents/Tobii Export")
  
  gaze_export <- fread(paste("AdaptiveMSNV_Bar_Study_New test_Rec ", a_user, ".tsv", sep=""), sep="\t") 
  gaze_export <- subset(gaze_export, sel = c(RecordingTimestamp, LocalTimeStamp, MouseEvent))
  
  #convert to integer time
  gaze_export$IntTime <- apply(gaze_export, 1, function(x) as.double(strptime(x[2], "%H:%M:%OS")))
  #convert to time in Tobii timeline 
  gaze_export$TobiiTimeline <- apply(gaze_export, 1, function(x) format(as.POSIXct(as.numeric(x[1]) / 1000, origin = "1970-01-01", tz = "America/Los_Angeles"), "%H:%M:%OS3"))
  
  seg_frame <- data.table()
  
  for (j in 1:length(an_order)) {
  
    mmd_item <- an_order[j]
    
    performance_mmd <- subset(mmd_performance, user_id == a_user & mmd_id == mmd_item & screen_id == "mmd")
    
    mmd_start <- as.double(strptime(performance_mmd[1, task_start], "%H:%M:%OS"))
    mmd_end   <- as.double(strptime(performance_mmd[1,   task_end], "%H:%M:%OS"))
    
    range_start <- subset(gaze_export, IntTime > (mmd_start - 1) & IntTime < ( mmd_start + 1))
    range_start$rid <- rownames(range_start)
    id <- subset(range_start, MouseEvent == "Left")
    if (nrow(id) > 1) {
      print("START")
      print(j)
      print(id)
      input <- readline(prompt="Enter an integer: ") # pick one row with correct "Left" time in id
      id <- id[as.integer(input),]
    }
    
    start_stamp <- as.integer(range_start[as.integer(id[1,rid])+1,RecordingTimestamp]) #the one after "Left" is start
    if (nrow(id) == 0) {
      print("START is NA")
      print(j)
      input <- readline(prompt="Enter recTimestamp: ")
      start_stamp <- as.integer(input)
    }
    
    range_end <- subset(gaze_export, IntTime > (mmd_end - 1) & IntTime < ( mmd_end + 1))
    range_end$rid <- rownames(range_end)
    
    id <- subset(range_end, MouseEvent == "Left")
    if (nrow(id) > 1) {
      print("END")
      print(j)
      print(id)
      input <- readline(prompt="Enter an integer: ")
      id <- id[as.integer(input),]
    }
    end_stamp <- as.integer(range_end[as.integer(id[1,rid])-1,RecordingTimestamp])
    if (nrow(id) == 0) {
      print("END is NA")
      print(j)
      input <- readline(prompt="Enter recTimestamp: ")
      end_stamp <- as.integer(input)
    }
    
    temp <- data.table(scene = mmd_item, seg = mmd_item, start = start_stamp, end = end_stamp)
    
    seg_frame <- rbind(seg_frame, temp)
  }
  
  setwd("/Users/kristys/Documents/MSNV_Adapt_Gaze_Analysis_2019/Eye_Tracking_Processing/segs")
  write_tsv(seg_frame, paste(a_user,".seg", sep = ""), col_names = FALSE)
}



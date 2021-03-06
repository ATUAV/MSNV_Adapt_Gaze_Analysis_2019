
# Takes static fine-grained aoi files and makes them dynamic, with time invervals
# taken from trigger (intervention) seg files. Generated files can only be used 
# by the subtractive and dynamic boundaries EMDAT version. 

library(data.table)

setwd("/Users/kristys/Documents/MSNV_Adapt_Gaze_Analysis_2019/Eye_Tracking_Processing")
#users <- fread("study2_user_list.csv")
users <- fread("study1_user_list.csv")
msnv_ids <- as.character(c(27, 60, 11, 30, 62, 72, 28, 74, 5, 20, 76, 66, 9, 3))

for (user in users$user_id) {
  #segs <- fread(paste0("segs/", user, ".seg"), sep="\t") 
  segs <- fread(paste0("Control_segs/", user, ".seg"), sep="\t") 
  
  #fileConn <- file(paste0('finegrained_aois_per_user/dynamic_' , user, ".aoi"))
  fileConn <- file(paste0('control_finegrained_aois_per_user/dynamic_' , user, ".aoi"))
  
  dyn_aois <- c()
  for (msnv in msnv_ids) {
    finegrained <- fread(paste0('finegrained_aois_adjusted/', msnv,".aoi"), sep="\n", header = FALSE)
    for (i in 1:nrow(finegrained)) {
      seg_start <- segs[V1==msnv]$V3
      seg_end <- segs[V1==msnv]$V4
      aoi <- paste0(finegrained[i], '\n#\t', seg_start, ',', seg_end)
      dyn_aois <- c(dyn_aois, aoi)
    }
    labels <- fread(paste0('labels_aois/', msnv,".aoi"), sep="\n", header = FALSE)
    for (i in 1:nrow(labels)) {
      seg_start <- segs[V1==msnv]$V3
      seg_end <- segs[V1==msnv]$V4
      aoi <- paste0(labels[i], '\n#\t', seg_start, ',', seg_end)
      dyn_aois <- c(dyn_aois, aoi)
    }
  }
  
  writeLines(paste(dyn_aois, collapse="\n"), sep="", fileConn)
  close(fileConn)
}



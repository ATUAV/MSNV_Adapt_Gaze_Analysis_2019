# Generate dynamic aoi files from static aoi files and trigger seg files 
#
# Author: Kristy Siu
# Last Update: May 2019

library(data.table)
library(stringr)

setwd("/Users/kristys/Documents/Eye_Tracking_Processing")

users <- fread("study2_user_list.csv")
msnv_ids <- c(27, 60, 11, 30, 62, 72, 28, 74,  5, 20, 76, 66,  9,  3)

for (msnv in msnv_ids) {

  for (user in users$user_id) {
  
    trigger_segs <- fread(paste("trigger_segs/", user, "_trigger.seg", sep=""), sep="\t") 
  
    segs <- c()
  
    triggers <- subset(trigger_segs, grepl(paste0(msnv, "_ref_"), V1))
    
    aois <- fread(paste("static_aois_adjusted/", msnv, "_intervention.aoi", sep=""), sep="\n", header=FALSE)
    
    for (k in 1:nrow(aois)) {
      rule <- unlist(strsplit(as.character(aois[k,]), split="\t"))[1]
      trigger <- triggers[V1==rule]
      if (nrow(trigger) != 1) {
        next
      }
      segs <- c(segs, aois[k,])
      segs <- c(segs, paste("#\t",trigger[1,V3],",",trigger[1,V4], sep=""))
    }
    
    #add overall aoi
    overall_aoi <- paste0(user, '_', msnv, '_overall')
    if (length(segs)==0) {
      next
    }
    for (i in seq(1, length(segs), by=2)) {
      aoi <- str_split(as.character(segs[i]), '\t', n=2)[[1]][2]
      overall_aoi <- paste(overall_aoi, aoi, sep='\t')
    }
    trigger_segs <- trigger_segs[order(V3),]
    overall_aoi <- paste0(overall_aoi, "\n#\t", trigger_segs[1]$V3, ",", trigger_segs[nrow(trigger_segs)]$V4)
    segs <- c(segs, overall_aoi)
    
    # for 1 file per user
    # cat(paste(segs, collapse="\n"), file=paste("dynamic_aois/", 'dynamic_' , user, ".aoi", sep=""), sep="\n", append=TRUE) 
    # for 1 file per user and per msnv
    cat(paste(segs, collapse="\n"), file=paste("dynamic_aois_per_user_msnv/", 'dynamic_' , user, "_", msnv, ".aoi", sep=""), sep="", append=FALSE)
  }
}



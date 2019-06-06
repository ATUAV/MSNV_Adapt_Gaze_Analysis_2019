# Generate dynamic aoi files from static aoi files and trigger seg files 
#
# Author: Kristy Siu
# 
# In each .aoi file generated for a msnv, all lines except the last correspond to 
# a bar for a triggered rule that can trigger more than 1 bar, with the aoi name set 
# to corresponding rule name appended with a 0-based index. Each bar is defined with
# 5 points, although the 5th point is the same the 1st point and is not actually
# needed. The last line has the concatenation of all the above AOIs' coordinates. 
# and does not actually define an AOI.


library(data.table)
library(stringr)

setwd("/Users/kristys/Documents/MSNV_Adapt_Gaze_Analysis_2019/Eye_Tracking_Processing")

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
      split <- str_split(as.character(aois[k,]), '\t')[[1]]
      
      for (i in 0:((length(split)-1)/5 -1)) {
        seg <- c(paste0(split[1], '_', i))
        seg <- c(seg, split[(i*5+2):(i*5+5)])
        seg <- paste(seg, collapse = '\t')
        segs <- c(segs, seg)
        segs <- c(segs, paste("#\t",trigger[1,V3],",",trigger[1,V4], sep=""))
      }
    }
    
    #create overall aoi string
    overall_aoi <- ''
    if (length(segs)==0) {
      next
    }
    for (i in seq(1, length(segs), by=2)) {
      aoi <- str_split(as.character(segs[i]), '\t', n=2)[[1]][2]
      overall_aoi <- paste(overall_aoi, aoi, sep='\t')
    }
    
    # remove duplicate bars from overall
    aois_combined <- str_split(as.character(overall_aoi), '\t', n=2)[[1]][2]
    dat <- unlist(strsplit(aois_combined, "\t"))
    no_dup <- c()
    for (i in 0:(length(dat)/5 - 1)) {
      no_dup <- c(no_dup, paste(dat[(i*5+1):(i*5+5)], collapse = '\t'))
    }
    no_dup <- no_dup[!duplicated(no_dup)]
    no_dup <- paste0(no_dup, collapse = '\t')
    overall_aoi <- paste0(user, '_', msnv, '_overall\t', no_dup)
    
    
    trigger_segs <- trigger_segs[order(V3),]
    overall_aoi <- paste0(overall_aoi, "\n#\t", trigger_segs[1]$V3, ",", trigger_segs[nrow(trigger_segs)]$V4)
    segs <- c(segs, overall_aoi)
    
    # for 1 file per user, need to delete existing files first 
    #cat(paste(segs, collapse="\n"), file=paste("dynamic_aois/", 'dynamic_' , user, ".aoi", sep=""), sep="\n", append=TRUE) 
    # for 1 file per user and per msnv
    cat(paste(segs, collapse="\n"), file=paste("dynamic_aois_per_user_msnv/", 'dynamic_' , user, "_", msnv, ".aoi", sep=""), sep="", append=FALSE)
  }
}



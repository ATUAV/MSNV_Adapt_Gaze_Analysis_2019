# Generate dynamic aoi files from static aoi files and trigger seg files 
#
# Author: Kristy Siu
# 
# In each .aoi file generated for a msnv and a user, each AOI that is not named 
# 'Highlighted bars' correspond to a bar for a triggered rule that can highlight more than 1 bar,
# where the aoi name is set to corresponding rule name. AOIs named the same would be combined in
# the subtractive and dynamic boundaries EMDAT version. The AOI 'Highlighted bars' combines all 
# highlighted bars. 

library(data.table)
library(stringr)

setwd("/Users/kristys/Documents/MSNV_Adapt_Gaze_Analysis_2019/Eye_Tracking_Processing")

users <- fread("study2_user_list.csv")
msnv_ids <- c(27, 60, 11, 30, 62, 72, 28, 74,  5, 20, 76, 66,  9,  3)

for (msnv in msnv_ids) {

  for (user in users$user_id) {
  
    trigger_segs <- fread(paste("trigger_segs/", user, "_trigger.seg", sep=""), sep="\t") 
  
    bars <- c()
  
    triggers <- subset(trigger_segs, grepl(paste0(msnv, "_ref_"), V1))
    
    aois <- fread(paste("static_aois_adjusted/", msnv, "_intervention.aoi", sep=""), sep="\n", header=FALSE)
    
    for (k in 1:nrow(aois)) {
      rule <- unlist(strsplit(as.character(aois[k,]), split="\t"))[1]
      trigger <- triggers[V1==rule]
      
      split <- str_split(as.character(aois[k,]), '\t')[[1]]
      
      for (i in 0:((length(split)-1)/4 -1)) {
        bar <- c(split[1], split[(i*4+2):(i*4+5)])
        bar <- paste(bar, collapse = '\t')
        bars <- c(bars, bar)
        if (nrow(trigger) != 1) {
          bars <- c(bars, paste("#\t", -1, ",", -1, sep=""))
          break
        }
        bars <- c(bars, paste("#\t",trigger[1,V3],",",trigger[1,V4], sep=""))
        
        bars_combined <- c('Highlighted bars', split[(i*4+2):(i*4+5)])
        bars_combined <- paste(bars_combined, collapse = '\t')
        bars <- c(bars, bars_combined)
        bars <- c(bars, paste("#\t",trigger[1,V3],",",trigger[1,V4], sep=""))
      }
    }
    
    ### write to files 
    
    # for 1 file per user, need to delete existing files first 
    #cat(paste(segs, collapse="\n"), file=paste("intervention_aois/", 'dynamic_' , user, ".aoi", sep=""), sep="\n", append=TRUE) 
    # for 1 file per user and per msnv
    cat(paste(bars, collapse="\n"), file=paste("intervention_aois_per_user_msnv/", 'dynamic_' , user, "_", msnv, ".aoi", sep=""), sep="", append=FALSE)
  }
}



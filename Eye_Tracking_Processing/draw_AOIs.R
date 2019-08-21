  
library(magick)
library(stringr)

setwd("/Users/kristys/Documents/MSNV_Adapt_Gaze_Analysis_2019")

#mmd_list <- as.character(c(3,5,9,11,20,27,28,30,60,62,66,72,74,76))
#mmd_list <- as.character(c(20, 60, 62, 74, 76))
mmd_list <- as.character(c(62))

for (msnv in mmd_list) {
  
  #read in image
  an_image <- image_read(paste("../msnv_screenshots/", msnv,".png",sep=""))
  im <- image_draw(an_image)
  
  #read in AOI coordinates
  my_aois <- fread(paste("Eye_Tracking_Processing/relevant_aois_adjusted/", msnv,"_Relevant.aoi",sep=""), header = FALSE, sep = "\n")

  if (nrow(my_aois) <= 1)
    next
 
  for (i in 1:(nrow(my_aois)-1)) {
    bars <- str_split(as.character(my_aois[i,]), "\t")[[1]][-1]
    
    bars <- bars[!sapply(bars, function(x) x == "")]
    
    top <- as.integer(str_split(bars[1], ",")[[1]][[2]])
    left <- as.integer(str_split(bars[1], ",")[[1]][[1]])
    width <- as.integer(str_split(bars[2], ",")[[1]][[1]]) - as.integer(str_split(bars[1], ",")[[1]][[1]])
    height <- as.integer(str_split(bars[3], ",")[[1]][[2]]) - as.integer(str_split(bars[2], ",")[[1]][[2]])
      
    rect(left, top + height, left + width, top, col = rgb(1, 0.2, 0.2, alpha = 0.2))
  }
  
  my_aois <- fread(paste("Eye_Tracking_Processing/labels_AOIs/", msnv,".aoi",sep=""), header = FALSE, sep = "\n")
  
  for (i in 1:nrow(my_aois)) {
    if (str_split(as.character(my_aois[i,]), "\t")[[1]][1] != 'labels')
      next
    bars <- str_split(as.character(my_aois[i,]), "\t")[[1]][-1]
    
    bars <- bars[!sapply(bars, function(x) x == "")]
    
    top <- as.integer(str_split(bars[1], ",")[[1]][[2]])
    left <- as.integer(str_split(bars[1], ",")[[1]][[1]])
    width <- as.integer(str_split(bars[2], ",")[[1]][[1]]) - as.integer(str_split(bars[1], ",")[[1]][[1]])
    height <- as.integer(str_split(bars[3], ",")[[1]][[2]]) - as.integer(str_split(bars[2], ",")[[1]][[2]])
    
    rect(left, top + height, left + width, top, col = rgb(1, 0.2, 0.2, alpha = 0.2))
  }
  
  my_aois <- fread(paste("Eye_Tracking_Processing/finegrained_aois_adjusted/", msnv,".aoi",sep=""), header = FALSE, sep = "\n")
  
  for (i in 1:nrow(my_aois)) {
    if (str_split(as.character(my_aois[i,]), "\t")[[1]][1] != 'legend')
      next
    bars <- str_split(as.character(my_aois[i,]), "\t")[[1]][-1]
    
    bars <- bars[!sapply(bars, function(x) x == "")]
    
    top <- as.integer(str_split(bars[1], ",")[[1]][[2]])
    left <- as.integer(str_split(bars[1], ",")[[1]][[1]])
    width <- as.integer(str_split(bars[2], ",")[[1]][[1]]) - as.integer(str_split(bars[1], ",")[[1]][[1]])
    height <- as.integer(str_split(bars[3], ",")[[1]][[2]]) - as.integer(str_split(bars[2], ",")[[1]][[2]])
    
    rect(left, top + height, left + width, top, col = rgb(1, 0.2, 0.2, alpha = 0.2))
  }
 
  my_aois <- fread(paste("Eye_Tracking_Processing/non_relevant_aois_adjusted/", msnv,"_NR.aoi",sep=""), header = FALSE, sep = "\n")
  
  if (nrow(my_aois) <= 1) {
    image_write(im, path = paste0("../AOI drawing/", msnv,"_bars_label_legend.png"), format = "png")
    next
  }
    
  for (i in 1:(nrow(my_aois)-1)) {
    #label <- str_split(as.character(my_aois[i,]), "\t")[[1]][1]
    bars <- str_split(as.character(my_aois[i,]), "\t")[[1]][-1]
    
    bars <- bars[!sapply(bars, function(x) x == "")]
    
    top <- as.integer(str_split(bars[1], ",")[[1]][[2]])
    left <- as.integer(str_split(bars[1], ",")[[1]][[1]])
    width <- as.integer(str_split(bars[2], ",")[[1]][[1]]) - as.integer(str_split(bars[1], ",")[[1]][[1]])
    height <- as.integer(str_split(bars[3], ",")[[1]][[2]]) - as.integer(str_split(bars[2], ",")[[1]][[2]])
    
    rect(left, top + height, left + width, top, col = rgb(1, 0.2, 0.2, alpha = 0.2))
    #text((left + left + width)/2 +9, (top + height + top)/2, labels = label)
  }
  
  image_write(im, path = paste0("../AOI drawing/", msnv,"_bars_label_legend.png"), format = "png")
  
}

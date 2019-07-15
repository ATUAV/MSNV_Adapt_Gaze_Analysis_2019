library(data.table)
library(ggplot2)


setwd("/Users/kristys/Documents/MSNV_Adapt_Gaze_Analysis_2019")
txt_viz_ctrl_matrix <- fread('control_txt_viz_features_rearranged.tsv', sep = '\t')
txt_viz_adpt_matrix <- fread('adaptive_txt_viz_features_rearranged.tsv', sep = '\t')

fg_ctrl_matrix <- fread('control_FG_features_rearranged.tsv', sep = '\t')
fg_adpt_matrix <- fread('adaptive_FG_features_rearranged.tsv', sep = '\t')

fg_ctrl_matrix <- fg_ctrl_matrix[aoi_name=='legend' | aoi_name=='labels']
fg_adpt_matrix <- fg_adpt_matrix[aoi_name=='legend' | aoi_name=='labels']

msnv_ids <- as.character(c(27, 60, 11, 30, 62, 72, 28, 74, 5, 20, 76, 66, 9, 3))

ctrl_matrix <- rbind(txt_viz_ctrl_matrix[, c("part_id", 'msnv', 'aoi_name', 'totaltimespent', 'proportiontime', 'timetofirstfixation', 'group')], 
                    fg_ctrl_matrix[, c("part_id", 'msnv', 'aoi_name', 'totaltimespent', 'proportiontime', 'timetofirstfixation', 'group')])

adpt_matrix <- rbind(txt_viz_adpt_matrix[, c("part_id", 'msnv', 'aoi_name', 'totaltimespent', 'proportiontime', 'timetofirstfixation', 'group')], 
                    fg_adpt_matrix[, c("part_id", 'msnv', 'aoi_name', 'totaltimespent', 'proportiontime', 'timetofirstfixation', 'group')])


important_features <- c('totaltimespent', 'proportiontime', 'timetofirstfixation')
graphs <- list()

for(f in important_features) {
  #pdf(paste0("control ", f, ".pdf"))
  p <- with(adpt_matrix[!(aoi_name=='Text_all'|aoi_name=='Viz_all'|aoi_name=='Refs')], 
            boxplot(get(f)~aoi_name, outline=FALSE, main=paste0("adaptive: ", f)))
  text(y = p$stats[3,1]+0.01, labels = p$stats[3,1], x = 1.5, cex=0.5, col="blue")
  text(y = p$stats[3,2]+0.01, labels = p$stats[3,2], x = 2.5, cex=0.5, col="blue")
  text(y = p$stats[3,3]+0.01, labels = p$stats[3,3], x = 3.5, cex=0.5, col="blue")
  text(y = p$stats[3,4]+0.01, labels = p$stats[3,4], x = 4.5, cex=0.5, col="blue")
  text(y = p$stats[3,5]+0.01, labels = p$stats[3,5], x = 5.5, cex=0.5, col="blue")
  text(y = p$stats[3,6]+0.01, labels = p$stats[3,6], x = 6.5, cex=0.5, col="blue")
  #dev.off() 
}

combined_matrix <- rbind(ctrl_matrix, adpt_matrix)
combined_matrix[, group := factor(group, levels = c('control', 'adaptive'))]

for(f in important_features) {
  
  p <- ggplot(combined_matrix, aes_string(y=f, x='aoi_name', fill='group')) +
    geom_boxplot(outlier.shape=NA) +
    coord_cartesian(ylim = range(boxplot(combined_matrix[, ..f], plot=FALSE)$stats)*c(.9, 2.3))
  graphs[[f]] <- p
}

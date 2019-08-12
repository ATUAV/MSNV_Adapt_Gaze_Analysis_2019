library(data.table)
library(ggplot2)


setwd("/Users/kristys/Documents/MSNV_Adapt_Gaze_Analysis_2019")
ref_viz_ctrl_matrix <- fread('control_ref_viz_features_rearranged.tsv', sep = '\t')
ref_viz_adpt_matrix <- fread('adaptive_ref_viz_features_rearranged.tsv', sep = '\t')

msnv_ids <- as.character(c(27, 60, 11, 30, 62, 72, 28, 74, 5, 20, 76, 66, 9, 3))

ctrl_matrix <- ref_viz_ctrl_matrix[, c("part_id", 'msnv', 'aoi_name', 'totaltimespent', 'proportiontime', 'timetofirstfixation', 'proportionnum','group')]

adpt_matrix <- ref_viz_adpt_matrix[, c("part_id", 'msnv', 'aoi_name', 'totaltimespent', 'proportiontime', 'timetofirstfixation', 'proportionnum','group')]

important_features <- c('totaltimespent', 'proportiontime', 'timetofirstfixation')

for(f in important_features) {
  pdf(paste0("control ", f, ".pdf"))
  p <- with(adpt_matrix[!(aoi_name=='Text_all'|aoi_name=='Viz_all'|aoi_name=='Refs')], 
            boxplot(get(f)~aoi_name, outline=FALSE, main=paste0("adaptive: ", f)))
  text(y = p$stats[3,1]+0.01, labels = p$stats[3,1], x = 1.5, cex=0.5, col="blue")
  text(y = p$stats[3,2]+0.01, labels = p$stats[3,2], x = 2.5, cex=0.5, col="blue")
  text(y = p$stats[3,3]+0.01, labels = p$stats[3,3], x = 3.5, cex=0.5, col="blue")
  text(y = p$stats[3,4]+0.01, labels = p$stats[3,4], x = 4.5, cex=0.5, col="blue")
  text(y = p$stats[3,5]+0.01, labels = p$stats[3,5], x = 5.5, cex=0.5, col="blue")
  text(y = p$stats[3,6]+0.01, labels = p$stats[3,6], x = 6.5, cex=0.5, col="blue")
  dev.off() 
}

combined_matrix <- rbind(ctrl_matrix, adpt_matrix)
combined_matrix[, group := factor(group, levels = c('control', 'adaptive'))]
setDT(combined_matrix)[timetofirstfixation == -1, timetofirstfixation := NA]

My_Theme = theme(
  axis.title.x = element_text(size = 18),
  axis.text.x = element_text(size = 20),
  axis.text.y = element_text(size = 20),
  axis.title.y = element_text(size = 18))

for(f in important_features) {
  if (f != 'proportiontime'| f != 'proportiontime') {
    means <- aggregate(get(f) ~ aoi_name + group, combined_matrix, function(i) round(mean(i)))
  } else {
    means <- aggregate(get(f) ~ aoi_name + group, combined_matrix, function(i) round(mean(i), digits = 5))
  }
  names(means) <- c('aoi_name', 'group', f)
  
  p <- ggplot(combined_matrix, aes_string(y=f, x='aoi_name', fill='group')) +
    geom_boxplot(na.rm = TRUE) +
    stat_summary(fun.y=mean, colour="gold2", geom="point", na.rm = TRUE,
                 position = position_dodge(0.75),
                 shape=18, size=5, show.legend = FALSE) + 
    geom_text(data = means, aes_string(label = f), size=4, colour='grey30',
              position = position_dodge(0.75)) +
    ggtitle(f) + My_Theme
  
  #ymax <- max(ggplot_build(p)[["data"]][[1]][["ymax_final"]])
  ggsave(paste0(f, ".png"), p, width=20, height= 24, limitsize = FALSE)
}

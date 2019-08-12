library(data.table)
library(hash)
library(stringr)
library(ggplot2)


msnv_ids <- as.character(c(27, 60, 11, 30, 62, 72, 28, 74, 5, 20, 76, 66, 9, 3))

control_dropped_sc <- hash('3'=c('12'), '11'=c('78'),'5'=c('68','40','59'),'27'=c('59'),
                           '72'=c('60','59'),'30'=c('71','59','97'),'18'=c('59','16','73','65','97'),
                           '66'=c('60'),'74'=c('16','52','97'),'76'=c('59'),'60'=c('55'),
                           '9'=c('55'),'20'=c('85'))
adaptive_dropped_sc <- hash('20'=c('msnv12'),'76'=c('msnv50'),'30'=c('msnv38'),
                            '66'=c('msnv69','msnv12'),'60'=c('msnv12','msnv81'),
                            '72'=c('msnv43'),'5'=c('msnv81'))

setwd("/Users/kristys/Documents/MSNV_Adapt_Gaze_Analysis_2019")
UC_features_df <- fread("control_adaptive_UC_performance.csv", sep=",") 
UC_features_df[, msnv := as.character(msnv)]


setwd("/Users/kristys/Documents/EMDAT-subtractive/outputfolder")
ctrl_features_df <- fread("control_ref_viz_features.tsv", sep="\t") 
adpt_features_df <- fread("adaptive_ref_viz_features.tsv", sep="\t") 
ctrl_col_names <- names(ctrl_features_df)[c(-1,-2)]
adpt_col_names <- names(adpt_features_df)[c(-1,-2)]

ctrl_part_id_row_indices <- which(grepl('_allsc', ctrl_features_df$Sc_id))
ctrl_part_ids <- gsub('_allsc', '', ctrl_features_df[ctrl_part_id_row_indices, Sc_id])
ctrl_matrix <- data.table()

adpt_part_id_row_indices <- which(grepl('_allsc', adpt_features_df$Sc_id))
adpt_part_ids <- gsub('_allsc', '', adpt_features_df[adpt_part_id_row_indices, Sc_id])
adpt_matrix <- data.table()

# generate new control feature matrix 
for (msnv in msnv_ids) {
  
  if (msnv %in% keys(control_dropped_sc)) {
    new_part_ids <- ctrl_part_ids[!ctrl_part_ids %in% control_dropped_sc[[msnv]]]
  } else {
    new_part_ids <- ctrl_part_ids
  }
  
  msnv_matrix <- data.table()
  
  for (col_name in ctrl_col_names) {
    subset <- subset(ctrl_features_df, ctrl_features_df$Sc_id == msnv, select = col_name)
    
    split <- str_split(col_name, '_')[[1]]
    if (('numtransfrom' %in% split) || ('proptransfrom' %in% split)) {
      d <- match(c('numtransfrom','proptransfrom'), split)
      d <- d[!is.na(d)][1]
      aoi_name <- paste(split[1:d-1], collapse = '_')
      feat_name <- paste(split[d:length(split)], collapse = '_')
    } else {
      aoi_name <- paste(split[-length(split)], collapse = '_')
      feat_name <- split[length(split)]
    }
    names(subset) <- c('value')

    msnv_matrix <- rbind(msnv_matrix, data.table(part_id=new_part_ids, msnv=msnv, aoi_name = aoi_name, feature = feat_name, subset))
    
  }
  # reshape
  reshape <- dcast(msnv_matrix, part_id + msnv + aoi_name ~ feature, value.var = 'value')
  
  ctrl_matrix <- rbind(ctrl_matrix, reshape)
  
}
ctrl_matrix <- merge(ctrl_matrix, UC_features_df, all.x=TRUE)


# generate new adaptive feature matrix 
for (msnv in msnv_ids) {
  
  if (msnv %in% keys(adaptive_dropped_sc)) {
    new_part_ids <- adpt_part_ids[!adpt_part_ids %in% adaptive_dropped_sc[[msnv]]]
  } else {
    new_part_ids <- adpt_part_ids
  }
  
  msnv_matrix <- data.table()
  
  for (col_name in adpt_col_names) {
    subset <- subset(adpt_features_df, adpt_features_df$Sc_id == msnv, select = col_name)
    
    split <- str_split(col_name, '_')[[1]]
    if (('numtransfrom' %in% split) || ('proptransfrom' %in% split)) {
      d <- match(c('numtransfrom','proptransfrom'), split)
      d <- d[!is.na(d)][1]
      aoi_name <- paste(split[1:d-1], collapse = '_')
      feat_name <- paste(split[d:length(split)], collapse = '_')
    } else {
      aoi_name <- paste(split[-length(split)], collapse = '_')
      feat_name <- split[length(split)]
    }
    names(subset) <- c('value')
    
    msnv_matrix <- rbind(msnv_matrix, data.table(part_id=new_part_ids, msnv=msnv, aoi_name = aoi_name, feature = feat_name, subset))
    
  }
  # reshape
  reshape <- dcast(msnv_matrix, part_id + msnv + aoi_name ~ feature, value.var = 'value')
  
  adpt_matrix <- rbind(adpt_matrix, reshape)
  
}
adpt_matrix <- merge(adpt_matrix, UC_features_df, all.x=TRUE)

# write out rearranged feature files 
setwd("/Users/kristys/Documents/MSNV_Adapt_Gaze_Analysis_2019")
write.table(ctrl_matrix, file='control_ref_viz_features_rearranged.tsv', sep = "\t", row.names = FALSE)
write.table(adpt_matrix, file='adaptive_ref_viz_features_rearranged.tsv', sep = "\t", row.names = FALSE)




### plotting control vs adaptive bar charts
important_features <- c('totaltimespent', 'proportiontime', 'timetofirstfixation')
graphs <- list()
for(f in important_features) {
  # stats for adaptive
  adpt_feat_stats <- adpt_matrix[, .(mean_feat = mean(get(f)), sd_feat = var(get(f)), size = .N),by = .(aoi_name, msnv, group)]
  adpt_feat_stats <- adpt_feat_stats[!(aoi_name=='Non-relevant bars' & (msnv=='20' | msnv=='60' | msnv=='62'))]
  
  adpt_feat_weighted_stats <- adpt_feat_stats[, .(weighted_mean = unlist(lapply(.SD,weighted.mean,w=size))),
                                                  by=.(aoi_name, group),.SDcols=c('mean_feat')]
  adpt_feat_weighted_stats$weighted_sd <- adpt_feat_stats[, .(weighted_sd = lapply(.SD,wtd.var,weights=size)),
                                                              by=.(aoi_name, group),.SDcols=c('sd_feat')]$weighted_sd
  adpt_feat_weighted_stats[, weighted_sd:=sqrt(as.numeric(weighted_sd))]
  
  # stats for control 
  ctrl_feat_stats <- ctrl_matrix[, .(mean_feat = mean(get(f)), sd_feat = var(get(f)), size = .N),by = .(aoi_name, msnv, group)]
  ctrl_feat_stats <- ctrl_feat_stats[!(aoi_name=='Non-relevant bars' & (msnv=='20' | msnv=='60' | msnv=='62'))]
  
  ctrl_feat_weighted_stats <- ctrl_feat_stats[, .(weighted_mean = unlist(lapply(.SD,weighted.mean,w=size))),
                                                  by=.(aoi_name, group),.SDcols=c('mean_feat')]
  ctrl_feat_weighted_stats$weighted_sd <- ctrl_feat_stats[, .(weighted_sd = lapply(.SD,wtd.var,weights=size)),
                                                              by=.(aoi_name, group),.SDcols=c('sd_feat')]$weighted_sd
  ctrl_feat_weighted_stats[, weighted_sd:=sqrt(as.numeric(weighted_sd))]

  adpt_feat_weighted_stats[, group := factor(group, levels = c('control', 'adaptive'))]
  ctrl_feat_weighted_stats[, group := factor(group, levels = c('control', 'adaptive'))]
  
  p <- ggplot(rbind(ctrl_feat_weighted_stats, adpt_feat_weighted_stats), aes(x=aoi_name, y=weighted_mean, fill=group)) +
    geom_bar(stat = "identity", color="black", position=position_dodge()) + 
    ggtitle(f) +
    theme(plot.title = element_text(hjust = 0.5)) 
  #+ geom_errorbar(aes(ymin=weighted_mean-weighted_sd, ymax=weighted_mean+weighted_sd), width=.2, position=position_dodge(.9))
  graphs[[f]] <- p
}


# checks normality of gaze metrics
qqnorm(ctrl_matrix[aoi_name=='Relevant bars']$totaltimespent, main = 'control: Relevant bars totaltimespent')
qqline(ctrl_matrix[aoi_name=='Relevant bars']$totaltimespent)







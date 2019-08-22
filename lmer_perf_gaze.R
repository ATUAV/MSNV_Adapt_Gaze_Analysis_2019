library(data.table)
library(lmerTest)

setwd("/Users/kristys/Documents/MSNV_Adapt_Gaze_Analysis_2019")
ref_viz_ctrl_matrix <- fread('control_ref_viz_features_rearranged.tsv', sep = '\t')
ref_viz_adpt_matrix <- fread('adaptive_ref_viz_features_rearranged.tsv', sep = '\t')

features <- c("part_id", 'msnv', 'aoi_name', 'group', 'mmd_task_time', 'mmd_accuracy',
              'totaltimespent', 'proportiontime', 'timetofirstfixation', 
              'meanfixationduration', 'numtransfrom_Relevant bars', 'numtransfrom_Non-relevant bars',
              'numtransfrom_Refs', 'numtransfrom_labels', 'numtransfrom_legend', 
              'BarChartLit', 'Meara', 'verbalWM')

ctrl_matrix <- ref_viz_ctrl_matrix[, ..features]
adpt_matrix <- ref_viz_adpt_matrix[, ..features]

combined_matrix <- rbind(ctrl_matrix, adpt_matrix)
combined_matrix[, group := factor(group, levels = c('control', 'adaptive'))]
names(combined_matrix) <- gsub(' |-', '_', names(combined_matrix))
combined_matrix$aoi_name <- gsub(' |-', '_', combined_matrix$aoi_name)

setDT(combined_matrix)[timetofirstfixation == -1, timetofirstfixation := 300000]

# scale the numeric measures to between 0 and 1 and center to 0 
scale01 <- function(x) {(x - min(x)) / (max(x) - min(x))}
combined_matrix[, names(combined_matrix)[-c(1, 2, 3, 4, 5, 6)] := lapply(.SD, function(x) scale(scale01(x), scale = FALSE)),
                 .SDcols=names(combined_matrix)[-c(1, 2, 3, 4, 5, 6)]]

# read in the spreadsheet from stage 1
results_matrix <- fread("lmer_gaze_UC_signif_effects.csv", sep = ',')
results_matrix <- results_matrix[significant=='y']
results_matrix <- unique(results_matrix, by=c('aoi', 'gaze metric'))


sink("R_output_LMM_perf_gaze_metrics_normalized_b.txt")

for(i in 1:nrow(results_matrix)) {
  for(uf in c('mmd_task_time', 'mmd_accuracy')) {
    metric <- results_matrix[i, 'gaze metric']
    aoi <- as.character(results_matrix[i, 'aoi'])
    
    formula <- paste0(uf, ' ~ ', metric, ' + (1 | msnv) + (1 | part_id)')
    
    test <- lmerTest::lmer(formula, na.omit(combined_matrix[aoi_name==aoi]), REML = TRUE)

    cat('\n')
    print(paste0('///////////////// AOI: ', results_matrix[i, 'aoi']))
    print(paste0('///////////////// sig. feature: ', results_matrix[i, 'feature']))
    print(paste0('///////////////// gaze metric: ', metric))
    print(paste0('||||||||||||||||| Model: ', formula))
    print(summary(test))
  }
  
}

closeAllConnections()







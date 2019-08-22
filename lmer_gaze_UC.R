library(data.table)
library(lmerTest)
library(ARTool)


setwd("/Users/kristys/Documents/MSNV_Adapt_Gaze_Analysis_2019")
ref_viz_ctrl_matrix <- fread('control_ref_viz_features_rearranged.tsv', sep = '\t')
ref_viz_adpt_matrix <- fread('adaptive_ref_viz_features_rearranged.tsv', sep = '\t')

msnv_ids <- as.character(c(27, 60, 11, 30, 62, 72, 28, 74, 5, 20, 76, 66, 9, 3))

features <- c("part_id", 'msnv', 'aoi_name','group','totaltimespent', 'proportiontime', 'timetofirstfixation', 
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

# scale to between 0 and 1 and center to 0 
scale01 <- function(x) {(x - min(x)) / (max(x) - min(x))}
# combined_matrix[, BarChartLit := scale(scale01(BarChartLit), scale = FALSE)]
# combined_matrix[, Meara := scale(scale01(Meara), scale = FALSE)]
# combined_matrix[, verbalWM := scale(scale01(verbalWM), scale = FALSE)]
combined_matrix[, names(combined_matrix)[-c(1, 2, 3, 4)] := lapply(.SD, function(x) scale(scale01(x), scale = FALSE)),
                .SDcols=names(combined_matrix)[-c(1, 2, 3, 4)]]

sink("R_output_LMM_gaze_metrics_UC_normalized_b.txt")

for (aoi in c('Relevant_bars', 'Non_relevant_bars', 'Refs', 'labels', 'legend')) {
  
  print(paste0('////////////////////////// Models for AOI ', aoi))
  
  for (y in c('totaltimespent', 'proportiontime', 'timetofirstfixation', 'meanfixationduration',
               'numtransfrom_Relevant_bars', 'numtransfrom_Non_relevant_bars',
              'numtransfrom_Refs', 'numtransfrom_labels', 'numtransfrom_legend')) {
    
    if (y==paste0('numtransfrom_', aoi)) next
    
    .env <- environment()
    formula <- as.formula(paste(y, "~ BarChartLit*group + Meara*group + verbalWM*group + (1 | msnv) + (1 | part_id)"), env= .env)
    test <- lmerTest::lmer(formula, na.omit(combined_matrix[aoi_name==aoi]), REML = TRUE)
    stepwise_model <- step(test, reduce.random=FALSE)
    
    cat('\n')
    print(paste0('||||||||||||||||| Model: ', paste(y, "~ BarChartLit*group + Meara*group + verbalWM*group + (1 | msnv) + (1 | part_id)")))
    print(summary(test))
    print('############################## Stepwise Model:')
    print(stepwise_model)
  }
  
  cat("///////////////////////////////////////////////////////////////////////////\n")
}

closeAllConnections()


## Error in summary.art(x) : 
# Aligned responses do not sum to ~0. ART may not be appropriate.

# combined_matrix$BarChartLit = as.factor(combined_matrix$BarChartLit)
# combined_matrix$Meara = as.factor(combined_matrix$Meara)
# combined_matrix$verbalWM = as.factor(combined_matrix$verbalWM)
# 
# sink("gaze_metrics_art_jul26.txt")
# 
# for (aoi in c('Relevant_bars', 'Non_relevant_bars', 'Refs', 'labels', 'legend')) {
#   
#   print(paste0('###################### Models for AOI ', aoi))
#   
#   for (y in c('totaltimespent', 'proportiontime', 'timetofirstfixation', 'meanfixationduration',
#               'numtransfrom_Relevant_bars', 'numtransfrom_Non_relevant_bars',
#               'numtransfrom_Refs', 'numtransfrom_labels', 'numtransfrom_legend')) {
#     
#     if (y==paste0('numtransfrom_', aoi)) next
#     
#     test <- art(get(y) ~ BarChartLit * group * verbalWM * Meara + (1 | msnv) + (1 | part_id), na.omit(combined_matrix[aoi_name==aoi]))
#     cat('\n')
#     print(paste0('######## Dependent Measure = ', y))
#     print(summary(test))
#   }
#   
#   cat("########################################################\n")
# }
# 
# closeAllConnections()



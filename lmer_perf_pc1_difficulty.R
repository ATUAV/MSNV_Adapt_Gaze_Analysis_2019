library(data.table)
library(lmerTest)
library(lsmeans)
library(Rmisc)
library(ggplot2)


setwd("/Users/kristys/Documents/MSNV_Adapt_Gaze_Analysis_2019")
ref_viz_ctrl_matrix <- fread('control_ref_viz_features_rearranged.tsv', sep = '\t')
ref_viz_adpt_matrix <- fread('adaptive_ref_viz_features_rearranged.tsv', sep = '\t')

features <- c("part_id", 'msnv', 'group', 'mmd_task_time', 'mmd_accuracy', 'BarChartLit', 'Meara')
ctrl_matrix <- ref_viz_ctrl_matrix[, ..features]
adpt_matrix <- ref_viz_adpt_matrix[, ..features]

combined_matrix <- rbind(ctrl_matrix, adpt_matrix)
combined_matrix[, group := factor(group, levels = c('control', 'adaptive'))]

# scale and center PC1 and difficulty 
scale01 <- function(x) {(x - min(x)) / (max(x) - min(x))}
pc1_df <- fread('msnv_pc1_coordinates.csv')
pc1_df[, PC1 := scale(scale01(PC1), scale = FALSE)]
difficulty_df <- fread('difficulty_ratings.csv')
difficulty_df[, difficulty := scale(scale01(difficulty), scale = FALSE)]

# merge in 3 UCs
UC_features_df <- fread("labelsControlAdaptive.csv", sep=",") 
combined_matrix <- merge(combined_matrix, UC_features_df[, c('part_id', 'msnv', 'VerbalWM_longest', 'NAART', 'N4C')])

# scale the numeric measures to between 0 and 1 and center to 0 
combined_matrix[, names(combined_matrix)[-c(1, 2, 3)] := lapply(.SD, function(x) scale(scale01(x), scale = FALSE)),
                .SDcols=names(combined_matrix)[-c(1, 2, 3)]]
# merge pc1 and difficulty into the same df 
merged_df <- merge(combined_matrix, pc1_df, by='msnv')
merged_df <- merge(merged_df, difficulty_df, by=c('msnv', 'part_id'))
# eliminate duplicated rows for different AOIs
merged_df <- merged_df[, .SD[1], by=c('msnv', 'part_id')]


### run performance on pc1 and group and interaction

sink("R_output_LMM_perf_pc1_normalized_b.txt")

for(uf in c('mmd_task_time', 'mmd_accuracy')) {
  formula <- paste0(uf, ' ~ ', 'PC1*group + (1 | msnv) + (1 | part_id)')
  
  test <- lmerTest::lmer(formula, merged_df, REML = TRUE)
  
  cat('\n')
  print(paste0('||||||||||||||||| Model: ', formula))
  print(summary(test))
  
}
closeAllConnections()

### run performance on difficulty and group and interaction
sink("R_output_LMM_perf_difficulty_normalized_b.txt")

for(uf in c('mmd_task_time', 'mmd_accuracy')) {
  formula <- paste0(uf, ' ~ ', 'difficulty*group + (1 | msnv) + (1 | part_id)')
  
  test <- lmerTest::lmer(formula, merged_df, REML = TRUE)
  
  cat('\n')
  print(paste0('||||||||||||||||| Model: ', formula))
  print(summary(test))
  
}
closeAllConnections()


##### TVCG models with PC1 or difficulty added 

sink("R_output_LMM_perf_pc1_UC_normalized_b.txt")

for(uf in c('mmd_task_time', 'mmd_accuracy')) {
  formula <- paste0(uf, ' ~ ', '(PC1 + BarChartLit + Meara + VerbalWM_longest + NAART + N4C)*group + (1 | msnv) + (1 | part_id)')
  
  test <- lmerTest::lmer(formula, merged_df, REML = TRUE)
  stepwise_model <- step(test)
  
  cat('\n')
  print(paste0('||||||||||||||||| Model: ', formula))
  print(summary(test))
  print('||||||||||||||||| Stepwise Model:')
  print(stepwise_model)
  
}
closeAllConnections()

sink("R_output_LMM_perf_difficulty_UC_normalized_b.txt")

for(uf in c('mmd_task_time', 'mmd_accuracy')) {
  formula <- paste0(uf, ' ~ ', '(difficulty + BarChartLit + Meara + VerbalWM_longest + NAART + N4C)*group + (1 | msnv) + (1 | part_id)')
  
  test <- lmerTest::lmer(formula, merged_df, REML = TRUE)
  stepwise_model <- step(test)
  
  cat('\n')
  print(paste0('||||||||||||||||| Model: ', formula))
  print(summary(test))
  print('||||||||||||||||| Stepwise Model:')
  print(stepwise_model)
  
}
closeAllConnections()


##### specify models involving PC1 or difficulty found by the step function
test <- lmerTest::lmer(mmd_task_time ~ PC1 + VerbalWM_longest + group + (1 | msnv) + 
                         (1 | part_id) + PC1:group, merged_df, REML = TRUE)
anova(test)
# Type III Analysis of Variance Table with Satterthwaite's method
#                    Sum Sq  Mean Sq NumDF   DenDF F value    Pr(>F)    
# PC1              0.243734 0.243734     1   12.03 65.7649 3.219e-06 ***
# VerbalWM_longest 0.016964 0.016964     1   94.81  4.5773   0.03497 *  
# group            0.005449 0.005449     1   94.83  1.4701   0.22834    
# PC1:group        0.016670 0.016670     1 1232.18  4.4978   0.03414 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

test <- lmerTest::lmer(mmd_task_time ~ difficulty + BarChartLit + VerbalWM_longest + 
                         (1 | msnv) + (1 | part_id), merged_df, REML = TRUE)
anova(test)
# Type III Analysis of Variance Table with Satterthwaite's method
#                    Sum Sq  Mean Sq NumDF   DenDF F value    Pr(>F)    
# difficulty       0.094519 0.094519     1 1305.10 25.9498 4.019e-07 ***
# BarChartLit      0.014998 0.014998     1   94.79  4.1176   0.04524 *  
# VerbalWM_longest 0.018136 0.018136     1   94.62  4.9792   0.02801 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

test <- lmerTest::lmer(mmd_accuracy ~ difficulty + BarChartLit + NAART + N4C + group + 
                         (1 | msnv) + (1 | part_id) + BarChartLit:group + NAART:group + 
                         N4C:group, merged_df, REML = TRUE)
anova(test)
# Type III Analysis of Variance Table with Satterthwaite's method
#                    Sum Sq Mean Sq NumDF  DenDF F value    Pr(>F)    
# difficulty        0.54613 0.54613     1 626.82  9.2101  0.002507 ** 
# BarChartLit       0.05219 0.05219     1  89.72  0.8802  0.350668    
# NAART             0.28087 0.28087     1  89.75  4.7368  0.032150 *  
# N4C               0.14064 0.14064     1  89.13  2.3718  0.127091    
# group             0.17319 0.17319     1  89.55  2.9207  0.090913 .  
# BarChartLit:group 1.02257 1.02257     1  88.87 17.2450 7.525e-05 ***
# NAART:group       0.60158 0.60158     1  88.89 10.1452  0.001996 ** 
# N4C:group         0.35690 0.35690     1  89.18  6.0189  0.016098 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



############# 3-way split 

pc1_df <- pc1_df[order(PC1)]
pivot_low <- pc1_df[nrow(pc1_df)*(1/3)]$PC1
pivot_high <- pc1_df[nrow(pc1_df)*(2/3)]$PC1
merged_df[PC1<=pivot_low, PC13 := 'Low']
merged_df[PC1>pivot_low & PC1<=pivot_high, PC13 := 'Med']
merged_df[PC1>pivot_high, PC13 := 'High']
merged_df[, PC13 := factor(PC13, levels=c('Low', 'Med', 'High'))]

difficulty_df <- difficulty_df[order(difficulty)]
pivot_low <- difficulty_df[nrow(difficulty_df)*(1/3)]$difficulty
pivot_high <- difficulty_df[nrow(difficulty_df)*(2/3)]$difficulty
merged_df[difficulty<=pivot_low, difficulty3 := 'Low']
merged_df[difficulty>pivot_low & difficulty<=pivot_high, difficulty3 := 'Med']
merged_df[difficulty>pivot_high, difficulty3 := 'High']
merged_df[, difficulty3 := factor(difficulty3, levels=c('Low', 'Med', 'High'))]

uc_subset <- merged_df[, .SD[1], by=part_id, .SDcols=c('NAART', 'N4C')]

uc_subset <- uc_subset[order(NAART)]
pivot_low <- uc_subset[nrow(uc_subset)*(1/3)]$NAART
pivot_high <- uc_subset[nrow(uc_subset)*(2/3)]$NAART
merged_df[NAART<pivot_low, NAART3 := 'Low']
merged_df[NAART>=pivot_low & NAART<pivot_high, NAART3 := 'Med']
merged_df[NAART>=pivot_high, NAART3 := 'High']
merged_df[, NAART3 := factor(NAART3, levels=c('Low', 'Med', 'High'))]

uc_subset <- uc_subset[order(N4C)]
pivot_low <- uc_subset[nrow(uc_subset)*(1/3)]$N4C
pivot_high <- uc_subset[nrow(uc_subset)*(2/3)]$N4C
merged_df[N4C<=pivot_low, N4C3 := 'Low']
merged_df[N4C>pivot_low & N4C<pivot_high, N4C3 := 'Med']
merged_df[N4C>=pivot_high, N4C3 := 'High']
merged_df[, N4C3 := factor(N4C3, levels=c('Low', 'Med', 'High'))]

VisLit3 <- fread('3_UCs_2_3_way_splits.tsv', sep='\t')[, c('part_id', 'VisLit3')]
merged_df <- merge(merged_df, VisLit3, by='part_id')

# contrast 
test <- lmerTest::lmer(mmd_task_time ~ PC13 + VerbalWM_longest + group + (1 | msnv) + 
                         (1 | part_id) + PC13:group, merged_df, REML = TRUE)
means1 <- lsmeans(test, pairwise ~ PC13|group, adjust = 'fdr')
means2 <- lsmeans(test, pairwise ~ group|PC13, adjust = 'fdr')
means1$contrasts
means2$contrasts

test <- lmerTest::lmer(mmd_accuracy ~ difficulty + BarChartLit + NAART + N4C3 + group + 
                         (1 | msnv) + (1 | part_id) + BarChartLit:group + NAART:group + 
                         N4C3:group, merged_df, REML = TRUE)
means1 <- lsmeans(test, pairwise ~ N4C3|group, adjust = 'fdr')
means2 <- lsmeans(test, pairwise ~ group|N4C3, adjust = 'fdr')
means1$contrasts
means2$contrasts

test <- lmerTest::lmer(mmd_accuracy ~ difficulty + BarChartLit + NAART3 + N4C + group + 
                         (1 | msnv) + (1 | part_id) + BarChartLit:group + NAART3:group + 
                         N4C:group, merged_df, REML = TRUE)
means1 <- lsmeans(test, pairwise ~ NAART3|group, adjust = 'fdr')
means2 <- lsmeans(test, pairwise ~ group|NAART3, adjust = 'fdr')
means1$contrasts
means2$contrasts


############# 2-way split 
merged_df[PC1>=median(pc1_df$PC1), PC12 := 'High']
merged_df[PC1<median(pc1_df$PC1), PC12 := 'Low']
merged_df[, PC12 := factor(PC12, levels=c('Low', 'High'))]

merged_df[NAART>=median(merged_df$NAART), NAART2 := 'High']
merged_df[NAART<median(merged_df$NAART), NAART2 := 'Low']
merged_df[, NAART2 := factor(NAART2, levels=c('Low', 'High'))]
merged_df[N4C>=median(merged_df$N4C), N4C2 := 'High']
merged_df[N4C<median(merged_df$N4C), N4C2 := 'Low']
merged_df[, N4C2 := factor(N4C2, levels=c('Low', 'High'))]
# table(p[group=='adaptive']$NAART2)
# Low High 
# 0   46 
# table(p[group=='control']$NAART2)
# Low High 
# 49    3 

# contrast 
test <- lmerTest::lmer(mmd_task_time ~ PC12 + VerbalWM_longest + group + (1 | msnv) + 
                         (1 | part_id) + PC12:group, merged_df, REML = TRUE)
means1 <- lsmeans(test, pairwise ~ PC12|group, adjust = 'fdr')
means2 <- lsmeans(test, pairwise ~ group|PC12, adjust = 'fdr')
means1$contrasts
means2$contrasts

sss <- summarySE(merged_df, measurevar="mmd_task_time", groupvars=c("PC12","group"))
ggplot(sss, aes(x=PC12, y=mmd_task_time, group=group)) + 
  geom_point(aes(color=group)) + 
  geom_errorbar(aes(ymin=mmd_task_time-ci*0.8, ymax=mmd_task_time+ci*0.8, color=group), width=.2, 
                position = position_dodge(width = 0.08), size = 0.6, show.legend=FALSE) +
  geom_line(aes(color=group, linetype=group), size = 0.75) +
  theme(legend.key.size =  unit(0.5, "in")) +
  scale_x_discrete(labels=c("Low", "High")) + 
  labs(x = 'complexity', y = "speed") + 
  scale_linetype_manual(values=c("dashed",'solid'))

test <- lmerTest::lmer(mmd_accuracy ~ difficulty + BarChartLit + NAART + N4C2 + group + 
                         (1 | msnv) + (1 | part_id) + BarChartLit:group + NAART:group + 
                         N4C2:group, merged_df, REML = TRUE)
means1 <- lsmeans(test, pairwise ~ N4C2|group, adjust = 'fdr')
means2 <- lsmeans(test, pairwise ~ group|N4C2, adjust = 'fdr')
means1$contrasts
means2$contrasts

test <- lmerTest::lmer(mmd_accuracy ~ difficulty + BarChartLit + NAART2 + N4C + group + 
                         (1 | msnv) + (1 | part_id) + BarChartLit:group + NAART2:group + 
                         N4C:group, merged_df, REML = TRUE)
means1 <- lsmeans(test, pairwise ~ NAART2|group, adjust = 'fdr')
means2 <- lsmeans(test, pairwise ~ group|NAART2, adjust = 'fdr')
means1$contrasts
means2$contrasts

sss <- summarySE(merged_df, measurevar="mmd_accuracy", groupvars=c("NAART2","group"))
ggplot(sss, aes(x=NAART2, y=mmd_accuracy, group=group)) + 
  geom_point(aes(color=group)) + 
  geom_errorbar(aes(ymin=mmd_accuracy-ci*0.8, ymax=mmd_accuracy+ci*0.8, color=group), width=.2, 
                position = position_dodge(width = 0.08), size = 0.6, show.legend=FALSE) +
  geom_line(aes(color=group, linetype=group), size = 0.75) +
  theme(legend.key.size =  unit(0.5, "in")) +
  scale_x_discrete(labels=c("Low", "High")) + 
  labs(x = 'VerbalIQ', y = "accuracy") + 
  scale_linetype_manual(values=c("dashed",'solid'))
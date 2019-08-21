library(data.table)
library(lmerTest)
library(lsmeans)
library(Rmisc)

setwd("/Users/kristys/Documents/MSNV_Adapt_Gaze_Analysis_2019")
ref_viz_ctrl_matrix <- fread('control_ref_viz_features_rearranged.tsv', sep = '\t')
ref_viz_adpt_matrix <- fread('adaptive_ref_viz_features_rearranged.tsv', sep = '\t')

msnv_ids <- as.character(c(27, 60, 11, 30, 62, 72, 28, 74, 5, 20, 76, 66, 9, 3))

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

ctrl_bcl_raw <- unique(fread("mmd_study1_barchartlit_raw.csv", sep=",")[, c('user_id', 'BarChartLit_raw')])
setnames(ctrl_bcl_raw, 1, 'part_id')
ctrl_bcl_raw[, part_id := as.character(part_id)]
adpt_bcl_raw <- fread("mmd_study2_barchartlit_raw.csv", sep=",")[, c('user_id', 'BarChartLit_raw')]
setnames(adpt_bcl_raw, 1, 'part_id')
bcl_raw <- rbind(ctrl_bcl_raw, adpt_bcl_raw)
combined_matrix <- merge(combined_matrix, bcl_raw, all.x = TRUE)

uc_subset <- combined_matrix[, .SD[1], by=part_id, .SDcols=c('Meara', 'BarChartLit_raw', 'BarChartLit', 'verbalWM')]


###### 2-way split

combined_matrix[Meara>=median(uc_subset$Meara), Meara2 := 'High']
combined_matrix[Meara<median(uc_subset$Meara), Meara2 := 'Low']
combined_matrix[verbalWM>=median(uc_subset$verbalWM), verbalWM2 := 'High']
combined_matrix[verbalWM<median(uc_subset$verbalWM), verbalWM2 := 'Low']
combined_matrix[, Meara2 := factor(Meara2, levels=c('Low', 'High'))]
combined_matrix[BarChartLit_raw>=median(uc_subset$BarChartLit_raw), VisLit2 := 'High']
combined_matrix[BarChartLit_raw<median(uc_subset$BarChartLit_raw), VisLit2 := 'Low']
combined_matrix[, VisLit2 := factor(VisLit2, levels=c('Low', 'High'))]

# new pruned models without non-significant terms found in stage 1 and contrast tests
test <- lmerTest::lmer(numtransfrom_Refs ~ Meara2*group + (1 | msnv) + (1 | part_id),
                       na.omit(combined_matrix[aoi_name=='Relevant_bars']), REML = TRUE)
means1 <- lsmeans(test, pairwise ~ Meara2|group, adjust = 'none')
means2 <- lsmeans(test, pairwise ~ group|Meara2, adjust = 'none')
means1$contrasts
means2$contrasts

test <- lmerTest::lmer(numtransfrom_labels ~ VisLit2*group + (1 | msnv) + (1 | part_id),
                       na.omit(combined_matrix[aoi_name=='Relevant_bars']), REML = TRUE)
means1 <- lsmeans(test, pairwise ~ VisLit2|group, adjust = 'fdr')
means2 <- lsmeans(test, pairwise ~ group|VisLit2, adjust = 'fdr')
means1$contrasts
means2$contrasts

test <- lmerTest::lmer(numtransfrom_Non_relevant_bars ~ VisLit2*group + (1 | msnv) + (1 | part_id),
                       na.omit(combined_matrix[aoi_name=='legend']), REML = TRUE)
means1 <- lsmeans(test, pairwise ~ VisLit2|group, adjust = 'none')
means2 <- lsmeans(test, pairwise ~ group|VisLit2, adjust = 'none')
means1$contrasts
means2$contrasts

test <- lmerTest::lmer(numtransfrom_Refs ~ VisLit2*group + (1 | msnv) + (1 | part_id),
                       na.omit(combined_matrix[aoi_name=='legend']), REML = TRUE)
means1 <- lsmeans(test, pairwise ~ VisLit2|group, adjust = 'none')
means2 <- lsmeans(test, pairwise ~ group|VisLit2, adjust = 'none')
means1$contrasts
means2$contrasts

# 2-way split interaction plots
theme_update(plot.title = element_text(hjust = 0.5))

sss <- summarySE(combined_matrix[aoi_name=='Relevant_bars'], measurevar="numtransfrom_Refs", groupvars=c("Meara2","group"))
ggplot(sss, aes(x=Meara2, y=numtransfrom_Refs, group=group)) + 
  geom_point(aes(color=group)) + 
  geom_errorbar(aes(ymin=numtransfrom_Refs-ci*0.8, ymax=numtransfrom_Refs+ci*0.8, color=group), width=.2, 
                position = position_dodge(width = 0.08), size = 0.6, show.legend=FALSE) +
  geom_line(aes(color=group, linetype=group), size = 0.75) +
  theme(legend.key.size =  unit(0.5, "in")) +
  scale_x_discrete(labels=c("Low", "High")) + 
  labs(x = 'Reading Proficiency', y = "transitions from Refs to R-Bars") + 
  scale_linetype_manual(values=c("dashed",'solid'))

sss <- summarySE(combined_matrix[aoi_name=='Relevant_bars'], measurevar="numtransfrom_labels", groupvars=c("VisLit2","group"))
ggplot(sss, aes(x=VisLit2, y=numtransfrom_labels, group=group)) + 
  geom_errorbar(aes(ymin=numtransfrom_labels-ci*0.8, ymax=numtransfrom_labels+ci*0.8, color=group), width=.2, 
                position = position_dodge(width = 0.08), size = 0.6, show.legend=FALSE) +
  geom_point(aes(color=group)) + 
  geom_line(aes(color=group, linetype=group), size = 0.75) +
  theme(legend.key.size =  unit(0.5, "in")) +
  scale_x_discrete(labels=c("Low", "High")) + 
  labs(x = 'Vis Literacy', y = "transitions from Labels to R-Bars") + 
  scale_linetype_manual(values=c("dashed",'solid'))

sss <- summarySE(combined_matrix[aoi_name=='legend'], measurevar="numtransfrom_Non_relevant_bars", groupvars=c("VisLit2","group"))
ggplot(sss, aes(x=VisLit2, y=numtransfrom_Non_relevant_bars, group=group)) + 
  geom_errorbar(aes(ymin=numtransfrom_Non_relevant_bars-ci*0.8, ymax=numtransfrom_Non_relevant_bars+ci*0.8, color=group), width=.2, 
                position = position_dodge(width = 0.08), size = 0.6, show.legend=FALSE) +
  geom_point(aes(color=group)) + 
  geom_line(aes(color=group, linetype=group), size = 0.75) +
  theme(legend.key.size =  unit(0.5, "in")) +
  scale_x_discrete(labels=c("Low", "High")) + 
  labs(x = 'Vis Literacy', y = "transitions from NR-Bars to Legend") + 
  scale_linetype_manual(values=c("dashed",'solid'))

sss <- summarySE(combined_matrix[aoi_name=='legend'], measurevar="numtransfrom_Refs", groupvars=c("VisLit2","group"))
ggplot(sss, aes(x=VisLit2, y=numtransfrom_Refs, group=group)) + 
  geom_errorbar(aes(ymin=numtransfrom_Refs-ci*0.8, ymax=numtransfrom_Refs+ci*0.8, color=group), width=.2, 
                position = position_dodge(width = 0.08), size = 0.6, show.legend=FALSE) +
  geom_point(aes(color=group)) + 
  geom_line(aes(color=group, linetype=group), size = 0.75) +
  theme(legend.key.size =  unit(0.5, "in")) +
  scale_x_discrete(labels=c("Low", "High")) + 
  labs(x = 'Vis Literacy', y = "transitions from Refs to Legend") + 
  scale_linetype_manual(values=c("dashed",'solid'))


##### 3-way split

# uc_subset <- uc_subset[order(BarChartLit_raw)]
# pivot_low <- uc_subset[nrow(uc_subset)*(1/3)]$BarChartLit_raw
# pivot_high <- uc_subset[nrow(uc_subset)*(2/3)]$BarChartLit_raw
# combined_matrix[BarChartLit_raw<pivot_low, VisLit3 := 'Low']
# combined_matrix[BarChartLit_raw>=pivot_low & BarChartLit_raw<pivot_high, VisLit3 := 'Med']
# combined_matrix[BarChartLit_raw>=pivot_high, VisLit3 := 'High']
# combined_matrix[, VisLit3 := factor(VisLit3, levels=c('Low', 'Med', 'High'))]

uc_subset <- uc_subset[order(Meara)]
pivot_low <- uc_subset[nrow(uc_subset)*(1/3)]$Meara
pivot_high <- uc_subset[nrow(uc_subset)*(2/3)]$Meara
combined_matrix[Meara<pivot_low, Meara3 := 'Low']
combined_matrix[Meara>=pivot_low & Meara<pivot_high, Meara3 := 'Med']
combined_matrix[Meara>=pivot_high, Meara3 := 'High']
combined_matrix[, Meara3 := factor(Meara3, levels=c('Low', 'Med', 'High'))]

uc_subset <- uc_subset[order(verbalWM)]
pivot_low <- uc_subset[nrow(uc_subset)*(1/3)]$verbalWM
pivot_high <- uc_subset[nrow(uc_subset)*(2/3)]$verbalWM
combined_matrix[verbalWM<pivot_low, verbalWM3 := 'Low']
combined_matrix[verbalWM>=pivot_low & verbalWM<pivot_high, verbalWM3 := 'Med']
combined_matrix[verbalWM>=pivot_high, verbalWM3 := 'High']
combined_matrix[, verbalWM3 := factor(verbalWM3, levels=c('Low', 'Med', 'High'))]

# Dereck's way of 3-way split for VisLit
uc_subset <- uc_subset[order(BarChartLit,BarChartLit_raw)]
pivot_lowA <- uc_subset[nrow(uc_subset)*(1/3)]$BarChartLit
pivot_highA <- uc_subset[nrow(uc_subset)*(2/3)]$BarChartLit
pivot_lowB <- uc_subset[nrow(uc_subset)*(1/3)]$BarChartLit_raw
pivot_highB <- uc_subset[nrow(uc_subset)*(2/3)]$BarChartLit_raw
combined_matrix[BarChartLit<pivot_lowA, VisLit3 := 'Low']
combined_matrix[BarChartLit==pivot_lowA & BarChartLit_raw<pivot_lowB, VisLit3 := 'Low']
combined_matrix[BarChartLit<=pivot_highA & BarChartLit>pivot_lowA, VisLit3 := 'Med']
combined_matrix[BarChartLit==pivot_lowA & BarChartLit_raw>=pivot_lowB, VisLit3 := 'Med']
combined_matrix[BarChartLit>pivot_highA, VisLit3 := 'High']
combined_matrix[, VisLit3 := factor(VisLit3, levels=c('Low', 'Med', 'High'))]

# specify new pruned models without non-significant terms found in stage 1 and do contrast tests
test <- lmerTest::lmer(numtransfrom_Refs ~ Meara3*group + (1 | msnv) + (1 | part_id),
                       na.omit(combined_matrix[aoi_name=='Relevant_bars']), REML = TRUE)
means1 <- lsmeans(test, pairwise ~ Meara3|group, adjust = 'fdr')
means2 <- lsmeans(test, pairwise ~ group|Meara3, adjust = 'fdr')
means1$contrasts
means2$contrasts

test <- lmerTest::lmer(numtransfrom_labels ~ VisLit3*group + (1 | msnv) + (1 | part_id),
                       na.omit(combined_matrix[aoi_name=='Relevant_bars']), REML = TRUE)
means1 <- lsmeans(test, pairwise ~ VisLit3|group, adjust = 'fdr')
means2 <- lsmeans(test, pairwise ~ group|VisLit3, adjust = 'fdr')
means1$contrasts
means2$contrasts

test <- lmerTest::lmer(numtransfrom_Non_relevant_bars ~ VisLit3*group + (1 | msnv) + (1 | part_id),
                       na.omit(combined_matrix[aoi_name=='legend']), REML = TRUE)
means1 <- lsmeans(test, pairwise ~ VisLit3|group, adjust = 'fdr')
means2 <- lsmeans(test, pairwise ~ group|VisLit3, adjust = 'fdr')
means1$contrasts
means2$contrasts

test <- lmerTest::lmer(numtransfrom_Refs ~ VisLit3*group + (1 | msnv) + (1 | part_id),
                       na.omit(combined_matrix[aoi_name=='legend']), REML = TRUE)
means1 <- lsmeans(test, pairwise ~ VisLit3|group, adjust = 'fdr')
means2 <- lsmeans(test, pairwise ~ group|VisLit3, adjust = 'fdr')
means1$contrasts
means2$contrasts

# 3-way split interaction plots

sss <- summarySE(combined_matrix[aoi_name=='Relevant_bars'], measurevar="numtransfrom_Refs", groupvars=c("Meara3","group"))
ggplot(sss, aes(x=Meara3, y=numtransfrom_Refs, group=group)) + 
  geom_point(aes(color=group)) + 
  geom_errorbar(aes(ymin=numtransfrom_Refs-ci*0.8, ymax=numtransfrom_Refs+ci*0.8, color=group), width=.2, 
                position = position_dodge(width = 0.08), size = 0.6, show.legend=FALSE) +
  geom_line(aes(color=group, linetype=group), size = 0.75) +
  theme(legend.key.size =  unit(0.5, "in")) +
  scale_x_discrete(labels=c("Low", "Medium", "High")) + 
  labs(x = 'Reading Proficiency', y = "transitions from Refs to R-Bars") + 
  scale_linetype_manual(values=c("dashed",'solid'))

sss <- summarySE(combined_matrix[aoi_name=='Relevant_bars'], measurevar="numtransfrom_labels", groupvars=c("VisLit3","group"))
ggplot(sss, aes(x=VisLit3, y=numtransfrom_labels, group=group)) + 
  geom_errorbar(aes(ymin=numtransfrom_labels-ci*0.8, ymax=numtransfrom_labels+ci*0.8, color=group), width=.2, 
                position = position_dodge(width = 0.08), size = 0.6, show.legend=FALSE) +
  geom_point(aes(color=group)) + 
  geom_line(aes(color=group, linetype=group), size = 0.75) +
  theme(legend.key.size =  unit(0.5, "in")) +
  scale_x_discrete(labels=c("Low", "Medium", "High")) + 
  labs(x = 'Vis Literacy', y = "transitions from Labels to R-Bars") + 
  scale_linetype_manual(values=c("dashed",'solid'))

sss <- summarySE(combined_matrix[aoi_name=='legend'], measurevar="numtransfrom_Non_relevant_bars", groupvars=c("VisLit3","group"))
ggplot(sss, aes(x=VisLit3, y=numtransfrom_Non_relevant_bars, group=group)) + 
  geom_errorbar(aes(ymin=numtransfrom_Non_relevant_bars-ci*0.8, ymax=numtransfrom_Non_relevant_bars+ci*0.8, color=group), width=.2, 
                position = position_dodge(width = 0.08), size = 0.6, show.legend=FALSE) +
  geom_point(aes(color=group)) + 
  geom_line(aes(color=group, linetype=group), size = 0.75) +
  theme(legend.key.size =  unit(0.5, "in")) +
  scale_x_discrete(labels=c("Low", "Medium", "High")) + 
  labs(x = 'Vis Literacy', y = "transitions from NR-Bars to Legend") + 
  scale_linetype_manual(values=c("dashed",'solid'))

sss <- summarySE(combined_matrix[aoi_name=='legend'], measurevar="numtransfrom_Refs", groupvars=c("VisLit3","group"))
ggplot(sss, aes(x=VisLit3, y=numtransfrom_Refs, group=group)) + 
  geom_errorbar(aes(ymin=numtransfrom_Refs-ci*0.8, ymax=numtransfrom_Refs+ci*0.8, color=group), width=.2, 
                position = position_dodge(width = 0.08), size = 0.6, show.legend=FALSE) +
  geom_point(aes(color=group)) + 
  geom_line(aes(color=group, linetype=group), size = 0.75) +
  theme(legend.key.size =  unit(0.5, "in")) +
  scale_x_discrete(labels=c("Low", "Medium", "High")) + 
  labs(x = 'Vis Literacy', y = "transitions from Refs to Legend") + 
  scale_linetype_manual(values=c("dashed",'solid'))


#### for marginally significant interaction effects found in stage 1

test <- lmerTest::lmer(numtransfrom_Non_relevant_bars ~ Meara3 + VisLit3*group + (1 | msnv) + (1 | part_id),
                       na.omit(combined_matrix[aoi_name=='Relevant_bars']), REML = TRUE)
means1 <- lsmeans(test, pairwise ~ VisLit3|group, adjust = 'none')
means2 <- lsmeans(test, pairwise ~ group|VisLit3, adjust = 'none')
means1$contrasts
means2$contrasts

test <- lmerTest::lmer(numtransfrom_legend ~ VisLit3*group + (1 | msnv) + (1 | part_id),
                       na.omit(combined_matrix[aoi_name=='Relevant_bars']), REML = TRUE)
means1 <- lsmeans(test, pairwise ~ VisLit3|group, adjust = 'none')
means2 <- lsmeans(test, pairwise ~ group|VisLit3, adjust = 'none')
means1$contrasts
means2$contrasts

test <- lmerTest::lmer(totaltimespent ~ Meara3*group + (1 | msnv) + (1 | part_id),
                       na.omit(combined_matrix[aoi_name=='Non_relevant_bars']), REML = TRUE)
means1 <- lsmeans(test, pairwise ~ Meara3|group, adjust = 'none')
means2 <- lsmeans(test, pairwise ~ group|Meara3, adjust = 'none')
means1$contrasts
means2$contrasts

test <- lmerTest::lmer(numtransfrom_Relevant_bars ~ Meara3 + VisLit3*group + (1 | msnv) + (1 | part_id),
                       na.omit(combined_matrix[aoi_name=='Non_relevant_bars']), REML = TRUE)
means1 <- lsmeans(test, pairwise ~ VisLit3|group, adjust = 'none')
means2 <- lsmeans(test, pairwise ~ group|VisLit3, adjust = 'none')
means1$contrasts
means2$contrasts

test <- lmerTest::lmer(meanfixationduration ~ VisLit3*group + (1 | msnv) + (1 | part_id),
                       na.omit(combined_matrix[aoi_name=='Refs']), REML = TRUE)
means1 <- lsmeans(test, pairwise ~ VisLit3|group, adjust = 'none')
means2 <- lsmeans(test, pairwise ~ group|VisLit3, adjust = 'none')
means1$contrasts
means2$contrasts

test <- lmerTest::lmer(numtransfrom_Relevant_bars ~ VisLit3*group + (1 | msnv) + (1 | part_id),
                       na.omit(combined_matrix[aoi_name=='labels']), REML = TRUE)
means1 <- lsmeans(test, pairwise ~ VisLit3|group, adjust = 'none')
means2 <- lsmeans(test, pairwise ~ group|VisLit3, adjust = 'none')
means1$contrasts
means2$contrasts

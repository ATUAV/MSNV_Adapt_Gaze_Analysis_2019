library(data.table)
library(REdaS)


setwd("/Users/kristys/Documents/MSNV_Adapt_Gaze_Analysis_2019")

msnv_df <- fread('msnv complexity.csv')

### check corrleation of complexity measures
cor(msnv_df[, -c('MSNV')])

Modes <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}
Modes(UC_features_df[msnv=='5', difficulty])

### find the reverse of ease 
UC_features_df <- fread("labelsControlAdaptive.csv") 
UC_features_df[, difficulty := 6-ease]
## write out difficulty for LMM later
write.csv(UC_features_df[, c('part_id', 'msnv', 'difficulty')], file = 'difficulty_ratings.csv', row.names = FALSE)


### DO PCA
subset <- UC_features_df[, .(difficulty_mean = mean(difficulty), 
                             difficulty_sd = sd(difficulty), difficulty_mode = Modes(difficulty)[1]),
                         by=msnv]
setnames(msnv_df, 'MSNV', 'msnv')

#pcout=prcomp(msnv_df[, -c('msnv')], scale=TRUE) ## without the subjective measure 

measures_df <- merge(msnv_df, subset[, .(msnv, difficulty_mean)], by='msnv')[, -c('msnv')]
pcout=prcomp(measures_df, scale=TRUE)

summary(pcout)

pcout$rotation ## the components/vectors 
# (since the signs for the loadings/weights in component 1 are all negative, we should negate the 1st component and its coordinates)

## write out the negated pc1 coordinates 
x <- as.data.frame(pcout$x)[, c('PC1'), drop=FALSE]
x$msnv <- c('3', '5', '9', '11', '20', '27', '28', '30', '60', '62', '66', '72', '74', '76')
x$PC1 <- - x$PC1
x <- x[, c(2,1)]
write.csv(x, 'msnv_pc1_coordinates.csv', row.names = FALSE)


# Bartlett's Test of Sphericity
bart <- bart_spher(measures_df)
print(bart)
# X2 = 55.719
# df = 15
# p-value < 2.22e-16

# Kaiser-Meyer-Olkin Statistics
kmo <- KMOS(measures_df)
print(kmo)
# Measures of Sampling Adequacy (MSA):
#   Words       Sentences      References       Datapoint   Relevant Bars difficulty_mean 
# 0.7590856     0.9220564       0.8680619       0.6203770       0.7390426       0.7635270 
# 
# KMO-Criterion: 0.7881805

# Kaiser criterion (Kaiser, 1960): retain and interpret any component with an eigenvalue greater than 1.00
(pcout$sdev)^2 # the eigenvalues of the components
# 4.04497992 1.14707319 0.38173375 0.21832193 0.10933373 0.09855748

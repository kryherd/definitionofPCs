### checks for: -- from Clint?
#univariate and multivariate outliers
#linearity
#independent observation
#homoscedasticity
#normal distribution of residual errors

library(ggplot2)
library(reshape2)
library(car)
library(caret)

#reading in & organizing data (md = model data)
md <- read.csv("~/Dropbox/Misc Analyses/Definition_of_PCs/definition_analysis_color.csv")
#remove extraneous column
md<-md[,-17]
names(md) <- c("Subj", "Age", "BD_Raw", "Matrix_Raw", "BD_SS","Matrix_SS","PIQ","WA_Raw","WA_SS",
               "LW_Raw", "LW_SS", "PPVT_raw","PPVT_SS", "KTEA_Raw", "KTEA_SS", "Group")
#taking away participants that don't have all values
## you can remove this if we end up imputing values
md2 <- md[complete.cases(md),]
#center predictor variables
pp_md <- preProcess(md2[,-c(1,14)], method = "center")
tf_md <- predict(pp_md, newdata=md2[,-c(1,14)])
tf_md <- data.frame(md2$Subj, md2$KTEA_Raw, tf_md)
colnames(tf_md)[1] <- "Subj"
colnames(tf_md)[2] <- "KTEA_Raw"
#model with LW, WA, PIQ, Age
m1 <- lm(data=tf_md, formula = KTEA_Raw~Age*LW_Raw*WA_Raw*PIQ)
#standardize predicted values
unstandardizedPredicted <- predict(m1)
standardizedPredicted <- (unstandardizedPredicted - mean(unstandardizedPredicted)) / sd(unstandardizedPredicted)
m2 <- lm(tf_md$KTEA_Raw~standardizedPredicted)
#find 1.5 SD below predicted value for UPC <- not clear how to do this, just went with 80% CI
ci <- predict(m1, level=.8, interval="confidence") 
# find within .5 SD of predicted value for EGC <- same, just went with 15% CI
ci2 <- predict(m1, level=.15, interval="confidence") 
#create CI dataframe
all_ci <- data.frame(tf_md$Subj, tf_md$KTEA_Raw, ci[,2], ci2[,2], ci2[,3])
names(all_ci) <- c("Subj","KTEA","eighty_lwr","fifteen_lwr","fifteen_upr")
all_ci[,"CI_Group"] <- NA
#classify
#anyone whose Raw KTEA value is below the 80% CI is an unexpected poor comprehender
all_ci[(all_ci$KTEA<all_ci$eighty_lwr), "CI_Group"] <- "UPC"
#anyone who is within 15% CI is an expected good comprehender
all_ci[(all_ci$KTEA<all_ci$fifteen_upr & all_ci$KTEA>all_ci$fifteen_lwr), "CI_Group"] <- "EGC"
#show how many are in each group
table(all_ci$CI_Group)


#model with LW, WA, PIQ, Age, WASI - Vocab
## I'll do this part once the above is verified
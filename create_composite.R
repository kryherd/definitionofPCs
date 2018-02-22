library(fBasics)
library(caret)
library(MASS)
library(ggplot2)
library(plyr)
library(reshape2)
setwd("~/Dropbox/Definition_of_PCs/RegressionDefinitionMethod/")
data <- read.csv("FullData_FinalMay23_2017.csv")
data <- data[data$handedness == "right",]
### STEP 1 - create composite.
composite.data <- data[, c(1, 10, 11, 16, 17)]
comp <- composite.data[complete.cases(composite.data),]
# remove subject with WA score of 0
comp$wj3.watt.raw[comp$wj3.watt.raw == 0] <- NA
comp <- comp[complete.cases(comp),]

#check distribution of composite parts
dagoTest(comp$wj3.watt.raw)
dagoTest(comp$wj3.wid.raw)
dagoTest(comp$towre.w.ipm)
dagoTest(comp$towre.nw.ipm)
#They're all significant, so let's center/scale/transform

#prepare to center, scale, and (if necessary) transform predictor variables
vars_tf <- c("wj3.watt.raw","wj3.wid.raw","towre.w.ipm", "towre.nw.ipm")
pp_md_tf <- preProcess(comp[,vars_tf], method = c("center", "scale", "YeoJohnson"), na.remove=T)

#check the lambda values associated with the predictors
pp_md_tf$yj$wj3.watt.raw$lambda
pp_md_tf$yj$wj3.wid.raw$lambda
pp_md_tf$yj$towre.w.ipm$lambda
pp_md_tf$yj$towre.nw.ipm$lambda

#execute centering, scaling, tranforming, etc.
tf_data <- predict(pp_md_tf, comp[,vars_tf])

#re-check distribution
dagoTest(tf_data$wj3.watt.raw)
dagoTest(tf_data$wj3.wid.raw)
dagoTest(tf_data$towre.w.ipm)
dagoTest(tf_data$towre.nw.ipm)

#create composite
tf_data$composite1 <- rowMeans(scale(tf_data[,c("wj3.watt.raw","wj3.wid.raw","towre.w.ipm", "towre.nw.ipm")]))

#put composite into data frame
comp$composite1 <- tf_data$composite1

#scale (but don't center!) composite
comp$composite1 <- scale(comp$composite1, center = FALSE)

# export composite values
comp.export <- comp[,-c(2:5)]
write.csv(comp.export, "decoding_composite.csv")

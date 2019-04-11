library(fBasics)
library(caret)
library(MASS)
library(ggplot2)
library(plyr)
library(reshape2)
setwd("~/Dropbox/Definition_of_PCs/RegressionDefinitionMethod/MultipleCompTests-Aug2017/RegressionMethod")
data <- read.csv("classification.csv")
### STEP 1 - create composite.
composite.data <- data[, c(1,  16, 18)]
comp <- composite.data[complete.cases(composite.data),]

#check distribution of composite parts
dagoTest(comp$ktea2.raw)
dagoTest(comp$wj3.rcomp.raw)

#They're all significant, so let's center/scale/transform

#prepare to center, scale, and (if necessary) transform predictor variables
vars_tf <- c("ktea2.raw","wj3.rcomp.raw")
pp_md_tf <- preProcess(comp[,vars_tf], method = c("center", "scale", "YeoJohnson"), na.remove=T)

#check the lambda values associated with the predictors
pp_md_tf$yj$ktea2.raw$lambda
pp_md_tf$yj$wj3.rcomp.raw$lambda

#execute centering, scaling, tranforming, etc.
tf_data <- predict(pp_md_tf, comp[,vars_tf])

#re-check distribution
dagoTest(tf_data$ktea2.raw)
dagoTest(tf_data$wj3.rcomp.raw)

#create composite
tf_data$rc.comp <- rowMeans(scale(tf_data[,c("ktea2.raw","wj3.rcomp.raw")]))

#put composite into data frame
comp$rc.comp <- tf_data$rc.comp

#scale (but don't center!) composite
comp$rc.comp <- scale(comp$rc.comp, center = FALSE)


# export composite values
comp.export <- comp[,-c(2,3)]
write.csv(comp.export, "comprehension_composite.csv")

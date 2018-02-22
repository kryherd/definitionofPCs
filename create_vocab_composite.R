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
composite.data <- data[, c(1, 6, 7)]
comp <- composite.data[complete.cases(composite.data),]

#check distribution of composite parts
dagoTest(comp$ppvt.raw)
dagoTest(comp$wasi.vocab.raw)
#PPVT is sig, wasi vocab is not
#but, preprocess get stressed out when I just try to transform one variable
# so I'll transform both but just use the raw wasi vocab

#prepare to center, scale, and (if necessary) transform predictor variables
vars_tf <- c("ppvt.raw","wasi.vocab.raw")
pp_md_tf <- preProcess(comp[,vars_tf], method = c("center", "scale", "YeoJohnson"), na.remove=T)

#check the lambda values associated with the predictors
pp_md_tf$yj$ppvt.raw$lambda
pp_md_tf$yj$wasi.vocab.raw$lambda


#execute centering, scaling, tranforming, etc.
tf_data <- predict(pp_md_tf, comp[,vars_tf])

#re-check distribution
dagoTest(tf_data$ppvt.raw)
# fixed skewness

# new data frame
comp$ppvt.tf <- tf_data$ppvt.raw

#create composite
comp$composite2 <- rowMeans(scale(comp[,c("ppvt.tf","wasi.vocab.raw")]))

#scale (but don't center!) composite
comp$composite2 <- scale(comp$composite2, center = FALSE)

# export composite values
comp.export <- comp[,-c(2:4)]
write.csv(comp.export, "vocab_composite.csv")

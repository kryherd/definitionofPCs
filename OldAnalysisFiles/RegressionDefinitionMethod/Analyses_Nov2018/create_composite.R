library(fBasics)
library(caret)
library(MASS)
library(reshape2)
library(tidyverse)
setwd("~/Dropbox/Definition_of_PCs/RegressionDefinitionMethod/Analyses_Nov2018")
data <- read.csv("FullData_Sept27_2018.csv")
### STEP 1 - create composite.
composite.data <- dplyr::select(data, SubjectID, wj3.wid.raw, wj3.watt.raw, towre.w.ipm, towre.nw.ipm)
comp <- composite.data[complete.cases(composite.data),]
# remove subject with WA score of 0
comp$wj3.watt.raw[comp$wj3.watt.raw == 0] <- NA
comp <- comp[complete.cases(comp),]

#check distribution of composite parts
for (i in 2:length(comp)){
  cat("\n-----------------\n",names(comp)[i],"\n")
  x <- dagoTest(comp[,i])
  omni <- x@test$p.value[1]
  skew <- x@test$p.value[2]
  cat("Omnibus p-val: ",omni,"\n\nSkewness p-val: ", skew, "\n")
}
#They're all significant, so let's center/scale/transform

#prepare to center, scale, and (if necessary) transform predictor variables
vars_tf <- names(comp[-1])
pp_md_tf <- preProcess(comp[,vars_tf], method = c("center", "scale", "YeoJohnson"), na.remove=T)

#check the lambda values associated with the predictors
for (i in 1:length(pp_md_tf$yj)){
  print(pp_md_tf$yj[i][1])
}

#execute centering, scaling, tranforming, etc.
tf_data <- predict(pp_md_tf, comp[,vars_tf])

#re-check distribution
for (i in 1:length(tf_data)){
  cat("\n-----------------\n",names(tf_data)[i],"\n")
  x <- dagoTest(tf_data[,i])
  omni <- x@test$p.value[1]
  skew <- x@test$p.value[2]
  cat("Omnibus p-val: ",omni,"\n\nSkewness p-val: ", skew, "\n")
}

#create composite
tf_data$decode_composite <- rowMeans(scale(tf_data[,vars_tf]))

#put composite into data frame
comp$decode_composite <- tf_data$decode_composite

#scale (but don't center!) composite
comp$decode_composite <- scale(comp$decode_composite, center = FALSE)

# export composite values
comp.export <- comp[,!names(comp) %in% vars_tf]
write.csv(comp.export, "decoding_composite.csv", row.names = FALSE)

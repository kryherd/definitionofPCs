library(fBasics)
## if your version of caret is later than this version, it won't work. So downgrade using the next two lines.
# require(devtools)
# install_version("caret", version = "6.0-76", repos = "http://cran.us.r-project.org")
library(caret)
library(MASS)
library(ggplot2)
library(plyr)
library(reshape2)
setwd("~/Dropbox/Definition_of_PCs/RegressionDefinitionMethod/ClassifierwithImputedData_July2018")
data <- read.csv("ImputedData_July2_2018.csv")
### STEP 1 - create composite.
composite.data <- data[, c(1, 4,5,6,7)]
comp <- composite.data[complete.cases(composite.data),]
# remove subject with WA score of 0
comp$wj3.watt.raw[comp$wj3.watt.raw == 0] <- NA
comp <- comp[complete.cases(comp),]

#check distribution of composite parts
#dagoTest(comp$wj3.watt.raw)
#dagoTest(comp$wj3.wid.raw)
#dagoTest(comp$towre.w.ipm)
#dagoTest(comp$towre.nw.ipm)
#They're all significant except TOWRE real workds, so let's center/scale/transform

#prepare to center, scale, and (if necessary) transform predictor variables
#vars_tf <- c("wj3.watt.raw", "towre.nw.ipm", "wj3.wid.raw")
#pp_md_tf <- preProcess(comp[,vars_tf], method = c("center", "scale", "YeoJohnson"), na.remove=T)

#check the lambda values associated with the predictors
#pp_md_tf$yj$wj3.watt.raw$lambda
#pp_md_tf$yj$wj3.wid.raw$lambda
# pp_md_tf$yj$towre.w.ipm$lambda
#pp_md_tf$yj$towre.nw.ipm$lambda

#execute centering, scaling, tranforming, etc.
#tf_data <- predict(pp_md_tf, comp[,vars_tf])

#re-check distribution
#dagoTest(tf_data$wj3.watt.raw)
#dagoTest(tf_data$wj3.wid.raw)
# dagoTest(tf_data$towre.w.ipm)
#dagoTest(tf_data$towre.nw.ipm)

# scale and center predictors
comp$wj3.watt.cs <- scale(comp$wj3.watt.raw)
comp$wj3.wid.cs <- scale(comp$wj3.wid.raw)
comp$towre.w.cs <- scale(comp$towre.w.ipm)
comp$towre.nw.cs <- scale(comp$towre.nw.ipm)

#create composite
comp$decoding_comp <- rowMeans(scale(comp[,c("wj3.watt.cs","wj3.wid.cs","towre.w.cs", "towre.nw.cs")]))

#put composite into data frame
#comp$composite1 <- tf_data$composite1

#scale (but don't center!) composite
comp$decoding_comp <- scale(comp$decoding_comp, center = FALSE)

# export composite values
comp.export <- comp[,-c(2:5)]
write.csv(comp.export, "decoding_composite.csv", row.names = FALSE)

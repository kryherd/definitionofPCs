# read in libraries, data
library(missForest)
library(fBasics)
library(dplyr)
library(micemd)
setwd("~/Dropbox/Definition_of_PCs/RegressionDefinitionMethod/ClassifierwithImputedData_July2018")
# this has all the raw data, no overlapping subjects
data <- read.csv("FullData_Sept27_2018.csv")
# look at missing data
data %>% summarize_all(funs(sum(is.na(.)) / length(.)))

# select columns for imputation

impute_data <- data %>%
  select(SubjectID, Project, 
         #age.mri, 
         age.beh, ppvt.raw, wasi.matr.raw, wj3.watt.raw,
         wj3.wid.raw, towre.w.ipm, towre.nw.ipm, ktea2.raw, 
         #gm.rcomp.raw,
         #nd.rcomp.raw,
         wj3.rcomp.raw)

############ TESTING IMPUTATION METHODS


cc_dat <- impute_data[complete.cases(impute_data),]
randsample <- sample_frac(cc_dat, size = .5)
cc_dat$wj3.rcomp.raw[cc_dat$SubjectID %in% randsample$SubjectID] <- NA
id_info <- cc_dat[,c(1:2)]

imputed_test_dat <- cbind(id_info, test_MF$ximp)
test_MF <- missForest(cc_dat[,-c(1,2)], xtrue = impute_data[,-c(1,2)])

imputed_compare <- imputed_test_dat[imputed_test_dat$SubjectID %in% randsample$SubjectID,]
imputed_compare <- imputed_compare[,c(1,11)]
colnames(imputed_compare)[2] <- "imputed_wj3_rcomp"
rand_wj3 <- randsample[,c(1,11)]
compare <- merge(imputed_compare, rand_wj3, by = "SubjectID")
colnames(compare)[3] <- "measured_wj3_rcomp"
compare$diff <- compare[,2] - compare[,3]
compare$absdiff <- abs(compare$diff)
summary(compare$diff)
summary(compare$absdiff)
hist(compare$diff)
hist(compare$absdiff)

##### MI using MICE & MICEMD
ind.clust <- 1
## use micemd to pick methods -- but some of the methods throw errors
# method <- find.defaultMethod(impute_data[,-1], 1)
temp <- mice(impute_data[,-1],m=1,maxit=0)
temp$pred[ind.clust,ind.clust] <-0
temp$pred[-ind.clust,ind.clust] <- -2
temp$pred[temp$pred==1] <- 2
predictor.matrix <- temp$pred
# using 2l.2stage.norm to nest data within projects/grants
res.mice <- mice(impute_data[,-1], predictorMatrix = predictor.matrix, method = "2l.2stage.norm")
completeData <- complete(res.mice, 3)

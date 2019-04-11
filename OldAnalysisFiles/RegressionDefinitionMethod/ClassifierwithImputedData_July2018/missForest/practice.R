# read in libraries, data
library(missForest)
library(fBasics)
library(dplyr)
setwd("~/Dropbox/Definition_of_PCs/RegressionDefinitionMethod/ClassifierwithImputedData_July2018/missForest")
# this has all the raw data, no overlapping subjects
data <- read.csv("All_IMPUTE_COMP.csv")

# look at missing data
data %>% summarize_all(funs(sum(is.na(.)) / length(.)))

# don't want to us GM or ND, maybe not use KTEA

# subset to just predictors and WJ3
dat_imp <- data[,-c(11:13)]

############ TESTING IMPUTATION ON OUR DATASET

cc_dat <- dat_imp[complete.cases(dat_imp),]
randsample <- sample_frac(cc_dat, size = .5)
cc_dat$wj3.rcomp.raw[cc_dat$SubjectID %in% randsample$SubjectID] <- NA
  
cc_dat %>% summarize_all(funs(sum(is.na(.)) / length(.)))

impute_test <- missForest(cc_dat[,-c(1,2)], xtrue = dat_imp[,-c(1,2)])
id_info <- cc_dat[,c(1:2)]
imputed_test_dat <- cbind(id_info, impute_test$ximp)

imputed_compare <- imputed_test_dat[imputed_test_dat$SubjectID %in% randsample$SubjectID,]
imputed_compare <- imputed_compare[,c(1,10)]
colnames(imputed_compare)[2] <- "imputed_wj3_rcomp"
rand_wj3 <- randsample[,c(1,10)]
compare <- merge(imputed_compare, rand_wj3, by = "SubjectID")
colnames(compare)[3] <- "measured_wj3_rcomp"
compare$diff <- compare[,2] - compare[,3]
compare$absdiff <- abs(compare$diff)
summary(compare$diff)
summary(compare$absdiff)
hist(compare$diff)
hist(compare$absdiff)


high_error <- compare[compare$absdiff > 5,]

high_error_data <- data[data$SubjectID %in% high_error$SubjectID,]

error_compare <- newList[[2]]$KTEA
error_compare_subs <- error_compare[error_compare$SubjectID %in% high_error$SubjectID,]

error_compare2 <- allList[[2]]$KTEA
error_compare_subs2 <- error_compare2[error_compare2$SubjectID %in% high_error$SubjectID,]

############# IMPUTING WITHOUT KTEA, ND, or GM
  
  
# flag all rows with 4 or more missing predictor columns (incl. WJ3)
  dat_imp$too_missing <- 9999
missing_labels <- function(data, i){
  missings <- table(is.na(data[i,]))
  if (missings[[1]] <= 6) {
    return(1)
  } else {
    return(0)
  } } 
for (i in 1:nrow(dat_imp)){
  dat_imp$too_missing[i] <- missing_labels(dat_imp, i)
}
# select relevant data
dat_to_impute <- dat_imp[dat_imp$too_missing == 0,]
id_info <- dat_to_impute[,c(1:2)]
raw_dat <- dat_to_impute[,c(3:10)]

# check normality
sink("normality_justWJ3_pre_impute.txt")
apply(raw_dat, 2,dagoTest)
sink()

# do 5 imputations with Out-of-bag (OOB) errors aggregated over entire data frame
imputeList <- list()
errors <- vector(mode = "numeric", length = 0)
for (i in (1:5)){
  imputed <- missForest(raw_dat)
  imputed_dat <- cbind(id_info, imputed$ximp)
  write.csv(imputed_dat, paste0("missForestimputed",i,".csv"))
  imputeList[[i]] <- imputed$ximp
  errors <- append(errors, as.numeric(unname(imputed$OOBerror)))
}
# print mean errors
paste("Mean OOB Error: ", mean(errors))
# check normality
for (i in 1:length(imputeList)){
  dat0 <- imputeList[[i]]
  sink(paste0("normality_justWJ3_post_imput",i,".txt"))
  print(apply(dat0, 2,dagoTest))
  sink()
}

# do 5 imputations with Out-of-bag (OOB) errors aggregated variable-wise
imputeList <- list()
errors <- vector(mode = "numeric", length = 0)
for (i in (1:5)){
  imputed <- missForest(raw_dat, variablewise=TRUE)
  imputed_dat <- cbind(id_info, imputed$ximp)
  write.csv(imputed_dat, paste0("missForestimputed_vw",i,".csv"))
  imputeList[[i]] <- imputed$ximp
  errors <- append(errors, as.numeric(unname(imputed$OOBerror)))
}
# print errors
errors.mat <- matrix(errors, ncol = 8, byrow=TRUE)
errors.df <- as.data.frame(errors.mat)
names(errors.df) <- names(raw_dat)
errors.df <- rbind(errors.df, colMeans(errors.df))
rownames(errors.df)[6] <- "Means"
errors.df
# check normality
for (i in 1:length(imputeList)){
  dat0 <- imputeList[[i]]
  sink(paste0("normality_justWJ3_post_vw_imput",i,".txt"))
  print(apply(dat0, 2,dagoTest))
  sink()
}

############# IMPUTING WITH KTEA
# select relevant data
dat_imp <- cbind(dat_imp,data$ktea2.raw)
dat_to_impute <- dat_imp[dat_imp$too_missing == 0,]
id_info <- dat_to_impute[,c(1:2)]
raw_dat <- dat_to_impute[,c(3:10,12)]
colnames(raw_dat)[9] <- "ktea2.raw"
# check normality
sink("normality_KTEA_pre_impute.txt")
apply(raw_dat, 2,dagoTest)
sink()
# do 5 imputations with Out-of-bag (OOB) errors aggregated over entire data frame
imputeList <- list()
errors <- vector(mode = "numeric", length = 0)
for (i in (1:5)){
  imputed <- missForest(raw_dat)
  imputed_dat <- cbind(id_info, imputed$ximp)
  write.csv(imputed_dat, paste0("missForestimputed_KTEA",i,".csv"))
  imputeList[[i]] <- imputed$ximp
  errors <- append(errors, as.numeric(unname(imputed$OOBerror)))
}
# print errors
paste("Mean OOB Error: ", mean(errors))
# check normality
for (i in 1:length(imputeList)){
  dat0 <- imputeList[[i]]
  sink(paste0("normality_KTEA_post_imput",i,".txt"))
  print(apply(dat0, 2,dagoTest))
  sink()
}

# do 5 imputations with Out-of-bag (OOB) errors aggregated variable-wise
imputeList <- list()
errors <- vector(mode = "numeric", length = 0)
for (i in (1:5)){
  imputed <- missForest(raw_dat, variablewise=TRUE)
  imputed_dat <- cbind(id_info, imputed$ximp)
  write.csv(imputed_dat, paste0("missForestimputed_vw_KTEA",i,".csv"))
  imputeList[[i]] <- imputed$ximp
  errors <- append(errors, as.numeric(unname(imputed$OOBerror)))
}
# print errors
errors.mat <- matrix(errors, ncol = 9, byrow=TRUE)
errors.df <- as.data.frame(errors.mat)
names(errors.df) <- names(raw_dat)
errors.df <- rbind(errors.df, colMeans(errors.df))
rownames(errors.df)[6] <- "Means"
errors.df
# check normality
for (i in 1:length(imputeList)){
  dat0 <- imputeList[[i]]
  sink(paste0("normality_KTEA_post_vw_imput",i,".txt"))
  print(apply(dat0, 2,dagoTest))
  sink()
}

############# IMPUTING WITH GM
# select relevant data
dat_imp <- cbind(dat_imp,data$gm.rcomp.raw)
dat_to_impute <- dat_imp[dat_imp$too_missing == 0,]
id_info <- dat_to_impute[,c(1:2)]
raw_dat <- dat_to_impute[,c(3:10,12)]
colnames(raw_dat)[9] <- "gm.rcomp.raw"
# check normality
sink("normality_GM_pre_impute.txt")
apply(raw_dat, 2,dagoTest)
sink()

# do 5 imputations with Out-of-bag (OOB) errors aggregated over entire data frame
imputeList <- list()
errors <- vector(mode = "numeric", length = 0)
for (i in (1:5)){
  imputed <- missForest(raw_dat)
  imputed_dat <- cbind(id_info, imputed$ximp)
  write.csv(imputed_dat, paste0("missForestimputed_GM",i,".csv"))
  imputeList[[i]] <- imputed$ximp
  errors <- append(errors, as.numeric(unname(imputed$OOBerror)))
}
# print errors
paste("Mean OOB Error: ", mean(errors))
# check normality
for (i in 1:length(imputeList)){
  dat0 <- imputeList[[i]]
  sink(paste0("normality_GM_post_imput",i,".txt"))
  print(apply(dat0, 2,dagoTest))
  sink()
}

# do 5 imputations with Out-of-bag (OOB) errors aggregated variable-wise
imputeList <- list()
errors <- vector(mode = "numeric", length = 0)
for (i in (1:5)){
  imputed <- missForest(raw_dat, variablewise=TRUE)
  imputed_dat <- cbind(id_info, imputed$ximp)
  write.csv(imputed_dat, paste0("missForestimputed_vw_GM",i,".csv"))
  imputeList[[i]] <- imputed$ximp
  errors <- append(errors, as.numeric(unname(imputed$OOBerror)))
}
# print errors
errors.mat <- matrix(errors, ncol = 9, byrow=TRUE)
errors.df <- as.data.frame(errors.mat)
names(errors.df) <- names(raw_dat)
errors.df <- rbind(errors.df, colMeans(errors.df))
rownames(errors.df)[6] <- "Means"
errors.df
# check normality
for (i in 1:length(imputeList)){
  dat0 <- imputeList[[i]]
  sink(paste0("normality_GM_post_vw_imput",i,".txt"))
  print(apply(dat0, 2,dagoTest))
  sink()
}


############# IMPUTING WITH ND
# select relevant data
dat_imp <- cbind(dat_imp,data$nd.rcomp.raw)
dat_to_impute <- dat_imp[dat_imp$too_missing == 0,]
id_info <- dat_to_impute[,c(1:2)]
raw_dat <- dat_to_impute[,c(3:10,12)]
colnames(raw_dat)[9] <- "nd.rcomp.raw"
# check normality
sink("normality_ND_pre_impute.txt")
apply(raw_dat, 2,dagoTest)
sink()

# do 5 imputations with Out-of-bag (OOB) errors aggregated over entire data frame
imputeList <- list()
errors <- vector(mode = "numeric", length = 0)
for (i in (1:5)){
  imputed <- missForest(raw_dat)
  imputed_dat <- cbind(id_info, imputed$ximp)
  write.csv(imputed_dat, paste0("missForestimputed_ND",i,".csv"))
  imputeList[[i]] <- imputed$ximp
  errors <- append(errors, as.numeric(unname(imputed$OOBerror)))
}
# print errors
paste("Mean OOB Error: ", mean(errors))
# check normality
for (i in 1:length(imputeList)){
  dat0 <- imputeList[[i]]
  sink(paste0("normality_ND_post_imput",i,".txt"))
  print(apply(dat0, 2,dagoTest))
  sink()
}

# do 5 imputations with Out-of-bag (OOB) errors aggregated variable-wise
imputeList <- list()
errors <- vector(mode = "numeric", length = 0)
for (i in (1:5)){
  imputed <- missForest(raw_dat, variablewise=TRUE)
  imputed_dat <- cbind(id_info, imputed$ximp)
  write.csv(imputed_dat, paste0("missForestimputed_vw_GM",i,".csv"))
  imputeList[[i]] <- imputed$ximp
  errors <- append(errors, as.numeric(unname(imputed$OOBerror)))
}
# print errors
errors.mat <- matrix(errors, ncol = 9, byrow=TRUE)
errors.df <- as.data.frame(errors.mat)
names(errors.df) <- names(raw_dat)
errors.df <- rbind(errors.df, colMeans(errors.df))
rownames(errors.df)[6] <- "Means"
errors.df
# check normality
for (i in 1:length(imputeList)){
  dat0 <- imputeList[[i]]
  sink(paste0("normality_ND_post_vw_imput",i,".txt"))
  print(apply(dat0, 2,dagoTest))
  sink()
}


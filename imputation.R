# read in libraries, data
library(missForest)
library(fBasics)
setwd("~/Dropbox/Definition_of_PCs/RegressionDefinitionMethod/missForest")
# this has all the raw data, no overlapping subjects
data <- read.csv("All_IMPUTE_COMP.csv")

# subset to just predictors and WJ3
dat_imp <- data[,-c(11:13)]

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

############# IMPUTING WITHOUT KTEA, ND, or GM
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


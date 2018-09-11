# read in libraries, data
library(missForest)
library(mice)
library(fBasics)
library(dplyr)
library(ggplot2)
setwd("~/definitionofPCs")
# this has all the raw data, no overlapping subjects
data <- read.csv("All_IMPUTE_COMP.csv")
md.pattern(data)

# look at missing data
data %>% summarize_all(funs(sum(is.na(.)) / length(.)))

# subset to just predictors and WJ3
dat_imp <- data[,-c(11:13)]



############# Comparing mice to missForest

## full dataset -- how similar are the two imputed datasets?

missForest <- missForest(dat_imp[,-c(1,2)], maxiter = 20)
mice <- mice(dat_imp[,-c(1,2)], m = 10, method = "pmm", printFlag = FALSE, maxit = 20)
mice_complete <- complete(mice)

compare <- data.frame(data$SubjectID)
compare$MF_wj3.rcomp.raw <- missForest$ximp$wj3.rcomp.raw
compare$M_wj3.rcomp.raw <- mice_complete$wj3.rcomp.raw
compare$real <- dat_imp$wj3.rcomp.raw

compare_missings <- compare[!complete.cases(compare$real),]

cor.test(compare_missings$MF_wj3.rcomp.raw, compare_missings$M_wj3.rcomp.raw)
ggplot(compare_missings, aes(MF_wj3.rcomp.raw, M_wj3.rcomp.raw)) + geom_point() + theme_bw() +
  labs(title = "Comparison of mice and missForest",
       x = "missForest WJ-III Reading Comprehension",
       y = "mice WJ-III Reading Comprehension") + annotate("text", x = 30, y = 42, label = "r = 0.6457")

## dropping half the dataset

# make complete-cases dataset
cc_dat <- dat_imp[complete.cases(dat_imp),]
randsample <- sample_frac(cc_dat, size = .5)
cc_dat$wj3.rcomp.raw[cc_dat$SubjectID %in% randsample$SubjectID] <- NA

# is half of rcomp missing?
cc_dat %>% summarize_all(funs(sum(is.na(.)) / length(.)))

missForest_imputeTest <- missForest(cc_dat[,-c(1,2)], xtrue = dat_imp[,-c(1,2)], maxiter = 20)
mice_imputeTest <- mice(cc_dat[,-c(1,2)], m = 10, method = "pmm", printFlag = FALSE, maxit = 20)
mice_imputeTest_data <- complete(mice_imputeTest)

compare_test <- data.frame(cc_dat$SubjectID, missForest_imputeTest$ximp$wj3.rcomp.raw, mice_imputeTest_data$wj3.rcomp.raw)
names(compare_test) <- c("SubjectID", "missForest_wj3.rcomp.raw", "mice_wj3.rcomp.raw")
compare_test <- merge(compare_test, dat_imp[,c(1,10)], by = "SubjectID")

compare_test$missForest_error <- compare_test$missForest_wj3.rcomp.raw - compare_test$wj3.rcomp.raw
compare_test$mice_error <- compare_test$mice_wj3.rcomp.raw - compare_test$wj3.rcomp.raw

sum(abs(compare_test$missForest_error))
sum(abs(compare_test$mice_error))

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
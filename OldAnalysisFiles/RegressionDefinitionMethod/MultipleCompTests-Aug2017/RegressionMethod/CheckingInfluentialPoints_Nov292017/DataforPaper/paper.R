library(xlsx)
library(phia)
library(multcomp)
library(fBasics)
library(caret)
library(MASS)
library(ggplot2)
library(plyr)
library(reshape2)
library(xtable)

dat1 <- read.csv("FullData_Dec5_2017.csv")
groups <- read.csv("MRIGroups_WJ3NoInt15_70.csv")
groups2 <- read.csv("WJ3NoImputeNoInt-15_70.csv")
dat2 <- merge(dat1, groups, by ="SubjectID")
dat3 <- merge(dat1, groups2, by = "SubjectID")


summ_pap <- function(x){
  a <- mean(x)
  b <- sd(x)
  c <- min(x)
  d <- max(x)
  cat(paste("Mean: ",round(a,2)),"\n")
  cat(paste("SD: ",round(b,2)),"\n")
  cat(paste("Range: ",c,"-",d))
}

summ_pap(dat1$age.mri)
table(dat1$gender)

dagoTest(dat1$wasi.matr.raw)
dagoTest(dat1$ppvt.raw)
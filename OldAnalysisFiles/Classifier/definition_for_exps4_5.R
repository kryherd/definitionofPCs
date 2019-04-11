library(ggplot2)
library(reshape2)
library(car)
library(caret)
library(fBasics)
library(plyr)
library(dplyr)

#reading in & organizing data (md = model data)
md <- read.csv("df.data.nowmc.KR.csv")

#taking away participants that don't have all values
## you can remove this if we end up imputing values
md2 <- md[complete.cases(md),]

#read in subjects for exps4&5
exp45 <- read.csv("4_5_subs.csv")

## All 3 cohorts (a40-4, a197, a182)
vars_ntf <- c("wasi.vocab.raw", "wj3.watt.raw","wj3.wid.raw","wasi.matr.raw")
#since nothing needs to be transformed, took away YJ
pp_md_ntf <- preProcess(md2[,vars_ntf], method = c("center", "scale"), na.remove=T)
#execute centering, scaling, tranforming, etc.
ntf_md <- predict(pp_md_ntf, md2[,vars_ntf])

vars_ntf2 <- c("age", "subject")
#since nothing needs to be transformed, took away YJ
pp_md_ntf2 <- preProcess(md2[,vars_ntf2], method = "center", na.remove=T)
#execute centering, scaling, tranforming, etc.
ntf_md2 <- predict(pp_md_ntf2, md2[,vars_ntf2])

#new data frame
sct_data <- data.frame(md2$subject, md2$cohort, md2$wj3.rcomp.ss, ntf_md, ntf_md2$age)
colnames(sct_data)[1] <- "subject"
colnames(sct_data)[2] <- "cohort"
colnames(sct_data)[3] <- "wj3.rcomp.ss"
colnames(sct_data)[8] <- "age"

#model with LW, WA, PIQ, Age
m1 <- lm(wj3.rcomp.ss~age+wj3.watt.raw+wj3.wid.raw+wasi.matr.raw+wasi.vocab.raw, data=sct_data)
#standardize predicted values
sct_data$Pred <- predict(m1)
sct_data$stdPred <- scale(sct_data$Pred)

# obtain the residuals
sct_data$resid <- resid <- residuals(m1)
qqnorm(scale(resid)); qqline(scale(resid))

##function to convert CIs to z-values
ci2z <- function(ci)
{
  qnorm(ci + (1 - ci)/2)
}

ci.to.use <- c(.15, .65)
#create the range of SD (z value) bins 
my.bin <- c(range(scale(resid))+.1, ci2z(ci.to.use), -ci2z(ci.to.use))
#we could also manually specify in terms of SDs
# my.bin <- c(-3, -1, -.25, .25, 1, 3)
my.bin <- sort(my.bin)
# names for each bin specified above
grouping <- c("UPC", "NSC", "EAC", "NSC", "UGC")

sct_data$CI_group <- grouping[findInterval(scale(resid), vec=my.bin, all.inside=T)]
#those with Standardized Predicted Value below -1 should be "NSC"
sct_data$CI_group[tf_md$stdPred < -1] <- "NSC"

table(sct_data$CI_group)
tapply(md2$age, sct_data$CI_group, mean)

# functions to get the ggplot default colors, 
# so I can match the colors for dots and lines
gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}

### there are four groups c("EAC", "NSC", "UGC", "UPC")
### so the argument n = 4
my.col <- gg_color_hue(n=4)

## unscale the z value for each CI, so I can add that value to intercept
unscale.ci <- my.bin[-c(1,length(my.bin))]*sd(resid)+mean(resid)

##
m2 <- lm(wj3.rcomp.ss ~ stdPred, data=sct_data)

p1 <- ggplot(sct_data, aes(x=stdPred, y=wj3.rcomp.ss)) + geom_point(aes(color=CI_group)) + 
  geom_abline(intercept = coef(m2)[1], slope = coef(m2)[2]) +
  geom_abline(intercept = coef(m2)[1] + unscale.ci[2], slope = coef(m2)[2], color=my.col[1]) +
  geom_abline(intercept = coef(m2)[1] + unscale.ci[3], slope = coef(m2)[2], color=my.col[1]) +
  geom_abline(intercept = coef(m2)[1] + unscale.ci[1], slope = coef(m2)[2], color=my.col[4]) +
  geom_abline(intercept = coef(m2)[1] + unscale.ci[4], slope = coef(m2)[2], color=my.col[3]) +
  geom_vline(xintercept = -1, lty="dotted") + 
  xlab("Standardized Predicted Value")

print(p1)

## same thing, but with only a40-4 data

md.aforty <- md[md$cohort=="a40",]

vars_ntf <- c("wasi.vocab.raw", "wj3.watt.raw","wj3.wid.raw","wasi.matr.raw")
#since nothing needs to be transformed, took away YJ
pp_md_ntf <- preProcess(md.aforty[,vars_ntf], method = c("center", "scale"), na.remove=T)
#execute centering, scaling, tranforming, etc.
ntf_md <- predict(pp_md_ntf, md.aforty[,vars_ntf])

vars_ntf2 <- c("age", "subject")
#since nothing needs to be transformed, took away YJ
pp_md_ntf2 <- preProcess(md.aforty[,vars_ntf2], method = "center", na.remove=T)
#execute centering, scaling, tranforming, etc.
ntf_md2 <- predict(pp_md_ntf2, md.aforty[,vars_ntf2])

#new data frame
sct_data.aforty <- data.frame(md.aforty$subject, md.aforty$cohort, md.aforty$wj3.rcomp.ss, ntf_md, ntf_md2$age)
colnames(sct_data.aforty)[1] <- "subject"
colnames(sct_data.aforty)[2] <- "cohort"
colnames(sct_data.aforty)[3] <- "wj3.rcomp.ss"
colnames(sct_data.aforty)[8] <- "age"

#model with LW, WA, PIQ, Age
m2 <- lm(wj3.rcomp.ss~age+wj3.watt.raw+wj3.wid.raw+wasi.matr.raw+wasi.vocab.raw, data=sct_data.aforty)
#standardize predicted values
sct_data.aforty$Pred <- predict(m2)
sct_data.aforty$stdPred <- scale(sct_data.aforty$Pred)

## same thing, but with only a40-4, relevant exps data

md.exps <- merge(exp45, md, by.x = "Subj", by.y = "subject")

vars_ntf <- c("wasi.vocab.raw", "wj3.watt.raw","wj3.wid.raw","wasi.matr.raw")
#since nothing needs to be transformed, took away YJ
pp_md_ntf <- preProcess(md.exps[,vars_ntf], method = c("center", "scale"), na.remove=T)
#execute centering, scaling, tranforming, etc.
ntf_md <- predict(pp_md_ntf, md.exps[,vars_ntf])

vars_ntf2 <- c("age", "Subj")
#since nothing needs to be transformed, took away YJ
pp_md_ntf2 <- preProcess(md.exps[,vars_ntf2], method = "center", na.remove=T)
#execute centering, scaling, tranforming, etc.
ntf_md2 <- predict(pp_md_ntf2, md.exps[,vars_ntf2])

#new data frame
sct_data.exps <- data.frame(md.exps$Subj, md.exps$cohort, md.exps$wj3.rcomp.ss, ntf_md, ntf_md2$age)
colnames(sct_data.exps)[1] <- "subject"
colnames(sct_data.exps)[2] <- "cohort"
colnames(sct_data.exps)[3] <- "wj3.rcomp.ss"
colnames(sct_data.exps)[8] <- "age"

#model with LW, WA, PIQ, Age
m3 <- lm(wj3.rcomp.ss~age+wj3.watt.raw+wj3.wid.raw+wasi.matr.raw+wasi.vocab.raw, data=sct_data.exps)
#standardize predicted values
sct_data.exps$Pred <- predict(m3)
sct_data.exps$stdPred <- scale(sct_data.exps$Pred)

## create data frame with residuals

merge1 <- merge(sct_data, sct_data.aforty, by = "subject")
merge2 <- merge(merge1, sct_data.exps, by = "subject")

class.resid <- data.frame(merge2$subject, merge2$stdPred.x, merge2$stdPred.y, merge2$stdPred)
names(class.resid) <- c("Subj", "ThreeCohortResid", "A40Resid", "Exps4.5Resid")
class.resid2 <- merge(class.resid, exp45, by="Subj", all=TRUE)


write.csv(class.resid2,"classifer_residuals.csv")



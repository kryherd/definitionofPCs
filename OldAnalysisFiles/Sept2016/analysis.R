library(fBasics)
library(caret)
library(MASS)
library(ggplot2)
library(plyr)
library(reshape2)
setwd("~/Dropbox/Definition_of_PCs/Sept2016")
data <- read.csv("FullExport_NoOverlap_9-29-16.csv")
comp <- read.csv("decoding_composite.csv")
comp <- comp[,-1]
data <- merge(data, comp, by = "SubjectID")

## USER-DEFINED FUNCTIONS
gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}

ci2z <- function(ci)
{
  qnorm(ci + (1 - ci)/2)
}

## Change this value to change CIs for all analyses
ci.to.use <- c(.15, .65)
ci.title <- "CIs: 15%, 65%"
###### MODEL WITHOUT VOCABULARY

model.data.nv <- data[, c(1, 2, 7, 11, 18)]
nv.cc <- model.data.nv[complete.cases(model.data.nv),]

### Centering/scaling predictors

#just centering
nv.cc$age.c <- nv.cc$age - mean(nv.cc$age, na.rm = TRUE)
#centering and scaling
nv.cc$wj3.rcomp.cs <- scale(nv.cc$wj3.rcomp.raw)
nv.cc$wasi.matr.cs <- scale(nv.cc$wasi.matr.raw)

### Run regression
m1 <- rlm(wj3.rcomp.cs ~ age.c + composite1 + wasi.matr.cs, data = nv.cc, method = "MM")

#standardize predicted values
nv.cc$Pred <- predict(m1)
nv.cc$stdPred <- scale(nv.cc$Pred)

#obtained residuals
nv.cc$resid <- resid <- residuals(m1)
qqnorm(scale(resid)); qqline(scale(resid))

#create the range of SD (z value) bins 
my.bin <- c(range(scale(resid))+.1, ci2z(ci.to.use), -ci2z(ci.to.use))
#we could also manually specify in terms of SDs
# my.bin <- c(-3, -1, -.25, .25, 1, 3)
my.bin <- sort(my.bin)
# names for each bin specified above
grouping <- c("UPC", "NSC", "EAC", "NSC", "UGC")

nv.cc$CI_group <- grouping[findInterval(scale(resid), vec=my.bin, all.inside=T)]
#those with Standardized Predicted Value below -1 should be "EPC"
nv.cc$CI_group[nv.cc$stdPred < -1] <- "EPC"

### there are five groups c("EAC", "NSC", "UGC", "UPC", "EPC")
### so the argument n = 5
my.col <- gg_color_hue(n=5)

## unscale the z value for each CI, so I can add that value to intercept
unscale.ci <- my.bin[-c(1,length(my.bin))]*sd(resid)+mean(resid)

##
m2 <- lm(wj3.rcomp.cs ~ stdPred, data=nv.cc)

p1 <- ggplot(nv.cc, aes(x=stdPred, y=wj3.rcomp.cs)) + geom_point(aes(color=CI_group)) + 
  geom_abline(intercept = coef(m2)[1], slope = coef(m2)[2]) +
  geom_abline(intercept = coef(m2)[1] + unscale.ci[2], slope = coef(m2)[2], color=my.col[1]) +
  geom_abline(intercept = coef(m2)[1] + unscale.ci[3], slope = coef(m2)[2], color=my.col[1]) +
  geom_abline(intercept = coef(m2)[1] + unscale.ci[1], slope = coef(m2)[2], color=my.col[4]) +
  geom_abline(intercept = coef(m2)[1] + unscale.ci[4], slope = coef(m2)[2], color=my.col[3]) +
  geom_vline(xintercept = -1, lty="dotted") + 
  xlab("Standardized Predicted Value")+ylab("WJ3 Reading Comprehension") +
  ggtitle(ci.title)

print(p1)
table(nv.cc$CI_group)

nv.cc2 <- subset(nv.cc, CI_group != "NSC")
nv.cc2$CI_group <- as.factor(nv.cc2$CI_group)
# reading comp
a1 <- aov(wj3.rcomp.cs ~ CI_group, data = nv.cc2)
summary(a1)
pairwise.t.test(nv.cc2$wj3.rcomp.cs, nv.cc2$CI_group, p.adjust.method = "bonferroni")
aggregate(nv.cc2$wj3.rcomp.cs, list(nv.cc2$CI_group), mean)
# age
a1 <- aov(age.c ~ CI_group, data = nv.cc2)
summary(a1)
pairwise.t.test(nv.cc2$age.c, nv.cc2$CI_group, p.adjust.method = "bonferroni")
aggregate(nv.cc2$age.c, list(nv.cc2$CI_group), mean)
# matrices
a1 <- aov(wasi.matr.cs ~ CI_group, data = nv.cc2)
summary(a1)
pairwise.t.test(nv.cc2$wasi.matr.cs, nv.cc2$CI_group, p.adjust.method = "bonferroni")
aggregate(nv.cc2$wasi.matr.cs, list(nv.cc2$CI_group), mean)
# decoding
a1 <- aov(composite1 ~ CI_group, data = nv.cc2)
summary(a1)
pairwise.t.test(nv.cc2$composite1, nv.cc2$CI_group, p.adjust.method = "bonferroni")
aggregate(nv.cc2$composite1, list(nv.cc2$CI_group), mean)
###### Model with Vocab

# with vocab complete cases data set
model.data.v <- data[, c(1, 2, 6, 7, 9, 10, 11, 15, 16)]
v.cc <- model.data.v[complete.cases(model.data.v),]

#adding composite to this data set
just.composite <- nv.cc[,c(1, 9)]
v.cc2 <- merge(just.composite, v.cc, by = "SubjectID")

# centering and scaling
v.cc2$age.c <- v.cc2$age - mean(v.cc2$age, na.rm = TRUE)
v.cc2$wj3.rcomp.cs <- scale(v.cc2$wj3.rcomp.raw)
v.cc2$wasi.matr.cs <- scale(v.cc2$wasi.matr.raw)
v.cc2$wasi.vocab.cs <- scale(v.cc2$wasi.vocab.raw)

### Run regressions
m1 <- rlm(wj3.rcomp.cs ~ age.c + composite1 + wasi.matr.cs + wasi.vocab.cs, data = v.cc2, method = "MM")

#standardize predicted values
v.cc2$Pred <- predict(m1)
v.cc2$stdPred <- scale(v.cc2$Pred)

#obtained residuals
v.cc2$resid <- resid <- residuals(m1)
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

v.cc2$CI_group <- grouping[findInterval(scale(resid), vec=my.bin, all.inside=T)]
#those with Standardized Predicted Value below -1 should be "NSC"
v.cc2$CI_group[v.cc$stdPred < -1] <- "NSC"

table(v.cc2$CI_group)


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
m2 <- lm(wj3.rcomp.cs ~ stdPred, data=v.cc2)

p1 <- ggplot(v.cc2, aes(x=stdPred, y=wj3.rcomp.cs)) + geom_point(aes(color=CI_group)) + 
  geom_abline(intercept = coef(m2)[1], slope = coef(m2)[2]) +
  geom_abline(intercept = coef(m2)[1] + unscale.ci[2], slope = coef(m2)[2], color=my.col[1]) +
  geom_abline(intercept = coef(m2)[1] + unscale.ci[3], slope = coef(m2)[2], color=my.col[1]) +
  geom_abline(intercept = coef(m2)[1] + unscale.ci[1], slope = coef(m2)[2], color=my.col[4]) +
  geom_abline(intercept = coef(m2)[1] + unscale.ci[4], slope = coef(m2)[2], color=my.col[3]) +
  geom_vline(xintercept = -1, lty="dotted") + 
  xlab("Standardized Predicted Value")+ylab("WJ3 Reading Comprehension") +
  ggtitle("CIs: 15%; 65%")

print(p1)


v.cc3 <- subset(v.cc2, CI_group != "NSC")
v.cc3$CI_group <- as.factor(v.cc3$CI_group)
# reading comp
a1 <- aov(wj3.rcomp.cs ~ CI_group, data = v.cc3)
summary(a1)
pairwise.t.test(v.cc3$wj3.rcomp.cs, v.cc3$CI_group, p.adjust.method = "bonferroni")
aggregate(v.cc3$wj3.rcomp.cs, list(v.cc3$CI_group), mean)
# age
a1 <- aov(age.c ~ CI_group, data = v.cc3)
summary(a1)
pairwise.t.test(v.cc3$age.c, v.cc3$CI_group, p.adjust.method = "bonferroni")
aggregate(v.cc3$age.c, list(v.cc3$CI_group), mean)
# matrices
a1 <- aov(wasi.matr.cs ~ CI_group, data = v.cc3)
summary(a1)
pairwise.t.test(v.cc3$wasi.matr.cs, v.cc3$CI_group, p.adjust.method = "bonferroni")
aggregate(v.cc3$wasi.matr.cs, list(v.cc3$CI_group), mean)
# decoding
a1 <- aov(composite1 ~ CI_group, data = v.cc3)
summary(a1)
pairwise.t.test(v.cc3$composite1, v.cc3$CI_group, p.adjust.method = "bonferroni")
aggregate(v.cc3$composite1, list(v.cc3$CI_group), mean)
# vocab
a1 <- aov(wasi.vocab.cs ~ CI_group, data = v.cc3)
summary(a1)
pairwise.t.test(v.cc3$wasi.vocab.cs, v.cc3$CI_group, p.adjust.method = "bonferroni")
aggregate(v.cc3$wasi.vocab.cs, list(v.cc3$CI_group), mean)

###### Model with OralComp

# with vocab complete cases data set
model.data.oc <- data[, c(1, 2, 7, 9, 10, 11, 12, 15, 16)]
oc.cc <- model.data.oc[complete.cases(model.data.oc),]

#adding composite to this data set
just.composite <- nv.cc[,c(1, 9)]
oc.cc2 <- merge(just.composite, oc.cc, by = "SubjectID")

# centering and scaling
oc.cc2$age.c <- oc.cc2$age - mean(oc.cc2$age, na.rm = TRUE)
oc.cc2$wj3.rcomp.cs <- scale(oc.cc2$wj3.rcomp.raw)
oc.cc2$wasi.matr.cs <- scale(oc.cc2$wasi.matr.raw)
oc.cc2$wj3.oralcomp.cs <- scale(oc.cc2$wj3.oralcomp)

### Run regressions
m1 <- rlm(wj3.rcomp.cs ~ age.c + composite1 + wasi.matr.cs + wj3.oralcomp.cs, data = oc.cc2, method = "MM")

#standardize predicted values
oc.cc2$Pred <- predict(m1)
oc.cc2$stdPred <- scale(oc.cc2$Pred)

#obtained residuals
oc.cc2$resid <- resid <- residuals(m1)
qqnorm(scale(resid)); qqline(scale(resid))

##function to convert CIs to z-values
ci2z <- function(ci)
{
  qnorm(ci + (1 - ci)/2)
}

ci.to.use <- c(.15, .70)
#create the range of SD (z value) bins 
my.bin <- c(range(scale(resid))+.1, ci2z(ci.to.use), -ci2z(ci.to.use))
#we could also manually specify in terms of SDs
# my.bin <- c(-3, -1, -.25, .25, 1, 3)
my.bin <- sort(my.bin)
# names for each bin specified above
grouping <- c("UPC", "NSC", "EAC", "NSC", "UGC")

oc.cc2$CI_group <- grouping[findInterval(scale(resid), vec=my.bin, all.inside=T)]
#those with Standardized Predicted Value below -1 should be "NSC"
oc.cc2$CI_group[oc.cc$stdPred < -1] <- "NSC"

table(oc.cc2$CI_group)


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
m2 <- lm(wj3.rcomp.cs ~ stdPred, data=oc.cc2)

p1 <- ggplot(oc.cc2, aes(x=stdPred, y=wj3.rcomp.cs)) + geom_point(aes(color=CI_group)) + 
  geom_abline(intercept = coef(m2)[1], slope = coef(m2)[2]) +
  geom_abline(intercept = coef(m2)[1] + unscale.ci[2], slope = coef(m2)[2], color=my.col[1]) +
  geom_abline(intercept = coef(m2)[1] + unscale.ci[3], slope = coef(m2)[2], color=my.col[1]) +
  geom_abline(intercept = coef(m2)[1] + unscale.ci[1], slope = coef(m2)[2], color=my.col[4]) +
  geom_abline(intercept = coef(m2)[1] + unscale.ci[4], slope = coef(m2)[2], color=my.col[3]) +
  geom_vline(xintercept = -1, lty="dotted") + 
  xlab("Standardized Predicted Value")+ylab("WJ3 Reading Comprehension") +
  ggtitle("CIs: 15%; 70%")

print(p1)


oc.cc3 <- subset(oc.cc2, CI_group != "NSC")
oc.cc3$CI_group <- as.factor(oc.cc3$CI_group)
# reading comp
a1 <- aov(wj3.rcomp.cs ~ CI_group, data = oc.cc3)
summary(a1)
pairwise.t.test(oc.cc3$wj3.rcomp.cs, oc.cc3$CI_group, p.adjust.method = "bonferroni")
aggregate(oc.cc3$wj3.rcomp.cs, list(oc.cc3$CI_group), mean)
# age
a1 <- aov(age.c ~ CI_group, data = oc.cc3)
summary(a1)
pairwise.t.test(oc.cc3$age.c, oc.cc3$CI_group, p.adjust.method = "bonferroni")
aggregate(oc.cc3$age.c, list(oc.cc3$CI_group), mean)
# matrices
a1 <- aov(wasi.matr.cs ~ CI_group, data = oc.cc3)
summary(a1)
pairwise.t.test(oc.cc3$wasi.matr.cs, oc.cc3$CI_group, p.adjust.method = "bonferroni")
aggregate(oc.cc3$wasi.matr.cs, list(oc.cc3$CI_group), mean)
# decoding
a1 <- aov(composite1 ~ CI_group, data = oc.cc3)
summary(a1)
pairwise.t.test(oc.cc3$composite1, oc.cc3$CI_group, p.adjust.method = "bonferroni")
aggregate(oc.cc3$composite1, list(oc.cc3$CI_group), mean)
# oral comp
a1 <- aov(wj3.oralcomp.cs ~ CI_group, data = oc.cc3)
summary(a1)
pairwise.t.test(oc.cc3$wj3.oralcomp.cs, oc.cc3$CI_group, p.adjust.method = "bonferroni")
aggregate(oc.cc3$wj3.oralcomp.cs, list(oc.cc3$CI_group), mean)

dagoTest(just.composite$composite1)


## looking at vocab of groups
vocab.barplot <- data.frame(v.cc3$SubjectID, v.cc3$wasi.vocab.raw, v.cc3$CI_group)
names(vocab.barplot) <- c("subj", "vocab", "group")

melt <- melt(vocab.barplot, id.vars=c("subj", "group"))
means <- ddply(melt, c("group", "variable"), summarise,
               mean=mean(value))
means.sem <- ddply(melt, c("group", "variable"), summarise,
                   mean=mean(value), sem=sd(value)/sqrt(length(value)))
means.sem <- transform(means.sem, lower=mean-sem, upper=mean+sem)


p <- ggplot(data = means.sem, aes(x = group, y = mean))

p + geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(ymax = means.sem$upper, ymin = means.sem$lower, position = "dodge", width = 0.25) + theme_bw()


# export file with residuals
residuals <- data.frame(tot.dat$subject, tot.dat$resid)
names(residuals) <- c("Subject", "Residual")

write.csv(residuals, "NoVocab.csv")




######## PPVT

model.data.pm <- data[, c(1, 2, 5, 7, 9, 10, 11, 15, 16)]
pm.cc <- model.data.pm[complete.cases(model.data.pm),]

## Create Decoding Composite

#check distribution of composite parts
dagoTest(pm.cc$wj3.watt.raw)
dagoTest(pm.cc$wj3.wid.raw)
dagoTest(pm.cc$towre.w.ipm)
dagoTest(pm.cc$towre.nw.ipm)
#They're all significant, so let's center/scale/transform

#prepare to center, scale, and (if necessary) transform predictor variables
vars_tf <- c("wj3.watt.raw","wj3.wid.raw","towre.w.ipm", "towre.nw.ipm")
pp_md_tf <- preProcess(pm.cc[,vars_tf], method = c("center", "scale", "YeoJohnson"), na.remove=T)

#check the lambda values associated with the predictors
pp_md_tf$yj$wj3.watt.raw$lambda
pp_md_tf$yj$wj3.wid.raw$lambda
pp_md_tf$yj$towre.w.ipm$lambda
pp_md_tf$yj$towre.nw.ipm$lambda
#If lambda > 4, then look at the density plots and the raw data - there may be outliers driving the large number. If so, remove outliers and preProcess again.
# watt and wid have lambdas above 4
x <- density(pm.cc$wj3.watt.raw, bw="SJ", na.rm=T)
plot(x)
y <- density(pm.cc$wj3.wid.raw, bw="SJ", na.rm=T)
plot(y)

# One subj with WA score of 0, but no obvious outlier in LW
data$wj3.watt.raw[data$wj3.watt.raw == 0] <- NA


# Re-do above with WA outlier removed
vars_tf <- c("wj3.watt.raw","wj3.wid.raw","towre.w.ipm", "towre.nw.ipm")
pp_md_tf <- preProcess(data[,vars_tf], method = c("center", "scale", "YeoJohnson"), na.remove=T)

#check the lambda values associated with the predictors
pp_md_tf$yj$wj3.watt.raw$lambda
pp_md_tf$yj$wj3.wid.raw$lambda
pp_md_tf$yj$towre.w.ipm$lambda
pp_md_tf$yj$towre.nw.ipm$lambda

# WA lambda just went up...

#execute centering, scaling, tranforming, etc.
tf_data <- predict(pp_md_tf, pm.cc[,vars_tf])

tf_data$composite1 <- rowMeans(scale(tf_data[,c("wj3.watt.raw","wj3.wid.raw","towre.w.ipm", "towre.nw.ipm")]))

pm.cc$composite1 <- tf_data$composite1

pm.cc$composite1 <- scale(pm.cc$composite1)

## Centering/scaling

pm.cc$age.c <- pm.cc$age - mean(pm.cc$age, na.rm = TRUE)
pm.cc$wj3.rcomp.cs <- scale(pm.cc$wj3.rcomp.raw)
pm.cc$wasi.matr.cs <- scale(pm.cc$wasi.matr.raw)
pm.cc$ppvt.cs <- scale(pm.cc$ppvt.raw)
### Run regressions
m1 <- rlm(wj3.rcomp.cs ~ age.c + composite1 + wasi.matr.cs, data = pm.cc, method = "MM")

#standardize predicted values
pm.cc$Pred <- predict(m1)
pm.cc$stdPred <- scale(pm.cc$Pred)

#obtained residuals
pm.cc$resid <- resid <- residuals(m1)
qqnorm(scale(resid)); qqline(scale(resid))

##function to copmert CIs to z-values
ci2z <- function(ci)
{
  qnorm(ci + (1 - ci)/2)
}

ci.to.use <- c(.20, .60)
#create the range of SD (z value) bins 
my.bin <- c(range(scale(resid))+.1, ci2z(ci.to.use), -ci2z(ci.to.use))
#we could also manually specify in terms of SDs
# my.bin <- c(-3, -1, -.25, .25, 1, 3)
my.bin <- sort(my.bin)
# names for each bin specified above
grouping <- c("UPC", "NSC", "EAC", "NSC", "UGC")

pm.cc$CI_group <- grouping[findInterval(scale(resid), vec=my.bin, all.inside=T)]
#those with Standardized Predicted Value below -1 should be "NSC"
pm.cc$CI_group[pm.cc$stdPred < -1] <- "NSC"

table(pm.cc$CI_group)


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
m2 <- lm(wj3.rcomp.cs ~ stdPred, data=pm.cc)

p1 <- ggplot(pm.cc, aes(x=stdPred, y=wj3.rcomp.cs)) + geom_point(aes(color=CI_group)) + 
  geom_abline(intercept = coef(m2)[1], slope = coef(m2)[2]) +
  geom_abline(intercept = coef(m2)[1] + unscale.ci[2], slope = coef(m2)[2], color=my.col[1]) +
  geom_abline(intercept = coef(m2)[1] + unscale.ci[3], slope = coef(m2)[2], color=my.col[1]) +
  geom_abline(intercept = coef(m2)[1] + unscale.ci[1], slope = coef(m2)[2], color=my.col[4]) +
  geom_abline(intercept = coef(m2)[1] + unscale.ci[4], slope = coef(m2)[2], color=my.col[3]) +
  geom_vline(xintercept = -1, lty="dotted") + 
  xlab("Standardized Predicted Value")+ylab("WJ3 Reading Comprehension") +
  ggtitle("CIs: 20%; 60%")

print(p1)

pm.cc2 <- subset(pm.cc, CI_group != "NSC")
pm.cc2$CI_group <- as.factor(pm.cc2$CI_group)
# reading comp
a1 <- aov(wj3.rcomp.cs ~ CI_group, data = pm.cc2)
summary(a1)
pairwise.t.test(pm.cc2$wj3.rcomp.cs, pm.cc2$CI_group, p.adjust.method = "bonferroni")
aggregate(pm.cc2$wj3.rcomp.cs, list(pm.cc2$CI_group), mean)
# age
a1 <- aov(age.c ~ CI_group, data = pm.cc2)
summary(a1)
pairwise.t.test(pm.cc2$age.c, pm.cc2$CI_group, p.adjust.method = "bonferroni")
aggregate(pm.cc2$age.c, list(pm.cc2$CI_group), mean)
# matrices
a1 <- aov(wasi.matr.cs ~ CI_group, data = pm.cc2)
summary(a1)
pairwise.t.test(pm.cc2$wasi.matr.cs, pm.cc2$CI_group, p.adjust.method = "bonferroni")
aggregate(pm.cc2$wasi.matr.cs, list(pm.cc2$CI_group), mean)
# decoding
a1 <- aov(composite1 ~ CI_group, data = pm.cc2)
summary(a1)
pairwise.t.test(pm.cc2$composite1, pm.cc2$CI_group, p.adjust.method = "bonferroni")
aggregate(pm.cc2$composite1, list(pm.cc2$CI_group), mean)
# ppvt
a1 <- aov(ppvt.cs ~ CI_group, data = pm.cc2)
summary(a1)
pairwise.t.test(pm.cc2$ppvt.cs, pm.cc2$CI_group, p.adjust.method = "bonferroni")
aggregate(pm.cc2$ppvt.cs, list(pm.cc2$CI_group), mean)

######## PPVT, regular LM

model.data.plm <- data[, c(1, 2, 5, 7, 9, 10, 11, 15, 16)]
plm.cc <- model.data.plm[complete.cases(model.data.plm),]

## Create Decoding Composite

#check distribution of composite parts
dagoTest(plm.cc$wj3.watt.raw)
dagoTest(plm.cc$wj3.wid.raw)
dagoTest(plm.cc$towre.w.ipm)
dagoTest(plm.cc$towre.nw.ipm)
#They're all significant, so let's center/scale/transform

#prepare to center, scale, and (if necessary) transform predictor variables
vars_tf <- c("wj3.watt.raw","wj3.wid.raw","towre.w.ipm", "towre.nw.ipm")
pp_md_tf <- preProcess(plm.cc[,vars_tf], method = c("center", "scale", "YeoJohnson"), na.remove=T)

#check the lambda values associated with the predictors
pp_md_tf$yj$wj3.watt.raw$lambda
pp_md_tf$yj$wj3.wid.raw$lambda
pp_md_tf$yj$towre.w.ipm$lambda
pp_md_tf$yj$towre.nw.ipm$lambda
#If lambda > 4, then look at the density plots and the raw data - there may be outliers driving the large number. If so, remove outliers and preProcess again.
# watt and wid have lambdas above 4
x <- density(plm.cc$wj3.watt.raw, bw="SJ", na.rm=T)
plot(x)
y <- density(plm.cc$wj3.wid.raw, bw="SJ", na.rm=T)
plot(y)

# One subj with WA score of 0, but no obvious outlier in LW
data$wj3.watt.raw[data$wj3.watt.raw == 0] <- NA


# Re-do above with WA outlier removed
vars_tf <- c("wj3.watt.raw","wj3.wid.raw","towre.w.ipm", "towre.nw.ipm")
pp_md_tf <- preProcess(data[,vars_tf], method = c("center", "scale", "YeoJohnson"), na.remove=T)

#check the lambda values associated with the predictors
pp_md_tf$yj$wj3.watt.raw$lambda
pp_md_tf$yj$wj3.wid.raw$lambda
pp_md_tf$yj$towre.w.ipm$lambda
pp_md_tf$yj$towre.nw.ipm$lambda

# WA lambda just went up...

#execute centering, scaling, tranforming, etc.
tf_data <- predict(pp_md_tf, plm.cc[,vars_tf])

tf_data$composite1 <- rowMeans(scale(tf_data[,c("wj3.watt.raw","wj3.wid.raw","towre.w.ipm", "towre.nw.ipm")]))

plm.cc$composite1 <- tf_data$composite1

plm.cc$composite1 <- scale(plm.cc$composite1)

## Checking predictors, outcome
dagoTest(plm.cc$composite1)
dagoTest(plm.cc$wasi.matr.raw)
dagoTest(plm.cc$ppvt.raw)
dagoTest(plm.cc$wj3.rcomp.raw)

## everything needs to be transformed except the composite
vars_tf <- c("wasi.matr.raw","ppvt.raw","wj3.rcomp.raw")
pp_md_tf <- preProcess(data[,vars_tf], method = c("center", "scale", "YeoJohnson"), na.remove=T)

#check the lambda values associated with the predictors
pp_md_tf$yj$wasi.matr.raw$lambda
pp_md_tf$yj$ppvt.raw$lambda
pp_md_tf$yj$wj3.rcomp.raw$lambda
## ppvt lambda is still high
x <- density(plm.cc$ppvt.raw, bw="SJ", na.rm=T)
plot(x)
# no obvious outlier?
tf_data <- predict(pp_md_tf, plm.cc[,vars_tf])
plm.cc$wasi.matr.tf <- tf_data$wasi.matr.raw
plm.cc$ppvt.tf <- tf_data$ppvt.raw
plm.cc$wj3.rcomp.tf <- tf_data$wj3.rcomp.raw

dagoTest(plm.cc$wasi.matr.tf)
dagoTest(plm.cc$ppvt.tf)
dagoTest(plm.cc$wj3.rcomp.tf)
# skewness has pretty much been fixed in all of them -- problem is just kurtosis

plm.cc$age.c <- plm.cc$age - mean(plm.cc$age, na.rm = TRUE)

### Run regressions
m1 <- lm(wj3.rcomp.tf ~ age.c + composite1 + wasi.matr.tf + ppvt.tf, data = plm.cc)

#standardize predicted values
plm.cc$Pred <- predict(m1)
plm.cc$stdPred <- scale(plm.cc$Pred)

#obtained residuals
plm.cc$resid <- resid <- residuals(m1)
qqnorm(scale(resid)); qqline(scale(resid))

##function to coplmert CIs to z-values
ci2z <- function(ci)
{
  qnorm(ci + (1 - ci)/2)
}

ci.to.use <- c(.15, .60)
#create the range of SD (z value) bins 
my.bin <- c(range(scale(resid))+.1, ci2z(ci.to.use), -ci2z(ci.to.use))
#we could also manually specify in terms of SDs
# my.bin <- c(-3, -1, -.25, .25, 1, 3)
my.bin <- sort(my.bin)
# names for each bin specified above
grouping <- c("UPC", "NSC", "EAC", "NSC", "UGC")

plm.cc$CI_group <- grouping[findInterval(scale(resid), vec=my.bin, all.inside=T)]
#those with Standardized Predicted Value below -1 should be "EPC"
plm.cc$CI_group[plm.cc$stdPred < -1] <- "EPC"

table(plm.cc$CI_group)


# functions to get the ggplot default colors, 
# so I can match the colors for dots and lines
gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}

### there are five groups c("EAC","NSC","UGC","UPC","EPC")
### so the argument n = 4
my.col <- gg_color_hue(n=5)

## unscale the z value for each CI, so I can add that value to intercept
unscale.ci <- my.bin[-c(1,length(my.bin))]*sd(resid)+mean(resid)

##
m2 <- lm(wj3.rcomp.tf ~ stdPred, data=plm.cc)

p1 <- ggplot(plm.cc, aes(x=stdPred, y=wj3.rcomp.tf)) + geom_point(aes(color=CI_group)) + 
  geom_abline(intercept = coef(m2)[1], slope = coef(m2)[2]) +
  geom_abline(intercept = coef(m2)[1] + unscale.ci[2], slope = coef(m2)[2], color=my.col[1]) +
  geom_abline(intercept = coef(m2)[1] + unscale.ci[3], slope = coef(m2)[2], color=my.col[1]) +
  geom_abline(intercept = coef(m2)[1] + unscale.ci[1], slope = coef(m2)[2], color=my.col[4]) +
  geom_abline(intercept = coef(m2)[1] + unscale.ci[4], slope = coef(m2)[2], color=my.col[3]) +
  geom_vline(xintercept = -1, lty="dotted") + 
  xlab("Standardized Predicted Value")+ylab("WJ3 Reading Comprehension") +
  ggtitle("CIs: 20%; 60%")

print(p1)

plm.cc2 <- subset(plm.cc, CI_group != "NSC")
plm.cc2$CI_group <- as.factor(plm.cc2$CI_group)
# reading comp
a1 <- aov(wj3.rcomp.tf ~ CI_group, data = plm.cc2)
summary(a1)
pairwise.t.test(plm.cc2$wj3.rcomp.tf, plm.cc2$CI_group, p.adjust.method = "bonferroni")
aggregate(plm.cc2$wj3.rcomp.tf, list(plm.cc2$CI_group), mean)
# age
a1 <- aov(age.c ~ CI_group, data = plm.cc2)
summary(a1)
pairwise.t.test(plm.cc2$age.c, plm.cc2$CI_group, p.adjust.method = "bonferroni")
aggregate(plm.cc2$age.c, list(plm.cc2$CI_group), mean)
# matrices
a1 <- aov(wasi.matr.tf ~ CI_group, data = plm.cc2)
summary(a1)
pairwise.t.test(plm.cc2$wasi.matr.tf, plm.cc2$CI_group, p.adjust.method = "bonferroni")
aggregate(plm.cc2$wasi.matr.tf, list(plm.cc2$CI_group), mean)
# decoding
a1 <- aov(composite1 ~ CI_group, data = plm.cc2)
summary(a1)
pairwise.t.test(plm.cc2$composite1, plm.cc2$CI_group, p.adjust.method = "bonferroni")
aggregate(plm.cc2$composite1, list(plm.cc2$CI_group), mean)
# ppvt
a1 <- aov(ppvtt.tf ~ CI_group, data = plm.cc2)
summary(a1)
pairwise.t.test(plm.cc2$ppvt.tf, plm.cc2$CI_group, p.adjust.method = "bonferroni")
aggregate(plm.cc2$ppvt.tf, list(plm.cc2$CI_group), mean)


export <- data.frame(plm.cc2$SubjectID, plm.cc2$CI_group)
names(export) <- c("subj", "group")
write.csv(export, "CIGroupsforAndy.csv")
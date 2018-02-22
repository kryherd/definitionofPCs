# Read in packages and data
library(fBasics)
library(caret)
library(MASS)
library(ggplot2)
library(plyr)
library(reshape2)
library(xtable)
setwd("~/Dropbox/Definition_of_PCs/Sept2016")
data <- read.csv("FullDataDec15_2016.csv")
data <- data[,-1]
dys <- read.csv("dys.csv")
data2 <- merge(data, dys, by = "SubjectID", all.x =TRUE)

# Define some functions
# for plotting
gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}

# CIs to z-scores
ci2z <- function(ci)
{
  qnorm(ci + (1 - ci)/2)
}

# Change your CIs
ci.to.use <- c(.15, .65)
ci.title <- "CIs: 15%, 65%"

# Run model, get groups

model.data.lmwv <- data[, c(1, 2, 18, 7, 6, 19, 11)]
lmwv.cc <- model.data.lmwv[complete.cases(model.data.lmwv),]
## Checking predictors, outcome for normality
dagoTest(lmwv.cc$decoding.comp)
dagoTest(lmwv.cc$wasi.matr.raw)
dagoTest(lmwv.cc$wasi.vocab.raw)
dagoTest(lmwv.cc$wj3.rcomp.raw)

## everything needs to be transformed except the decoding.comp, wasi vocab
vars_tf <- c("wasi.matr.raw", "wj3.rcomp.raw")
pp_md_tf <- preProcess(lmwv.cc[,vars_tf], method = c("center", "scale", "YeoJohnson"), na.remove=T)

#check the lambda values associated with the predictors
pp_md_tf$yj$wasi.matr.raw$lambda
pp_md_tf$yj$wj3.rcomp.raw$lambda

tf_data <- predict(pp_md_tf, lmwv.cc[,vars_tf])
lmwv.cc$wasi.matr.tf <- tf_data$wasi.matr.raw
lmwv.cc$wj3.rcomp.tf <- tf_data$wj3.rcomp.raw

dagoTest(lmwv.cc$wasi.matr.tf)
dagoTest(lmwv.cc$wj3.rcomp.tf)
# skewness has pretty much been fixed in all of them -- problem is just kurtosis

# center age
lmwv.cc$age.c <- lmwv.cc$age - mean(lmwv.cc$age, na.rm = TRUE)
# center/scale wasi vocab
lmwv.cc$wasi.vocab.cs <- scale(lmwv.cc$wasi.vocab.raw)
# composite has already been scaled

### Run regression
m1 <- lm(wj3.rcomp.tf ~ age.c + decoding.comp + wasi.matr.tf + wasi.vocab.cs, data = lmwv.cc)

#standardize predicted values
lmwv.cc$Pred <- predict(m1)
lmwv.cc$stdPred <- scale(lmwv.cc$Pred)

#obtained residuals
lmwv.cc$resid <- resid <- residuals(m1)
qqnorm(scale(resid)); qqline(scale(resid))

#create the range of SD (z value) bins 
my.bin <- c(range(scale(resid))+.1, ci2z(ci.to.use), -ci2z(ci.to.use))
#we could also manually specify in terms of SDs
# my.bin <- c(-3, -1, -.25, .25, 1, 3)
my.bin <- sort(my.bin)
# names for each bin specified above
grouping <- c("UPC", "NSC", "EAC", "NSC", "UGC")

lmwv.cc$Group <- grouping[findInterval(scale(resid), vec=my.bin, all.inside=T)]
#those with Standardized Predicted Value below -1 should be "EPC" for expected poor comprehender
lmwv.cc$Group[lmwv.cc$stdPred < -1] <- "EPC"

### there are five groups c("EAC", "NSC", "UGC", "UPC", "EPC")
### so the argument n = 5
my.col <- gg_color_hue(n=5)

## unscale the z value for each CI, so I can add that value to intercept
unscale.ci <- my.bin[-c(1,length(my.bin))]*sd(resid)+mean(resid)

## create plot
m2 <- lm(wj3.rcomp.tf ~ stdPred, data=lmwv.cc)

p1 <- ggplot(lmwv.cc, aes(x=stdPred, y=wj3.rcomp.tf)) + geom_point(aes(color=Group)) + 
  geom_abline(intercept = coef(m2)[1], slope = coef(m2)[2]) +
  geom_abline(intercept = coef(m2)[1] + unscale.ci[2], slope = coef(m2)[2], color=my.col[1]) +
  geom_abline(intercept = coef(m2)[1] + unscale.ci[3], slope = coef(m2)[2], color=my.col[1]) +
  geom_abline(intercept = coef(m2)[1] + unscale.ci[1], slope = coef(m2)[2], color=my.col[5]) +
  geom_abline(intercept = coef(m2)[1] + unscale.ci[4], slope = coef(m2)[2], color=my.col[4]) +
  geom_vline(xintercept = -1, lty="dotted") + 
  xlab("Standardized Predicted Value")+ylab("WJ3 Reading Comprehension") +
  ggtitle(ci.title) + theme_bw()

p1

# Show group sizes
table(lmwv.cc$Group)

# Test for group differences on individual difference measures

# take away NSC group
lmwv.cc2 <- subset(lmwv.cc, Group != "NSC")
lmwv.cc2$Group <- as.factor(lmwv.cc2$Group)

# reading comprehension
a1 <- aov(wj3.rcomp.tf ~ Group, data = lmwv.cc2)
summary(a1)
pairwise.t.test(lmwv.cc2$wj3.rcomp.tf, lmwv.cc2$Group, p.adjust.method = "bonferroni")
aggregate(lmwv.cc2$wj3.rcomp.tf, list(lmwv.cc2$Group), mean)

# age
a1 <- aov(age.c ~ Group, data = lmwv.cc2)
summary(a1)
pairwise.t.test(lmwv.cc2$age.c, lmwv.cc2$Group, p.adjust.method = "bonferroni")
aggregate(lmwv.cc2$age.c, list(lmwv.cc2$Group), mean)

# matrix reasoning
a1 <- aov(wasi.matr.tf ~ Group, data = lmwv.cc2)
summary(a1)
pairwise.t.test(lmwv.cc2$wasi.matr.tf, lmwv.cc2$Group, p.adjust.method = "bonferroni")
aggregate(lmwv.cc2$wasi.matr.tf, list(lmwv.cc2$Group), mean)

# decoding
a1 <- aov(decoding.comp ~ Group, data = lmwv.cc2)
summary(a1)
pairwise.t.test(lmwv.cc2$decoding.comp, lmwv.cc2$Group, p.adjust.method = "bonferroni")
aggregate(lmwv.cc2$decoding.comp, list(lmwv.cc2$Group), mean)

# vocab
a1 <- aov(wasi.vocab.cs ~ Group, data = lmwv.cc2)
summary(a1)
pairwise.t.test(lmwv.cc2$wasi.vocab.cs, lmwv.cc2$Group, p.adjust.method = "bonferroni")
aggregate(lmwv.cc2$wasi.vocab.cs, list(lmwv.cc2$Group), mean)
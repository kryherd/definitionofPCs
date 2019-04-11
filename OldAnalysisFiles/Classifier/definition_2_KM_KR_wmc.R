library(ggplot2)
library(reshape2)
library(car)
library(caret)
library(fBasics)

#reading in & organizing data (md = model data)
md <- read.csv("df.data.wmc.KR.csv")
beh <- read.csv("UPCClassifierProject.csv")
md.2 <- merge(md, beh, by = "subject", all.x=TRUE, all.y=FALSE)
md.3 <- data.frame(md.2$subject, md.2$cohort.x, md.2$age.x, md.2$wj3.watt.raw.x, 
                   md.2$wj3.wid.raw.x, md.2$wasi.matr.raw.x, md.2$wasi.vocab.raw.x,
                   md.2$wj3.rcomp.raw, md.2$sspan.raw.x)
names(md.3) <- c("subject", "cohort", "age", "wj3.watt.raw", "wj3.wid.raw", 
                 "wasi.matr.raw", "wasi.vocab.raw", "wj3.rcomp.raw", "sspan.raw")
#remove extraneous column
#md<-md[,-17]
#names(md) <- c("Subj", "Age", "BD_Raw", "Matrix_Raw", "BD_SS","Matrix_SS","PIQ","WA_Raw","WA_SS",
            #   "LW_Raw", "LW_SS", "PPVT_raw","PPVT_SS", "KTEA_Raw", "KTEA_SS", "Group")
#taking away participants that don't have all values
## you can remove this if we end up imputing values
md2 <- md.3[complete.cases(md.3),]

#Prepare predictor variables
#check density plots of predictors, e.g.,
x <- density(md$age, bw="SJ", na.rm=T)
plot(x)
y <- density(md$wj3.watt.raw, bw="SJ", na.rm=T)
plot(y)
z <- density(md$wj3.wid.raw, bw="SJ", na.rm=T)
plot(z)
a <- density(md$wasi.matr.raw, bw="SJ", na.rm=T)
plot(a)
b <- density(md$sspan.raw, bw="SJ", na.rm=T)
plot(b)
c <- density(md$wasi.vocab.raw, bw="SJ", na.rm=T)
plot(c)
#note that the bandwidth smoothing can be set to something other than "SJ" - this is a general recommendation, but the default is "nrd0". Sometimes a predictor can look very strange if you retain "SJ"

#conduct normality test for predictor variables, e.g.,
dagoTest(md2$age)
dagoTest(md2$wj3.watt.raw)
dagoTest(md2$wj3.wid.raw)
dagoTest(md2$wasi.matr.raw)
dagoTest(md2$wasi.vocab.raw)
dagoTest(md2$sspan.raw)
#If the omnibus test is significant, consider transforming the data

#prepare to center, scale, and (if necessary) transform predictor variables
#vars_tf <- c("wj3.watt.raw","wj3.wid.raw","wasi.matr.raw")
#pp_md_tf <- preProcess(md2[,vars_tf], method = c("center", "scale", "YeoJohnson"), na.remove=T)

#check the lambda values associated with the predictors
#pp_md_tf$yj$wj3.watt.raw$lambda
#pp_md_tf$yj$wj3.wid.raw$lambda
#pp_md_tf$yj$wasi.matr.raw$lambda
#If lambda > 4, then look at the density plots and the raw data - there may be outliers driving the large number. If so, remove outliers and preProcess again.

#execute centering, scaling, tranforming, etc.
#tf_md <- predict(pp_md_tf, md2[,vars_tf])

#check normality of transformed variables
#dagoTest(tf_md$wj3.watt.raw)
#dagoTest(tf_md$wj3.wid.raw)
#dagoTest(tf_md$wasi.matr.raw)
# It is possible for data to remain skewed post-transformation. In general, however, the non-normality appears to derive from kurtosis rather than skewing, and the chi-squared statistic should be appreciably lower. If the transformation had no measurable effects - that is, if skewness remains obvious in the density plots and the test statistic - then I would suggest not transforming.

vars_ntf <- c("wasi.vocab.raw", "wj3.watt.raw","wj3.wid.raw","wasi.matr.raw","sspan.raw")
#since nothing needs to be transformed, took away YJ
pp_md_ntf <- preProcess(md2[,vars_ntf], method = c("center", "scale"), na.remove=T)
#execute centering, scaling, tranforming, etc.
ntf_md <- predict(pp_md_ntf, md2[,vars_ntf])

#centering age
mean.age <- mean(md2$age)
center.age <- md2$age - mean.age

#new data frame
sct_data <- data.frame(md2$subject, md2$cohort, md2$wj3.rcomp.raw, ntf_md, center.age)
colnames(sct_data)[1] <- "subject"
colnames(sct_data)[2] <- "cohort"
colnames(sct_data)[3] <- "wj3.rcomp.raw"
colnames(sct_data)[9] <- "age"

#model with LW, WA, PIQ, Age
m1 <- lm(wj3.rcomp.raw~age+wj3.watt.raw+wj3.wid.raw+wasi.matr.raw+wasi.vocab.raw+sspan.raw, data=sct_data)
#standardize predicted values
sct_data$Pred <- predict(m1)
sct_data$stdPred <- scale(sct_data$Pred)

# obtain the residuals
sct_data$resid <- resid <- residuals(m1)
qqnorm(scale(resid)); qqline(scale(resid))

#write the residuals to a file
residuals <- data.frame(sct_data$subject, sct_data$resid)
write.csv(residuals, "WMCresid.csv")


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

# comparing behavioral data
#WCJ3 Passage comp
wj3.rcomp.ss <- aov(wj3.rcomp.ss ~ CI_group, data=sct_data) 
summary(wj3.rcomp.ss)
gwj3.rcomp.ss<-ddply(sct_data, c("CI_group"), summarize,
                 AVERAGE=mean(wj3.rcomp.ss),
                 SE=sqrt(var(wj3.rcomp.ss)/length(wj3.rcomp.ss)))
gwj3.rcomp.ss <- gwj3.rcomp.ss[-c(2,3),]
ggplot(gwj3.rcomp.ss) +
  aes(x=CI_group, y=AVERAGE, colour=CI_group)+
  geom_point()+
  geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE)) +
  scale_x_discrete("CI_group")+
  scale_y_continuous("wj3.rcomp.ss")
t.test(subset(sct_data$wj3.rcomp.ss,sct_data$CI_group == "UPC"),
       + subset(sct_data$wj3.rcomp.ss,sct_data$CI_group == "EAC"))

#Word Attack
wj3.watt.raw <- aov(wj3.watt.raw ~ CI_group, data=sct_data) 
summary(wj3.watt.raw)
gwj3.watt.raw<-ddply(sct_data, c("CI_group"), summarize,
                     AVERAGE=mean(wj3.watt.raw),
                     SE=sqrt(var(wj3.watt.raw)/length(wj3.watt.raw)))
gwj3.watt.raw <- gwj3.watt.raw[-c(2,3),]
ggplot(gwj3.watt.raw) +
  aes(x=CI_group, y=AVERAGE, colour=CI_group)+
  geom_point()+
  geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE)) +
  scale_x_discrete("CI_group")+
  scale_y_continuous("wj3.watt.raw")
t.test(subset(sct_data$wj3.watt.raw,sct_data$CI_group == "UPC"),
       + subset(sct_data$wj3.watt.raw,sct_data$CI_group == "EAC"))

#Letter-word
wj3.wid.raw <- aov(wj3.wid.raw ~ CI_group, data=sct_data) 
summary(wj3.wid.raw)
gwj3.wid.raw<-ddply(sct_data, c("CI_group"), summarize,
                     AVERAGE=mean(wj3.wid.raw),
                     SE=sqrt(var(wj3.wid.raw)/length(wj3.wid.raw)))
gwj3.wid.raw <- gwj3.wid.raw[-c(2,3),]
ggplot(gwj3.wid.raw) +
  aes(x=CI_group, y=AVERAGE, colour=CI_group)+
  geom_point()+
  geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE)) +
  scale_x_discrete("CI_group")+
  scale_y_continuous("wj3.wid.raw")
t.test(subset(sct_data$wj3.wid.raw,sct_data$CI_group == "UPC"),
       + subset(sct_data$wj3.wid.raw,sct_data$CI_group == "EAC"))
#Matrix Reasoning
wasi.matr.raw <- aov(wasi.matr.raw ~ CI_group, data=sct_data) 
summary(wasi.matr.raw)
gwasi.matr.raw<-ddply(sct_data, c("CI_group"), summarize,
                    AVERAGE=mean(wasi.matr.raw),
                    SE=sqrt(var(wasi.matr.raw)/length(wasi.matr.raw)))
gwasi.matr.raw <- gwasi.matr.raw[-c(2,3),]
ggplot(gwasi.matr.raw) +
  aes(x=CI_group, y=AVERAGE, colour=CI_group)+
  geom_point()+
  geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE)) +
  scale_x_discrete("CI_group")+
  scale_y_continuous("wasi.matr.raw")
t.test(subset(sct_data$wasi.matr.raw,sct_data$CI_group == "UPC"),
       + subset(sct_data$wasi.matr.raw,sct_data$CI_group == "EAC"))
#WASI Vocab
wasi.vocab.raw <- aov(wasi.vocab.raw ~ CI_group, data=sct_data) 
summary(wasi.vocab.raw)
gwasi.vocab.raw<-ddply(sct_data, c("CI_group"), summarize,
                      AVERAGE=mean(wasi.vocab.raw),
                      SE=sqrt(var(wasi.vocab.raw)/length(wasi.vocab.raw)))
gwasi.vocab.raw <- gwasi.vocab.raw[-c(2,3),]
ggplot(gwasi.vocab.raw) +
  aes(x=CI_group, y=AVERAGE, colour=CI_group)+
  geom_point()+
  geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE)) +
  scale_x_discrete("CI_group")+
  scale_y_continuous("wasi.vocab.raw")
t.test(subset(sct_data$wasi.vocab.raw,sct_data$CI_group == "UPC"),
       + subset(sct_data$wasi.vocab.raw,sct_data$CI_group == "EAC"))

#Age
age <- aov(age ~ CI_group, data=sct_data) 
summary(age)
gage<-ddply(sct_data, c("CI_group"), summarize,
                       AVERAGE=mean(age),
                       SE=sqrt(var(age)/length(age)))
gage <- gage[-c(2,3),]
ggplot(gage) +
  aes(x=CI_group, y=AVERAGE, colour=CI_group)+
  geom_point()+
  geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE)) +
  scale_x_discrete("CI_group")+
  scale_y_continuous("age")
t.test(subset(sct_data$age,sct_data$CI_group == "UPC"),
       + subset(sct_data$age,sct_data$CI_group == "EAC"))

#Sentence Span
sspan.raw <- aov(sspan.raw ~ CI_group, data=sct_data) 
summary(sspan.raw)
gsspan.raw<-ddply(sct_data, c("CI_group"), summarize,
            AVERAGE=mean(sspan.raw),
            SE=sqrt(var(sspan.raw)/length(sspan.raw)))
gsspan.raw <- gsspan.raw[-c(2,3),]
ggplot(gsspan.raw) +
  aes(x=CI_group, y=AVERAGE, colour=CI_group)+
  geom_point()+
  geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE)) +
  scale_x_discrete("CI_group")+
  scale_y_continuous("sspan.raw")
t.test(subset(sct_data$sspan.raw,sct_data$CI_group == "UPC"),
       + subset(sct_data$sspan.raw,sct_data$CI_group == "EAC"))

write.csv(sct_data, "df.groups.wmc.KR.csv")


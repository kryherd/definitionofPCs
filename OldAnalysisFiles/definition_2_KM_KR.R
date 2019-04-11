library(ggplot2)
library(reshape2)
library(car)
library(caret)
library(fBasics)

#reading in & organizing data (md = model data)
md <- read.csv("definition_analysis_color.csv")
#remove extraneous column
md<-md[,-17]
names(md) <- c("Subj", "Age", "BD_Raw", "Matrix_Raw", "BD_SS","Matrix_SS","PIQ","WA_Raw","WA_SS",
               "LW_Raw", "LW_SS", "PPVT_raw","PPVT_SS", "KTEA_Raw", "KTEA_SS", "Group")
#taking away participants that don't have all values
## you can remove this if we end up imputing values
md2 <- md[complete.cases(md),]

#Prepare predictor variables
#check density plots of predictors, e.g.,
x <- density(md$LW_Raw, bw="SJ", na.rm=T)
plot(x)
y <- density(md$WA_Raw, bw="SJ", na.rm=T)
plot(y)
z <- density(md$Age, bw="SJ", na.rm=T)
plot(z)
a <- density(md$PIQ, bw="SJ", na.rm=T)
plot(a)
#note that the bandwidth smoothing can be set to something other than "SJ" - this is a general recommendation, but the default is "nrd0". Sometimes a predictor can look very strange if you retain "SJ"

#conduct normality test for predictor variables, e.g.,
dagoTest(md2$LW_Raw)
dagoTest(md2$WA_Raw)
dagoTest(md2$Age)
dagoTest(md2$PIQ)
#If the omnibus test is significant, consider transforming the data

#prepare to center, scale, and (if necessary) transform predictor variables
vars_tf <- c("LW_Raw","WA_Raw", "Age")
pp_md_tf <- preProcess(md2[,vars_tf], method = c("center", "scale", "YeoJohnson"), na.remove=T)

#check the lambda values associated with the predictors
pp_md_tf$yj$WA_Raw$lambda
pp_md_tf$yj$LW_Raw$lambda
pp_md_tf$yj$Age$lambda
#If lambda > 4, then look at the density plots and the raw data - there may be outliers driving the large number. If so, remove outliers and preProcess again.

#execute centering, scaling, tranforming, etc.
tf_md <- predict(pp_md_tf, md2[,vars_tf])

#check normality of transformed variables
dagoTest(tf_md$LW_Raw)
dagoTest(tf_md$WA_Raw)
dagoTest(tf_md$Age)
# It is possible for data to remain skewed post-transformation. In general, however, the non-normality appears to derive from kurtosis rather than skewing, and the chi-squared statistic should be appreciably lower. If the transformation had no measurable effects - that is, if skewness remains obvious in the density plots and the test statistic - then I would suggest not transforming.

#subject 4012 has potentially outlier scores on WA and LW (low); since they're a poor decoder anyway we'll retry this without them.
md3 <- md2[-1,]
x <- density(md3$LW_Raw, bw="SJ", na.rm=T)
plot(x)
y <- density(md3$WA_Raw, bw="SJ", na.rm=T)
plot(y)
z <- density(md3$Age, bw="SJ", na.rm=T)
plot(z)
a <- density(md3$PIQ, bw="SJ", na.rm=T)
plot(a)
b <- density(md3$PPVT_raw, bw="SJ", na.rm=T)
plot(b)
dagoTest(md3$LW_Raw)
dagoTest(md3$WA_Raw)
dagoTest(md3$Age)
dagoTest(md3$PIQ)
dagoTest(md3$PPVT_raw)
# getting error "Error in match(x, table, nomatch = 0L) : object 'out' not found"
# but, before removing 4012, transforming Age didn't really seem to help anyways, so I'm not really worried about it.
#also, I tried transforming PPVT_raw nad it didn't seem to help again
vars_tf <- c("LW_Raw", "WA_Raw", "Age", "PIQ", "PPVT_raw")
#since nothing needs to be transformed, took away YJ
pp_md_tf <- preProcess(md3[,vars_tf], method = c("center", "scale"), na.remove=T)
#execute centering, scaling, tranforming, etc.
tf_md <- predict(pp_md_tf, md3[,vars_tf])

#new data frame
tf_md <- data.frame(md3$Subj, md3$KTEA_Raw, tf_md)
colnames(tf_md)[1] <- "Subj"
colnames(tf_md)[2] <- "KTEA_Raw"

#model with LW, WA, PIQ, Age
m1 <- lm(KTEA_Raw~Age*LW_Raw*WA_Raw*PIQ, data=tf_md)
#standardize predicted values
tf_md$Pred <- predict(m1)
tf_md$stdPred <- scale(tf_md$Pred)

# obtain the residuals
tf_md$resid <- resid <- residuals(m1)
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

tf_md$CI_group <- grouping[findInterval(scale(resid), vec=my.bin, all.inside=T)]
#those with Standardized Predicted Value below -1 should be "NSC"
tf_md$CI_group[tf_md$stdPred < -1] <- "NSC"

table(tf_md$CI_group)
tapply(md3$Age, tf_md$CI_group, mean)

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
m2 <- lm(KTEA_Raw ~ stdPred, data=tf_md)

p1 <- ggplot(tf_md, aes(x=stdPred, y=KTEA_Raw)) + geom_point(aes(color=CI_group)) + 
  geom_abline(intercept = coef(m2)[1], slope = coef(m2)[2]) +
  geom_abline(intercept = coef(m2)[1] + unscale.ci[2], slope = coef(m2)[2], color=my.col[1]) +
  geom_abline(intercept = coef(m2)[1] + unscale.ci[3], slope = coef(m2)[2], color=my.col[1]) +
  geom_abline(intercept = coef(m2)[1] + unscale.ci[1], slope = coef(m2)[2], color=my.col[4]) +
  geom_abline(intercept = coef(m2)[1] + unscale.ci[4], slope = coef(m2)[2], color=my.col[3]) +
  geom_vline(xintercept = -1, lty="dotted") + 
  xlab("Standardized Predicted Value")

print(p1)

m2 <- lm(KTEA_Raw~Age*LW_Raw*WA_Raw*PIQ*PPVT_raw, data=tf_md)
#standardize predicted values
tf_md$Pred <- predict(m2)
tf_md$stdPred <- scale(tf_md$Pred)

# obtain the residuals
tf_md$resid <- resid <- residuals(m2)
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

tf_md$CI_group <- grouping[findInterval(scale(resid), vec=my.bin, all.inside=T)]
#those with Standardized Predicted Value below -1 should be "NSC"
tf_md$CI_group[tf_md$stdPred < -1] <- "NSC"

table(tf_md$CI_group)
tapply(md3$Age, tf_md$CI_group, mean)

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
m3 <- lm(KTEA_Raw ~ stdPred, data=tf_md)

p1 <- ggplot(tf_md, aes(x=stdPred, y=KTEA_Raw)) + geom_point(aes(color=CI_group)) + 
  geom_abline(intercept = coef(m3)[1], slope = coef(m3)[2]) +
  geom_abline(intercept = coef(m3)[1] + unscale.ci[2], slope = coef(m3)[2], color=my.col[1]) +
  geom_abline(intercept = coef(m3)[1] + unscale.ci[3], slope = coef(m3)[2], color=my.col[1]) +
  geom_abline(intercept = coef(m3)[1] + unscale.ci[1], slope = coef(m3)[2], color=my.col[4]) +
  geom_abline(intercept = coef(m3)[1] + unscale.ci[4], slope = coef(m3)[2], color=my.col[3]) +
  geom_vline(xintercept = -1, lty="dotted") + 
  xlab("Standardized Predicted Value")

print(p1)

beh <- read.csv("export_for_kayLAY.csv")
colnames(beh)[1] <- "Subj"
total <- merge(tf_md,beh,by="Subj")
# comparing behavior data for groups
KTEA_Raw <- aov(KTEA_Raw ~ CI_group, data=total) 
summary(KTEA_Raw)
LW_Raw <- aov(LW_Raw ~ CI_group, data=total) 
summary(LW_Raw)
WA_Raw <- aov(WA_Raw ~ CI_group, data=total) 
summary(WA_Raw)
Age <- aov(Age ~ CI_group, data=total) 
summary(Age)
PIQ <- aov(PIQ ~ CI_group, data=total) 
summary(PIQ)
Auditory.Span..TotalScore <- aov(Auditory.Span..TotalScore ~ CI_group, data=total) 
summary(Auditory.Span..TotalScore)
CTOPPTable2..PA_SS <- aov(CTOPPTable2..PA_SS ~ CI_group, data=total) 
summary(CTOPPTable2..PA_SS)
PPVTTable2..rawScore <- aov(PPVTTable2..rawScore ~ CI_group, data=total) 
summary(PPVTTable2..rawScore)
Stroop..EffectB <- aov(Stroop..EffectB ~ CI_group, data=total) 
summary(Stroop..EffectB)
Op.Span..PartialScore <- aov(Op.Span..PartialScore ~ CI_group, data=total) 
summary(Op.Span..PartialScore)
WASITable2..Verb_IQ <- aov(WASITable2..Verb_IQ ~ CI_group, data=total) 
summary(WASITable2..Verb_IQ)
BRIEF.SR..Inhibit_Raw <- aov(BRIEF.SR..Inhibit_Raw ~ CI_group, data=total) 
summary(BRIEF.SR..Inhibit_Raw)
BRIEF.SR..TaskMonitor_Raw <- aov(BRIEF.SR..TaskMonitor_Raw ~ CI_group, data=total) 
summary(BRIEF.SR..TaskMonitor_Raw)
BRIEF.SR..PlanOrganize_Raw <- aov(BRIEF.SR..PlanOrganize_Raw ~ CI_group, data=total) 
summary(BRIEF.SR..PlanOrganize_Raw)
BRIEF.SR..GlobalExecComposite_Raw <- aov(BRIEF.SR..GlobalExecComposite_Raw ~ CI_group, data=total) 
summary(BRIEF.SR..GlobalExecComposite_Raw)

gKTEA_Raw<-ddply(total, c("CI_group"), summarize,
                     AVERAGE=mean(KTEA_Raw),
                     SE=sqrt(var(KTEA_Raw)/length(KTEA_Raw)))
gKTEA_Raw <- gKTEA_Raw[-c(2,3),]
ggplot(gKTEA_Raw) +
  aes(x=CI_group, y=AVERAGE, colour=CI_group)+
  geom_point()+
  geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE)) +
  scale_x_discrete("CI_group")+
  scale_y_continuous("KTEA_Raw")
t.test(subset(total$KTEA_Raw,total$CI_group == "UPC"),
       + subset(total$KTEA_Raw,total$CI_group == "EAC"))

gLW_Raw<-ddply(total, c("CI_group"), summarize,
                 AVERAGE=mean(LW_Raw),
                 SE=sqrt(var(LW_Raw)/length(LW_Raw)))
gLW_Raw <- gLW_Raw[-c(2,3),]
ggplot(gLW_Raw) +
  aes(x=CI_group, y=AVERAGE, colour=CI_group)+
  geom_point()+
  geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE)) +
  scale_x_discrete("CI_group")+
  scale_y_continuous("LW_Raw")
t.test(subset(total$LW_Raw,total$CI_group == "UPC"),
       + subset(total$LW_Raw,total$CI_group == "EAC"))


gWA_Raw<-ddply(total, c("CI_group"), summarize,
               AVERAGE=mean(WA_Raw),
               SE=sqrt(var(WA_Raw)/length(WA_Raw)))
gWA_Raw <- gWA_Raw[-c(2,3),]
ggplot(gWA_Raw) +
  aes(x=CI_group, y=AVERAGE, colour=CI_group)+
  geom_point()+
  geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE)) +
  scale_x_discrete("CI_group")+
  scale_y_continuous("WA_Raw")
t.test(subset(total$WA_Raw,total$CI_group == "UPC"),
       + subset(total$WA_Raw,total$CI_group == "EAC"))


gAge<-ddply(total, c("CI_group"), summarize,
               AVERAGE=mean(Age),
               SE=sqrt(var(Age)/length(Age)))
gAge <- gAge[-c(2,3),]
ggplot(gAge) +
  aes(x=CI_group, y=AVERAGE, colour=CI_group)+
  geom_point()+
  geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE)) +
  scale_x_discrete("CI_group")+
  scale_y_continuous("Age")
t.test(subset(total$Age,total$CI_group == "UPC"),
       + subset(total$Age,total$CI_group == "EAC"))

gPIQ<-ddply(total, c("CI_group"), summarize,
               AVERAGE=mean(PIQ),
               SE=sqrt(var(PIQ)/length(PIQ)))
gPIQ <- gPIQ[-c(2,3),]
ggplot(gPIQ) +
  aes(x=CI_group, y=AVERAGE, colour=CI_group)+
  geom_point()+
  geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE)) +
  scale_x_discrete("CI_group")+
  scale_y_continuous("PIQ")
t.test(subset(total$PIQ,total$CI_group == "UPC"),
       + subset(total$PIQ,total$CI_group == "EAC"))

gPPVT_raw<-ddply(total, c("CI_group"), summarize,
            AVERAGE=mean(PPVT_raw),
            SE=sqrt(var(PPVT_raw)/length(PPVT_raw)))
gPPVT_raw <- gPPVT_raw[-c(2,3),]
ggplot(gPPVT_raw) +
  aes(x=CI_group, y=AVERAGE, colour=CI_group)+
  geom_point()+
  geom_errorbar(aes(ymax=AVERAGE+SE, ymin=AVERAGE-SE)) +
  scale_x_discrete("CI_group")+
  scale_y_continuous("PPVT_raw")
t.test(subset(total$PPVT_raw,total$CI_group == "UPC"),
       + subset(total$PPVT_raw,total$CI_group == "EAC"))

total2 <- merge(total,md,by="Subj")

t.test(subset(total2$PIQ.x,total2$CI_group == "UPC"),
       + subset(total2$PIQ.x,total2$Group == "Target"))
t.test(subset(total2$KTEA_Raw.x,total2$CI_group == "UPC"),
       + subset(total2$KTEA_Raw.x,total2$Group == "Target"))
t.test(subset(total2$LW_Raw.x,total2$CI_group == "UPC"),
       + subset(total2$LW_Raw.x,total2$Group == "Target"))
t.test(subset(total2$WA_Raw.x,total2$CI_group == "UPC"),
       + subset(total2$WA_Raw.x,total2$Group == "Target"))
t.test(subset(total2$Age.x,total2$CI_group == "UPC"),
       + subset(total2$Age.x,total2$Group == "Target"))
t.test(subset(total2$PPVT_raw.x,total2$CI_group == "UPC"),
       + subset(total2$PPVT_raw.x,total2$Group == "Target"))





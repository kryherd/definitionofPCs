### checks for: -- from Clint?
#univariate and multivariate outliers
#linearity
#independent observation
#homoscedasticity
#normal distribution of residual errors

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
#note that the bandwidth smoothing can be set to something other than "SJ" - this is a general recommendation, but the default is "nrd0". Sometimes a predictor can look very strange if you retain "SJ"

#conduct normality test for predictor variables, e.g.,
dagoTest(md2$LW_Raw)
dagoTest(md2$WA_Raw)
#If the omnibus test is significant, consider transforming the data

#prepare to center, scale, and (if necessary) transform predictor variables
vars <- c("LW_Raw","WA_Raw")
pp_md <- preProcess(md2[,vars], method = c("center", "scale", "YeoJohnson"), na.remove=T)

#check the lambda values associated with the predictors
pp_md$yj$WA_Raw$lambda
pp_md$yj$LW_Raw$lambda
#If lambda > 4, then look at the density plots and the raw data - there may be outliers driving the large number. If so, remove outliers and preProcess again.

#execute centering, scaling, tranforming, etc.
tf_md <- predict(pp_md, md2[,vars])

#check normality of transformed variables
dagoTest(tf_md2$LW_Raw)
dagoTest(tf_md2$WA_Raw)
# It is possible for data to remain skewed post-transformation. In general, however, the non-normality appears to derive from kurtosis rather than skewing, and the chi-squared statistic should be appreciably lower. If the transformation had no measurable effects - that is, if skewness remains obvious in the density plots and the test statistic - then I would suggest not transforming.

#new data frame
tf_md <- data.frame(md2$Subj, md2$KTEA_Raw, tf_md)
colnames(tf_md)[1] <- "Subj"
colnames(tf_md)[2] <- "KTEA_Raw"

#model with LW, WA, PIQ, Age
m1 <- lm(KTEA_Raw~Age*LW_Raw*WA_Raw*PIQ, data=tf_md)
#standardize predicted values
tf_md$Pred <- predict(m1)
tf_md$stdPred <- scale(tf_md$Pred)

# obtain the residuals
tf_md$resid <- resid <- residuals(m1)
#qqnorm(scale(resid)); qqline(scale(resid))

##function to convert CIs to z-values
ci2z <- function(ci)
{
  qnorm(ci + (1 - ci)/2)
}

ci.to.use <- c(.80, .15)
#create the range of SD (z value) bins 
my.bin <- c(range(scale(resid))+.1, ci2z(ci.to.use), -ci2z(ci.to.use))
#we could also manually specify in terms of SDs
# my.bin <- c(-3, -1.5, -.5, .5, 1.5, 3)
my.bin <- sort(my.bin)
# names for each bin specified above
grouping <- c("UPC", "NSC", "EAC", "NSC", "UGC")

tf_md$CI_group <- grouping[findInterval(scale(resid), vec=my.bin, all.inside=T)]
#those with Standardized Predicted Value below -1 should be "NSC"
tf_md$CI_group[tf_md$stdPred < -1] <- "NSC"

table(tf_md$CI_group)
tapply(md2$Age, tf_md$CI_group, mean)

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


#model with LW, WA, PIQ, Age, WASI - Vocab
## I'll do this part once the above is verified
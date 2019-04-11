library(ggplot2)
library(reshape2)
library(car)
library(caret)
library(fBasics)
library(plyr)
library(dplyr)

data <- read.csv("robots.csv")

wmc <- data[complete.cases(data),]
nowmc <- data[,-8]
nowmc <- nowmc[complete.cases(nowmc),]


##WITH WMC
vars_ntf <- c("Age","PPVT", "WA",
              "LW","MatrixReasoning","Aspan")
pp_md_ntf <- preProcess(wmc[,vars_ntf], method = c("center", "scale"), na.remove=T)
ntf_md <- predict(pp_md_ntf, wmc[,vars_ntf])


sct_data <- data.frame(wmc$Subj, wmc$KTEA, ntf_md)
colnames(sct_data)[1] <- "Subj"
colnames(sct_data)[2] <- "KTEA"

m1 <- lm(KTEA~Age+PPVT+WA+LW+MatrixReasoning+Aspan, data=sct_data)
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

ci.to.use <- c(.20, .60)
#create the range of SD (z value) bins 
my.bin <- c(range(scale(resid))+.1, ci2z(ci.to.use), -ci2z(ci.to.use))
#we could also manually specify in terms of SDs
# my.bin <- c(-3, -1, -.25, .25, 1, 3)
my.bin <- sort(my.bin)
# names for each bin specified above
grouping <- c("UPC", "NSC", "EAC", "NSC", "UGC")

sct_data$CI_group <- grouping[findInterval(scale(resid), vec=my.bin, all.inside=T)]
#those with Standardized Predicted Value below -1 should be "NSC"
sct_data$CI_group[ntf_md$stdPred < -1] <- "NSC"

table(sct_data$CI_group)
tapply(wmc$Age, sct_data$CI_group, mean)

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
m2 <- lm(KTEA ~ stdPred, data=sct_data)

p1 <- ggplot(sct_data, aes(x=stdPred, y=KTEA)) + geom_point(aes(color=CI_group)) + 
  geom_abline(intercept = coef(m2)[1], slope = coef(m2)[2]) +
  geom_abline(intercept = coef(m2)[1] + unscale.ci[2], slope = coef(m2)[2], color=my.col[1]) +
  geom_abline(intercept = coef(m2)[1] + unscale.ci[3], slope = coef(m2)[2], color=my.col[1]) +
  geom_abline(intercept = coef(m2)[1] + unscale.ci[1], slope = coef(m2)[2], color=my.col[4]) +
  geom_abline(intercept = coef(m2)[1] + unscale.ci[4], slope = coef(m2)[2], color=my.col[3]) +
  geom_vline(xintercept = -1, lty="dotted") + 
  xlab("Standardized Predicted Value")

print(p1)

## WITHOUT WMC
vars_ntf <- c("Age","PPVT", "WA",
              "LW","MatrixReasoning")
pp_md_ntf <- preProcess(nowmc[,vars_ntf], method = c("center", "scale"), na.remove=T)
ntf_md <- predict(pp_md_ntf, nowmc[,vars_ntf])


sct_data2 <- data.frame(nowmc$Subj, nowmc$KTEA, ntf_md)
colnames(sct_data2)[1] <- "Subj"
colnames(sct_data2)[2] <- "KTEA"

m1 <- lm(KTEA~Age+PPVT+WA+LW+MatrixReasoning, data=sct_data2)
#standardize predicted values
sct_data2$Pred <- predict(m1)
sct_data2$stdPred <- scale(sct_data2$Pred)

# obtain the residuals
sct_data2$resid <- resid <- residuals(m1)
qqnorm(scale(resid)); qqline(scale(resid))

##function to convert CIs to z-values
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

sct_data2$CI_group <- grouping[findInterval(scale(resid), vec=my.bin, all.inside=T)]
#those with Standardized Predicted Value below -1 should be "NSC"
sct_data2$CI_group[ntf_md$stdPred < -1] <- "NSC"

table(sct_data2$CI_group)
tapply(nowmc$Age, sct_data2$CI_group, mean)

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
m2 <- lm(KTEA ~ stdPred, data=sct_data2)

p1 <- ggplot(sct_data, aes(x=stdPred, y=KTEA)) + geom_point(aes(color=CI_group)) + 
  geom_abline(intercept = coef(m2)[1], slope = coef(m2)[2]) +
  geom_abline(intercept = coef(m2)[1] + unscale.ci[2], slope = coef(m2)[2], color=my.col[1]) +
  geom_abline(intercept = coef(m2)[1] + unscale.ci[3], slope = coef(m2)[2], color=my.col[1]) +
  geom_abline(intercept = coef(m2)[1] + unscale.ci[1], slope = coef(m2)[2], color=my.col[4]) +
  geom_abline(intercept = coef(m2)[1] + unscale.ci[4], slope = coef(m2)[2], color=my.col[3]) +
  geom_vline(xintercept = -1, lty="dotted") + 
  xlab("Standardized Predicted Value")

print(p1)

test <- merge(sct_data, sct_data2, by = "Subj", all = TRUE)

groups <- data.frame(test$Subj, test$CI_group.x, test$CI_group.y)
names(groups) <- c("Subject", "Group_WMC", "Group_noWMC")
write.csv(groups, "groups1.csv")


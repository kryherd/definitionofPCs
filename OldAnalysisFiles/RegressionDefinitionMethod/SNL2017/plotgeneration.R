ci.to.use <- c(.15, .65)
ci.title <- "CIs: 15%, 65%"

resid <- new$resid
new$CI_group <- grouping[findInterval(scale(resid), vec=my.bin, all.inside=T)]
#those with Standardized Predicted Value below -1 should be "EPC"
new$CI_group[new$stdPred < -1] <- "EPC"

### there are five groups c("EAC", "NSC", "UGC", "UPC", "EPC")
### so the argument n = 5
my.col <- gg_color_hue(n=3)

## unscale the z value for each CI, so I can add that value to intercept
unscale.ci <- my.bin[-c(1,length(my.bin))]*sd(resid)+mean(resid)

my.bin <- c(range(scale(resid))+.1, ci2z(ci.to.use), -ci2z(ci.to.use))
#we could also manually specify in terms of SDs
# my.bin <- c(-3, -1, -.25, .25, 1, 3)
my.bin <- sort(my.bin)
# names for each bin specified above
grouping <- c("UPC", "EAC", "UGC")

m2 <- lm(ktea2.t ~ stdPred, data=KTEANoInt15_65)

p1 <- ggplot(KTEANoInt15_65, aes(x=stdPred, y=ktea2.t)) + geom_point(aes(color=CI_group)) + 
  geom_abline(intercept = coef(m2)[1], slope = coef(m2)[2]) +
  xlab("Standardized Predicted Value")+ylab("KTEA Reading Comprehension") +
  ggtitle("Group Definition") + theme_bw() +
  theme(title = element_text(size = 30), text = element_text(size = 24), plot.background = element_rect(fill = "#EAE0CC"),
        legend.position = c(0.85, 0.3))
p1

test2 <- test2[complete.cases(test2$CI_group),]


KTEA2 <- read.csv("./AndyAnalysis/KTEANoInt15_65-imputed.csv")

allKTEA <- merge(`KTEANoInt15_65-imputed`, KTEA2, by = "SubjectID", all = TRUE)
allKTEA2 <- allKTEA[!is.na(allKTEA$CI_group.y),]
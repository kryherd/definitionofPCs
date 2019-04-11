library(ggplot2)
library(fBasics)
library(pastecs)

setwd("~/Dropbox/Misc Analyses/Definition_of_PCs/OtherExpDVs")
exp8 <- read.csv("Exp8_Mara.csv", header = TRUE)
beh <- read.csv("UPCClassifierProject.csv", header = TRUE)
beh$wj3.watt.raw[beh$wj3.watt.raw == 0] <- NA
beh$wj3.rf.ipm <- as.numeric(levels(beh$wj3.rf.ipm))[beh$wj3.rf.ipm]
beh$wj3.rf.ipm[beh$wj3.rf.ipm == 0] <- NA
PPLS_sv <- read.csv("PrintPLS_StoryVisual.csv", header = TRUE)

## Make it easy to change which data you're using
df <- PPLS_sv
var <- PPLS_sv$ACC
varlab <- "ACC"

#### First, plots

## density
### discovering stats using R's version
density <- ggplot(df, aes(var)) + guides(fill=FALSE) + 
  geom_histogram(aes(y=..density..), color="black", fill="white") + 
  labs(x=varlab, y="Density") + 
  stat_function(fun = dnorm, args = list(mean = mean(var, na.rm = TRUE),
                                         sd = sd(var, na.rm = TRUE)), color="black", size =1)
density
### clint's version
x <- density(var, bw="SJ", na.rm=T)
plot(x, main = varlab)
## q-q
qqplot <- ggplot(df, aes(sample = var)) + stat_qq() + ggtitle(varlab)
qqplot

## tests for normality
dagoTest(var)
stat.desc(var, basic = FALSE, norm = TRUE)

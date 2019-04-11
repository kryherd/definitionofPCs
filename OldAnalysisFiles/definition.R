def <- CI_Analysis2
#install.packages("car",dependencies=TRUE)
library(ggplot2)
library(reshape2)
library(car)
#0 - Control; 1 - SoftControl; 2 - SoftTarget; 3 - Target; 4 - PoorDecoder
def$Group <- as.character(def$Group)
def$Group[def$Group == "Control"] <- as.integer(0)
def$Group[def$Group == "SoftControl"] <- as.integer(1)
def$Group[def$Group == "SoftTarget"] <- as.integer(2)
def$Group[def$Group == "Target"] <- as.integer(3)
def$Group[def$Group == "PoorDecoder"] <- as.integer(4)
meltdef <- melt(def, id.vars = "ID")
#examination of univariate outliers
#starting with boxplots
boxplot(def$BlockDesign_Raw, xlab="BlockDesign_Raw")
boxplot(def$BlockDesign_SS, xlab="BlockDesign_SS")
boxplot(def$Matrices_Raw, xlab="Matrices_Raw")
boxplot(def$Matrices_SS, xlab="Matrices_SS")
boxplot(def$PIQ, xlab="PIQ")
boxplot(def$WA_Raw, xlab="WA_Raw")
boxplot(def$WA_SS, xlab="WA_SS")
boxplot(def$LW_Raw, xlab="LW_Raw")
boxplot(def$LW_SS, xlab="LW_SS")
boxplot(def$PPVT_raw, xlab="PPVT_raw")
boxplot(def$PPVT_SS, xlab="PPVT_SS")
boxplot(def$KTEA_Raw, xlab="KTEA_Raw")
boxplot(def$KTEA_SS, xlab="KTEA_SS")
boxplot(def$Age, xlab="Age")

plot(ecdf(def$KTEA_Raw))
qqnorm(def$KTEA_Raw)
qqline(def$KTEA_Raw)

#getting rid of subs without all data
def2 <- def[complete.cases(def), ]
## probably will need to do more rigorous examination of assumptions; for now let's just assume it's OK

m1 <- lm(data=def2, formula = KTEA_Raw~Matrices_Raw*LW_Raw*WA_Raw*PPVT_raw)
summary(m1)
def2$KTEA_Raw <- as.numeric(def2$KTEA_Raw)
plot(def2$KTEA_Raw,m1$fitted.values)

unstandardizedPredicted <- predict(m1)
standardizedPredicted <- (unstandardizedPredicted - mean(unstandardizedPredicted)) / sd(unstandardizedPredicted)

m2 <- lm(def2$KTEA_Raw~standardizedPredicted)
summary(m2)
ggplot(def2, 
       aes(x=standardizedPredicted, y=def2$KTEA_Raw)) +
       geom_point() +
      geom_smooth(method = lm, level=0.65)   

ggplot(def2, 
       aes(x=standardizedPredicted, y=def2$KTEA_Raw)) +
  geom_point() +
  geom_smooth(method = lm, level=0.80)  

ci <- predict(m1, level=.8, interval="confidence") 
ci2 <- predict(m1, level=.2, interval="confidence") 

ci_all <- data.frame(def2$ID, def2$KTEA_Raw, ci[, "fit"], ci[, "lwr"], ci[, "upr"])
names(ci_all) <- c("sub","KTEA_Raw","fit","lwr","upr")
ci_all[,"CI_Group"] <- NA

ci_all[(ci_all$KTEA_Raw>ci_all$upr), "CI_Group"] <- "UGC"
ci_all[(ci_all$KTEA_Raw<ci_all$lwr), "CI_Group"] <- "UPC"
ci_all[(ci_all$KTEA_Raw>ci_all$lwr & ci_all$KTEA_Raw<ci_all$upr), "CI_Group"] <- "EGC"

write.table(ci_all, "CI_Analysis1.txt")

def3 <- data.frame(def2, ci_all$CI_Group)

kteam <- mean(def3$KTEA_Raw)
kteasd <- sd(def3$KTEA_Raw)
wam <- mean(def3$WA_Raw)
wasd <- sd(def3$WA_Raw)

ktean <- def3$KTEA_Raw - kteam
kteao <- ktean/kteasd

wan <- def3$WA_Raw - wam
wao <- wan/wasd

disc <- kteao - wao

def4 <- data.frame(def3, kteao, wao, disc)
def4[,"Disc_Group"] <- NA
def4[(def4$disc>-1 & def4$disc<1), "Disc_Group"] <- "TD"
def4[(def4$disc>1), "Disc_Group"] <- "DYS"
def4[(def4$disc<=-1), "Disc_Group"] <- "PC"

write.table(def4, "CI_Analysis2.txt")
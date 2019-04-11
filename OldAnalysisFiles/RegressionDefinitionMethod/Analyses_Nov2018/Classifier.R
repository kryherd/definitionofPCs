# read in libraries, data
library(fBasics)
library(caret)
library(MASS)
library(ggplot2)
library(plyr)
library(reshape2)
library(xtable)
setwd("~/Dropbox/Definition_of_PCs/RegressionDefinitionMethod/Analyses_Nov2018")
data <- read.csv("FullData_Sept27_2018.csv")
decoding <- read.csv("decoding_composite.csv")
dat2 <- merge(data, decoding, by = "SubjectID")

########## DATA IMPUTATION

dat3 <- dat2 %>%
  dplyr::select(SubjectID, Project,
         #age.mri, 
         age.beh, ppvt.raw, wasi.matr.raw, wj3.watt.raw,
         wj3.wid.raw, towre.w.ipm, towre.nw.ipm, 
         ktea2.raw, 
         #gm.rcomp.raw,
         #nd.rcomp.raw,
         wj3.rcomp.raw,
         decode_composite)

# missForest -- max iterations of 20
missForest <- missForest(dat3[,-c(1,2)], maxiter = 20)
imputed_data <- missForest$ximp
imputed_data <- cbind(dat3$SubjectID, imputed_data)
colnames(imputed_data)[1] <- "SubjectID"

extra_info <- dat2 %>%
  dplyr::select(SubjectID, handedness, StructuralMRI.ID)

### Define functions
gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}
ci2z <- function(ci)
{
  qnorm(ci + (1 - ci)/2)
}

#### Check normality of predictors and reading comp measures, transform
normality_test <- c("wj3.rcomp.raw", "ktea2.raw", "wasi.matr.raw", "ppvt.raw", "decode_composite")
for (measure in normality_test){
  cat(measure, "\n \n")
  dago.test <- eval(substitute(dagoTest(imputed_data$x), list(x = as.name(measure))))
  print(dago.test)
  cat("\n -------------------- \n")
}

## all reading comp measures need to be transformed
vars_tf <- normality_test
pp_md_tf <- preProcess(imputed_data[,vars_tf], method = c("center", "scale", "YeoJohnson"), na.remove=T)

#check the lambda values associated with the measures to be transformed
for (name in vars_tf){
  #q <- name
  lambda <- eval(substitute(pp_md_tf$yj$x$lambda, list(x = as.name(name))))
  cat(name, " : ", lambda, "\n")
}

# transform data
tf_data <- predict(pp_md_tf, imputed_data[,vars_tf])
# rename columns to include .tf instead of .raw
names(tf_data) <- data.frame(sapply(names(tf_data), gsub, pattern = "raw", replacement = "tf"))[,1]

normality_test2 <- names(tf_data)
for (measure in normality_test2){
  cat(measure, "\n \n")
  dago.test <- eval(substitute(dagoTest(tf_data$x), list(x = as.name(measure))))
  print(dago.test)
  cat("\n -------------------- \n")
}

# skewness is better, but still there.

# center age
imputed_data$age.c <- imputed_data$age.beh - mean(imputed_data$age.beh, na.rm = TRUE)
# center & scale matrix reasoning and ppvt
#dat2$wasi.matr.cs <- scale(dat2$wasi.matr.raw)
#dat2$ppvt.cs <- scale(dat2$ppvt.raw)

# create data frame for the model

#dat3 <- dat2[,c(1,20, 19, 21, 22)]
model_dat <- cbind(imputed_data$SubjectID, imputed_data$age.c, tf_data)
colnames(model_dat)[1] <- "SubjectID"
colnames(model_dat)[2] <- "age.c"

#### Run Models

comp.vars <- c("wj3.rcomp.tf", "ktea2.tf")
modelList <- list()
for (i in 1:length(comp.vars)){
  model <- eval(substitute(lm(x ~ age.c + decode_composite + wasi.matr.tf + ppvt.tf, data = model_dat), list(x = as.name(comp.vars[i]))))
  modelList[[i]] <- model
}
names(modelList) <- c("WJ3", "KTEA")
### Check models -- plots

for (i in 1:length(modelList)){
  jpeg(paste0(names(modelList[i]),"modelfit.jpg"), width = 1400, height = 500)
    par(mfrow=c(1,4))
    plot(residuals(modelList[[i]]))
    title(paste(names(modelList)[i], "\nSimple Residual Plot"))
    acf(residuals(modelList[[i]]), main = "")
    title(paste(names(modelList)[i], "\nResidual Autocorrelation Plot"))
    plot(fitted(modelList[[i]]), residuals(modelList[[i]]))
    title(paste(names(modelList)[i], "\nResidual vs Fit. value"))
    plot(modelList[[i]]$model[,1], residuals(modelList[[i]]) )
    title(paste(names(modelList)[i], "Residual vs Obs. value"))
  dev.off()
}

### Check for Influential Points
#influentialList <- list()
#par(mfrow=c(1,1))
#for (i in 1:length(modelList)){
#  cooksd <- cooks.distance(modelList[[i]])
#  plot(cooksd, pch="*", cex=2, main=paste(names(modelList[i]), "\nInfluential Obs by Cooks distance"))  # plot cook's distance
#  abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
#  text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red") # add labels
#}

# 73 influential for all, 234 & 668 influential for KTEA, GM, ND
# remove these data points
#dat5 <- dat4[-c(73,753),]

# re-run models
#comp.vars <- c("wj3.rcomp.tf", "ktea2.tf", "nd.rcomp.tf", "gm.rcomp.tf")
#modelList2 <- list()
#for (i in 1:length(comp.vars)){
#  model <- eval(substitute(lm(x ~ age.c + decoding_comp + wasi.matr.cs + ppvt.cs, data = dat5), list(x = as.name(comp.vars[i]))))
#  modelList2[[i]] <- model
#}
#names(modelList2) <- c("WJ3", "KTEA", "Nelson-Denny", "Gates-McGinitie")
### Check models -- plots

#for (i in 1:length(modelList2)){
#  jpeg(paste0(names(modelList2[i]),"modelfit2.jpg"), width = 1400, height = 500)
#  par(mfrow=c(1,4))
#  plot(residuals(modelList2[[i]]))
#  title(paste(names(modelList2)[i], "\nSimple Residual Plot"))
#  acf(residuals(modelList2[[i]]), main = "")
#  title(paste(names(modelList2)[i], "\nResidual Autocorrelation Plot"))
#  plot(fitted(modelList2[[i]]), residuals(modelList2[[i]]))
#  title(paste(names(modelList2)[i], "\nResidual vs Fit. value"))
#  plot(modelList2[[i]]$model[,1], residuals(modelList2[[i]]) )
#  title(paste(names(modelList2)[i], "Residual vs Obs. value"))
#  dev.off()
#}


#### Create Groups

# get predicted values and standardize them
resultsList <- list()
for (i in 1:length(modelList)){
  pred <- unlist(predict(modelList[i]))
  std.pred <- scale(pred)
  resid <- modelList[[i]]$residuals
  df <- data.frame(pred,std.pred,resid)
  resultsList[[i]] <- df
  rownames(resultsList[[i]]) <- 1:826
}
names(resultsList) <- c("WJ3", "KTEA")
list2env(resultsList,envir=.GlobalEnv)


grouping <- c("UPC", "NSC", "EAC", "NSC", "UGC")
# assign people to groups
## change here if you want to add more or different CIs
ciList <- list(c(.10, .70), c(.15,.65), c(.15,.60), c(.15,.70), c(.20,.65), c(.20,.70))
allList <- list()
for (i in 1:length(ciList)){
  ci.to.use <- ciList[[i]]
  groupsList <- list()
  for (model in names(resultsList)){
    my.bin <- eval(substitute(c(range(scale(i$resid))+.1, ci2z(ci.to.use), -ci2z(ci.to.use)), list(i = as.name(model))))
    my.bin <- sort(my.bin)
    groups <- eval(substitute(grouping[findInterval(scale(i$resid), vec=my.bin, all.inside=T)],list(i = as.name(model))))
    x <- as.data.frame(groups)
    y <- cbind(imputed_data$SubjectID, x)
    groupsList[[model]] <- cbind(eval(as.name(model)), y)
    colnames(groupsList[[model]])[4] <- c("SubjectID")
    groupsList[[model]]$groups <- as.character(groupsList[[model]]$groups)
    groupsList[[model]]$groups[groupsList[[model]]$std.pred < -1] <- "EPC"
  }
  allList[[i]] <- groupsList
}
names(allList) <- c("10_70", "15_65", "15_60", "15_70", "20_65", "20_70")


#### Merge with MRI IDs

mri <- read.csv("Age_Gender_Handedness_MRI.csv")

newList <- list()
tableList <- list()
for (i in 1:length(ciList)){
  mList <- list()
  tList <- list()
  for (model in names(resultsList)){
    rel_subs <- eval(substitute(subset(allList[[i]]$x, groups != "EPC"), list(x = as.name(model))))
    rel_subs <- subset(rel_subs, groups != "NSC")
    rel_subs$groups <- factor(rel_subs$groups)
    mri_subs <- merge(rel_subs, mri, by = "SubjectID", all = TRUE)
    mri_beh <- merge(mri_subs, imputed_data, by = "SubjectID", all = TRUE)
    all <- mri_beh[complete.cases(mri_beh$StructuralMRI.ID),]
    all2 <- all[complete.cases(all$groups),]
    all3 <- all2[all2$handedness == "right",]
    all3 <- all3[complete.cases(all3$handedness),]
    mList[[model]] <- all3
    table <- table(all3$groups)
    tList[[model]] <- table
  }
  newList[[i]] <- mList
  tableList[[i]] <- tList
}
names(newList) <- c("10_70", "15_65", "15_60", "15_70", "20_65", "20_70")
names(tableList) <- c("10_70", "15_65", "15_60", "15_70", "20_65", "20_70")

# write out how many people are in each group
pList <- list()
for (i in 1:length(tableList)){
  table.results <- data.frame(do.call("rbind", tableList[[i]]))
  RC_Measure <- rownames(table.results)
  table.results <- cbind(RC_Measure, table.results)
  pList[[i]] <- table.results
}
names(pList) <- c("10_70", "15_65", "15_60", "15_70", "20_65", "20_70")

table.results <- data.frame(do.call("rbind", pList))
CIs <- substring(rownames(table.results),1,5)
table.results <- cbind(CIs, table.results)
tables <- table.results

write.csv(tables, "GroupSizes.csv", row.names = FALSE)

#### Checking Group Differences
#mergeList <- list()
# merge in raw scores
#for (i in 1:length(newList)){
#  mrgList <- list()
#  for (j in 1:length(newList[[i]])){
#    a <- dat2[,c(1,8,9,10,11,12,13,18,19)]
#    b <- newList[[i]][[j]]
#    mrg <- merge(a,b, by = "SubjectID", all.x = FALSE, all.y = TRUE)
#    mrgList[[j]] <- mrg
#  }
# names(mrgList) <- c("WJ3", "KTEA")
# mergeList[[i]] <- mrgList 
#}
#names(mergeList) <- c("10_70", "15_65", "15_60", "15_70", "20_65", "20_70")

gdifm <- vector(mode = "character")
ps <- vector(mode = "numeric")
group.diffs <- list("age.beh", "wj3.rcomp.raw", "ktea2.raw", "wasi.matr.raw", "ppvt.raw", "decode_composite")
group.diffs.list <- list()
for (i in 1:length(ciList)){
  gdList <- list()
  for (model in names(resultsList)){
    ares <- vector()
    for (j in group.diffs){
      form <- substitute(j ~ groups, list(j = as.name(j)))
      form <- as.formula(form)
      a1 <- eval(substitute(aov(form, data = newList[[i]]$x), list(x = as.name(model))))
      p.val <- summary(a1)[[1]][[1,"Pr(>F)"]]
      row <- list(j, p.val)
      ares <- rbind(ares,row)
      }
  ares <- as.data.frame(ares)
  ares$RC_measure <- model
  colnames(ares)[1] <- "GroupDiff_measure"
  colnames(ares)[2] <- "p"
  ares$p<- as.numeric(as.character(ares$p))
  ares$Sig[ares$p < .05] <- "*"
  gdList[[model]] <- ares
  }
  group.diffs.list[[i]] <- gdList
}
names(group.diffs.list) <- c("10_70", "15_65", "15_60", "15_70", "20_65", "20_70")

gdList <- list()
for (i in 1:length(group.diffs.list)){
  table.results <- data.frame(do.call("rbind", group.diffs.list[[i]]))
  RC_Measure <- gsub("\\..*$", "", rownames(table.results))
  gdList[[i]] <- table.results
}
names(gdList) <- c("10_70", "15_65", "15_60", "15_70", "20_65", "20_70")


g.diffs <- data.frame(do.call("rbind", gdList))
CIs <- substring(rownames(g.diffs),1,5)
g.diffs<- cbind(CIs, g.diffs)

g.diffs <- as.data.frame(g.diffs)
g.diffs <- g.diffs[,c(1,4,2,3,5)]

## Create summary file
library(xlsx)
write.xlsx(tables, file="summary.xlsx", sheetName="GroupSizes", row.names=FALSE, showNA = FALSE)
write.xlsx(subset(g.diffs, GroupDiff_measure == "age.beh"), file="summary.xlsx", sheetName="Age", append=TRUE, row.names=FALSE, showNA = FALSE)
write.xlsx(subset(g.diffs, GroupDiff_measure == "wj3.rcomp.raw"), file="summary.xlsx", sheetName="WJ3", append=TRUE, row.names=FALSE, showNA = FALSE)
write.xlsx(subset(g.diffs, GroupDiff_measure == "ktea2.raw"), file="summary.xlsx", sheetName="KTEA", append=TRUE, row.names=FALSE, showNA = FALSE)
write.xlsx(subset(g.diffs, GroupDiff_measure == "wasi.matr.raw"), file="summary.xlsx", sheetName="Matrix Reasoning", append=TRUE, row.names=FALSE, showNA = FALSE)
write.xlsx(subset(g.diffs, GroupDiff_measure == "ppvt.raw"), file="summary.xlsx", sheetName="PPVT", append=TRUE, row.names=FALSE, showNA = FALSE)
write.xlsx(subset(g.diffs, GroupDiff_measure == "decode_composite"), file="summary.xlsx", sheetName="Decoding Composite", append=TRUE, row.names=FALSE, showNA = FALSE)

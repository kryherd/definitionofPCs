######## ORGANIZING DATA
setwd("~/Dropbox/Definition_of_PCs/RegressionDefinitionMethod/FreesurferResultsJan2018")
# read in packages
library(xlsx)
library(phia)
library(multcomp)
library(DTK)

beh <- read.csv("FullData_FinalMay23_2017.csv")
age <- data.frame(beh$SubjectID, beh$StructuralMRI.ID, beh$age.mri)
names(age) <- c("SubID", "MRI.ID", "age.mri")
# read all of the sheets in to a list of dataframes
volumes <- list(3,4,7,8,11,12,15,16)
VolumeList <- list()
for (i in volumes) {
  new <- read.xlsx("CorticalStats_VolumeThickness_WJ.xlsx", i) 
  VolumeList[[i]] <- new
}
VL <- VolumeList[unlist(lapply(VolumeList,length)!=0)] 

s2vol <- list(1,3,5,7)
S2VolumeList <- list()
for (i in s2vol) {
  new <- read.xlsx("S2_Volumetric_Results.xlsx", i) 
  S2VolumeList[[i]] <- new
}
S2L <- S2VolumeList[unlist(lapply(S2VolumeList,length)!=0)] 


thickness <- list(1,2,5,6,9,10,13,14)
ThickList <- list()
for (i in thickness) {
  new <- read.xlsx("CorticalStats_VolumeThickness_WJ.xlsx", i) 
  ThickList[[i]] <- new
}
TL <- ThickList[unlist(lapply(ThickList,length)!=0)] 

s2thick <- list(2,4,6,8)
S2ThickList <- list()
for (i in s2thick) {
  new <- read.xlsx("S2_Volumetric_Results.xlsx", i) 
  S2ThickList[[i]] <- new
}
S2TL <- S2ThickList[unlist(lapply(S2ThickList,length)!=0)] 

# give each dataframe in the list its name
names <- read.csv("CorticalStats_names.csv", header = FALSE)
nams <- names[c(3,4,7,8,11,12,15,16), 1]
names(VL) <-nams
nams <- names[c(1,2,5,6,9,10,13,14), 1]
names(TL) <-nams
names <- read.csv("S2CorticalStats_names.csv", header = FALSE)
nams <- names[c(1,3,5,7),1]
names(S2L) <- nams
nams <- names[c(2,4,6,8),1]
names(S2TL) <- nams

## merge Vol with S2L Vol

LeftList <- VL[c(1,3,5,7)]
s2mergeListvol <- list()
for (i in 1:length(LeftList)) {
  rel_col <- S2L[[i]][,c(1,4)]
  merge <- merge(LeftList[[i]], rel_col, by = "Subject" , all = TRUE)
  s2mergeListvol[[i]] <- merge
}
names <- read.csv("CorticalStats_names.csv", header = FALSE)
nams <- names[c(3,7,11,15),1]
names(s2mergeListvol) <- nams

## merge thick with S2L thick
LeftList <- TL[c(1,3,5,7)]
s2mergeListthick <- list()
for (i in 1:length(LeftList)) {
  rel_col <- S2TL[[i]][,c(1,4)]
  merge <- merge(LeftList[[i]], rel_col, by = "Subject" , all = TRUE)
  s2mergeListthick[[i]] <- merge
}
nams <- names[c(1,5,9,13),1]
names(s2mergeListthick) <- nams

allThick <- append(s2mergeListthick, TL[c(2,4,6,8)])
allVol <- append(s2mergeListvol, VL[c(2,4,6,8)])

VL <- allVol
TL <- allThick


# Pulling out covariates
for (i in 1:8) {
  VL[[i]]$Group <- VL[[i]]$Class
  TL[[i]]$Group <- TL[[i]]$Class
}

ICV <- read.csv("ICV.csv")
ICV$ICV.demean <- ICV$ICV - mean(ICV$ICV, na.rm=T)
ICV <- ICV[,-1]
mergeListVol <- list()
for (i in 1:length(VL)) {
  some <- VL[[i]][substr(VL[[i]]$Subject,1,1) == "h",]
  some$Subject2 <- substr(some$Subject, 2, 5)
  merged1 <- merge(some, age, by.x = "Subject2", by.y = "SubID", all.x = TRUE)
  merged1 <- merged1[, !names(merged1) %in% c("Subject2","MRI.ID")]
  others <- VL[[i]][substr(VL[[i]]$Subject,1,1) == "t",]
  merged2 <- merge(others, age, by.x = "Subject", by.y = "MRI.ID", all.x = TRUE)
  merged2 <- merged2[, !names(merged2) %in% c("SubID")]
  merge_total <- rbind(merged1, merged2)
  mergeListVol[[i]] <- merge_total
}


allVol <- list()
for (i in 1:length(mergeListVol)) {
  merged <- merge(mergeListVol[[i]], ICV, by = "Subject", all.x = TRUE)
  allVol[[i]] <- merged
  allVol[[i]] <- allVol[[i]][rowSums(allVol[[i]][,3:34]) != 0,]
} 
names(allVol) <- names(VL)

for (i in 1:length(TL)) {
  TL[[i]]$MeanThick_demeaned <- TL[[i]][,grep("MeanThickness", colnames(TL[[1]]))] - mean(TL[[i]][,grep("MeanThickness", colnames(TL[[1]]))], na.rm = TRUE)
}

mergeListThic <- list()
for (i in 1:length(TL)) {
  some <- TL[[i]][substr(TL[[i]]$Subject,1,1) == "h",]
  some$Subject2 <- substr(some$Subject, 2, 5)
  merged1 <- merge(some, age, by.x = "Subject2", by.y = "SubID", all.x = TRUE)
  merged1 <- merged1[, !names(merged1) %in% c("Subject2","MRI.ID")]
  others <- TL[[i]][substr(TL[[i]]$Subject,1,1) == "t",]
  merged2 <- merge(others, age, by.x = "Subject", by.y = "MRI.ID", all.x = TRUE)
  merged2 <- merged2[, !names(merged2) %in% c("SubID")]
  merge_total <- rbind(merged1, merged2)
  mergeListThic[[i]] <- merge_total
  mergeListThic[[i]] <- mergeListThic[[i]][rowSums(mergeListThic[[i]][,3:36]) != 0,]
}
names(mergeListThic) <- names(TL)



##################### ANALYZING DATA





#run models for LH volume
lhvol <- allVol[c(1:4)]
for (i in 1:length(lhvol)) {
  lhvol[[i]]$Group <- as.factor(lhvol[[i]]$Group)
  sink(paste0("volumeresults-",names(lhvol[i]), ".txt"))
  volumeVars = names(lhvol[[i]][c(5:38,40)])
  models = lapply( volumeVars, function(x) { lm(substitute( j ~  ICV.demean + age.mri + Group, list(j=as.name(x)) ), data=lhvol[[i]] ) } )
  lapply(models, function(y) { print(anova(y)); postHocs <- glht(y, linfct = mcp(Group = "Tukey")); print(summary(postHocs));
  cat("Tukey-Kramer Test; if 0 is in the CI, no sig. difference \n")
  q <- names(y$model[1]);TK <- DTK.test(eval(parse(text = paste("lhvol[[i]]$", as.name(q), sep = ""))), lhvol[[i]]$Class); print(TK[[2]])})
  sink()
}


#run models for LH volume
rhvol <- allVol[c(5:8)]
for (i in 1:length(rhvol)) {
  rhvol[[i]]$Group <- as.factor(rhvol[[i]]$Group)
  sink(paste0("volumeresults-",names(rhvol[i]), ".txt"))
  volumeVars = names(rhvol[[i]][c(5:38)])
  models = lapply( volumeVars, function(x) { lm(substitute( j ~  ICV.demean + age.mri + Group, list(j=as.name(x)) ), data=rhvol[[i]] ) } )
  lapply(models, function(y) { print(anova(y)); postHocs <- glht(y, linfct = mcp(Group = "Tukey")); print(summary(postHocs));
  cat("Tukey-Kramer Test; if 0 is in the CI, no sig. difference \n")
  q <- names(y$model[1]);TK <- DTK.test(eval(parse(text = paste("rhvol[[i]]$", as.name(q), sep = ""))), rhvol[[i]]$Class); print(TK[[2]])})
  sink()
}


# run LH models for thickness
lhthic <- mergeListThic[1:4]
for (i in 1:length(lhthic)) {
  lhthic[[i]]$Group <- as.factor(lhthic[[i]]$Group)
  sink(paste0("thicknessresults-",names(lhthic[i]), ".txt"))
  thickVars = names(lhthic[[i]][c(4:38,41)])
  models = lapply( thickVars, function(x) { lm(substitute( j ~  MeanThick_demeaned + age.mri + Group, list(j=as.name(x)) ), data=lhthic[[i]] ) } )
  lapply(models, function(y) { print(anova(y)); postHocs <- glht(y, linfct = mcp(Group = "Tukey")); print(summary(postHocs));
  cat("Tukey-Kramer Test; if 0 is in the CI, no sig. difference \n")
  q <- names(y$model[1]);TK <- DTK.test(eval(parse(text = paste("lhthic[[i]]$", as.name(q), sep = ""))), lhthic[[i]]$Class); print(TK[[2]])})
  sink()
}

# run RH models for thickness
rhthic <- mergeListThic[5:8]
for (i in 1:length(rhthic)) {
  rhthic[[i]]$Group <- as.factor(rhthic[[i]]$Group)
  sink(paste0("thicknessresults-",names(rhthic[i]), ".txt"))
  thickVars = names(rhthic[[i]][c(4:38)])
  models = lapply( thickVars, function(x) { lm(substitute( j ~  MeanThick_demeaned + age.mri + Group, list(j=as.name(x)) ), data=rhthic[[i]] ) } )
  lapply(models, function(y) { print(anova(y)); postHocs <- glht(y, linfct = mcp(Group = "Tukey")); print(summary(postHocs));
  cat("Tukey-Kramer Test; if 0 is in the CI, no sig. difference \n")
  q <- names(y$model[1]);TK <- DTK.test(eval(parse(text = paste("rhthic[[i]]$", as.name(q), sep = ""))), rhthic[[i]]$Class); print(TK[[2]])})
  sink()
}


################### PLOTS

lh.vol.data <- data.frame(allVol[[3]])
rh.vol.data <- data.frame(allVol[[4]])

library(plyr)
library(ggplot2)
library(ggsignif)
grh.po<-ddply(rh.vol.data, c("Group"), summarize,
              AVERAGE=mean(rh_parsopercularis_volume, na.rm = TRUE),
              SE=sqrt(var(rh_parsopercularis_volume, na.rm = TRUE)/length(rh_parsorbitalis_volume)))

p1 <- ggplot(rh.vol.data, aes(Group, rh_parsopercularis_volume)) + geom_violin(fill = "#FF7F00") +
  xlab("Group") + ylab("Gray Matter Volume") +
  theme(title = element_text(size = 20), text = element_text(size = 18)) + 
  geom_point(aes(y = AVERAGE), size = 2, position = position_dodge((width = 0.90)), data = grh.po) +
  ggtitle("p. Opercularis") + 
  theme_bw() + 
  geom_signif(comparisons = list(c("UPC", "UGC")), annotations="*", tip_length = 0) + 
  geom_errorbar(aes(y = AVERAGE, ymin = AVERAGE-SE, ymax = AVERAGE+SE), 
                width = 0.20, position = position_dodge((width = 0.90)), data = grh.po) +
  ylim(NA, 8100) +
  theme(title = element_text(size = 30), text = element_text(size = 28), plot.background = element_rect(fill = "#EAE0CC"))
p1

grh.pt<-ddply(rh.vol.data, c("Group"), summarize,
              AVERAGE=mean(rh_parstriangularis_volume, na.rm = TRUE),
              SE=sqrt(var(rh_parstriangularis_volume, na.rm = TRUE)/length(rh_parstriangularis_volume)))

p1 <- ggplot(rh.vol.data, aes(Group, rh_parstriangularis_volume)) + geom_violin(fill = "#FB0009") +
  xlab("Group") + ylab("Gray Matter Volume") +
  theme(title = element_text(size = 20), text = element_text(size = 18)) + 
  geom_point(aes(y = AVERAGE), size = 2, position = position_dodge((width = 0.90)), data = grh.pt) +
  ggtitle("p. Triangularis") + 
  theme_bw() + 
  geom_signif(comparisons = list(c("UPC", "UGC")), annotations="*", tip_length = 0) + 
  geom_errorbar(aes(y = AVERAGE, ymin = AVERAGE-SE, ymax = AVERAGE+SE), 
                width = 0.20, position = position_dodge((width = 0.90)), data = grh.pt) +
  ylim(NA, 7000) +
  theme(title = element_text(size = 30), text = element_text(size = 28), plot.background = element_rect(fill = "#EAE0CC"))
p1

lh.thi.data <- data.frame(mergeListThic[[3]])
rh.thi.data <- data.frame(mergeListThic[[4]])

glh.spl<-ddply(lh.thi.data, c("Group"), summarize,
               AVERAGE=mean(lh_superiorparietal_thickness, na.rm = TRUE),
               SE=sqrt(var(lh_superiorparietal_thickness, na.rm = TRUE)/length(lh_superiorparietal_thickness)))

p1 <- ggplot(lh.thi.data, aes(Group, lh_superiorparietal_thickness)) + geom_violin(fill = "#B900FF") +
  xlab("Group") + ylab("Cortical Thickness") +
  theme(title = element_text(size = 20), text = element_text(size = 18)) + 
  geom_point(aes(y = AVERAGE), size = 2, position = position_dodge((width = 0.90)), data = glh.spl) +
  ggtitle("Superior Parietal Lobule") + 
  theme_bw() + 
  geom_signif(comparisons = list(c("UPC", "EAC")), annotations="*", tip_length = 0) + 
  geom_errorbar(aes(y = AVERAGE, ymin = AVERAGE-SE, ymax = AVERAGE+SE), 
                width = 0.20, position = position_dodge((width = 0.90)), data = glh.spl) +
  theme(title = element_text(size = 30), text = element_text(size = 28), plot.background = element_rect(fill = "#EAE0CC")) +
  ylim(NA, 2.7)
p1

grh.ins<-ddply(rh.thi.data, c("Group"), summarize,
               AVERAGE=mean(rh_insula_thickness, na.rm = TRUE),
               SE=sqrt(var(rh_insula_thickness, na.rm = TRUE)/length(rh_insula_thickness)))

p1 <- ggplot(rh.thi.data, aes(Group, rh_insula_thickness)) + geom_violin(fill = "#FFF30B") +
  xlab("Group") + ylab("Cortical Thickness") +
  theme(title = element_text(size = 20), text = element_text(size = 18)) + 
  geom_point(aes(y = AVERAGE), size = 2, position = position_dodge((width = 0.90)), data = grh.ins) +
  ggtitle("Insula") + 
  theme_bw() + 
  geom_errorbar(aes(y = AVERAGE, ymin = AVERAGE-SE, ymax = AVERAGE+SE), 
                width = 0.20, position = position_dodge((width = 0.90)), data = grh.ins) +
  theme(title = element_text(size = 30), text = element_text(size = 28), plot.background = element_rect(fill = "#EAE0CC")) +
  geom_signif(comparisons = list(c("UPC", "EAC")), annotations="*", tip_length = 0, y_position = 3.72) + 
  geom_signif(comparisons = list(c("EAC", "UGC")), annotations="*", tip_length = 0, y_position = 3.62) +
  ylim(NA,3.79)
p1





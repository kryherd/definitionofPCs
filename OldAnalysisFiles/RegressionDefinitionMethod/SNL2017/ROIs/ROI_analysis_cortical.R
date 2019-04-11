######## ORGANIZING DATA
setwd("~/Dropbox/Definition_of_PCs/RegressionDefinitionMethod/SNL2017/ROIs")
# read in packages
library(xlsx)
library(phia)
library(multcomp)

beh <- read.csv("FullData_FinalMay23_2017.csv")
age <- data.frame(beh$SubjectID, beh$StructuralMRI.ID, beh$age.mri)
names(age) <- c("SubID", "MRI.ID", "age.mri")
# read all of the sheets in to a list of dataframes
volumes <- list(1,2,5,6,9,10,13,14,17,18)
VolumeList <- list()
for (i in volumes) {
new <- read.xlsx("CorticalStats_VolumeThickness.xlsx", i) 
VolumeList[[i]] <- new
}
VL <- VolumeList[unlist(lapply(VolumeList,length)!=0)] 

thickness <- list(3,4,7,8,11,12,15,16,19,20)
ThickList <- list()
for (i in thickness) {
  new <- read.xlsx("CorticalStats_VolumeThickness.xlsx", i) 
  ThickList[[i]] <- new
}
TL <- ThickList[unlist(lapply(ThickList,length)!=0)] 

# give each dataframe in the list its name
names <- read.csv("CorticalStats_names.csv", header = FALSE)
nams <- names[c(1,2,5,6,9,10,13,14,17,18), 1]
names(VL) <-nams
nams <- names[c(3,4,7,8,11,12,15,16,19,20), 1]
names(TL) <-nams

# Pulling out covariates
for (i in 1:10) {
 VL[[i]]$Group <- substr(VL[[i]]$Class, 1, 3)
 VL[[i]]$Sex <- substr(VL[[i]]$Class, 4, 4)
 TL[[i]]$Group <- substr(VL[[i]]$Class, 1, 3)
 TL[[i]]$Sex <- substr(VL[[i]]$Class, 4, 4)
}

ICV <- read.csv("ICV.csv")
ICV$ICV.demean <- ICV$ICV - mean(ICV$ICV, na.rm=T)
ICV <- ICV[,-1]
mergeListVol <- list()
for (i in 1:length(VL)) {
  some <- VL[[i]][substr(VL[[i]]$Subject,1,1) == "h",]
  some$Subject2 <- substr(some$Subject, 2, 5)
  merged1 <- merge(some, age, by.x = "Subject2", by.y = "SubID", all.x = TRUE)
  merged1 <- merged1[, -c(1, 42)]
  others <- VL[[i]][substr(VL[[i]]$Subject,1,1) == "t",]
  merged2 <- merge(others, age, by.x = "Subject", by.y = "MRI.ID", all.x = TRUE)
  merged2 <- merged2[,-41]
  merge_total <- rbind(merged1, merged2)
  mergeListVol[[i]] <- merge_total
}

allVol <- list()
for (i in 1:length(mergeListVol)) {
  merged <- merge(mergeListVol[[i]], ICV, by = "Subject", all.x = TRUE)
  allVol[[i]] <- merged
  allVol[[i]] <- allVol[[i]][rowSums(allVol[[i]][,3:34]) != 0,]
} 
nams <- names[c(1,2,5,6,9,10,13,14,17,18), 1]
names(allVol) <-nams

for (i in 1:length(TL)) {
  TL[[i]]$MeanThick_demeaned <- TL[[i]][,grep("MeanThickness", colnames(TL[[1]]))] - mean(TL[[i]][,grep("MeanThickness", colnames(TL[[1]]))])
}

mergeListThic <- list()
for (i in 1:length(TL)) {
  some <- TL[[i]][substr(TL[[i]]$Subject,1,1) == "h",]
  some$Subject2 <- substr(some$Subject, 2, 5)
  merged1 <- merge(some, age, by.x = "Subject2", by.y = "SubID", all.x = TRUE)
  merged1 <- merged1[, -c(1, 44)]
  others <- TL[[i]][substr(TL[[i]]$Subject,1,1) == "t",]
  merged2 <- merge(others, age, by.x = "Subject", by.y = "MRI.ID", all.x = TRUE)
  merged2 <- merged2[,-43]
  merge_total <- rbind(merged1, merged2)
  mergeListThic[[i]] <- merge_total
  mergeListThic[[i]] <- mergeListThic[[i]][rowSums(mergeListThic[[i]][,3:36]) != 0,]
}
nams <- names[c(3,4,7,8,11,12,15,16,19,20), 1]
names(mergeListThic) <-nams



##################### ANALYZING DATA

#run models for volume

for (i in 1:length(allVol)) {
  allVol[[i]]$Group <- as.factor(allVol[[i]]$Group)
  sink(paste0("volumeresults-",names(allVol[i]), ".txt"))
  volumeVars = names(allVol[[i]][c(19, 21, 30, 36)])
  models = lapply( volumeVars, function(x) { lm(substitute( j ~  ICV.demean + Sex + age.mri + Group, list(j=as.name(x)) ), data=allVol[[i]] ) } )
  lapply(models, function(y) { print(anova(y)); postHocs <- glht(y, linfct = mcp(Group = "Tukey")); print(summary(postHocs))})
  sink()
}

# run models for thickness

for (i in 1:length(mergeListThic)) {
  mergeListThic[[i]]$Group <- as.factor(mergeListThic[[i]]$Group)
  sink(paste0("thicknessresults-",names(mergeListThic[i]), ".txt"))
  thickVars = names(mergeListThic[[i]][c(19, 21, 30, 36)])
  models = lapply( thickVars, function(x) { lm(substitute( j ~  MeanThick_demeaned + Sex + age.mri + Group, list(j=as.name(x)) ), data=mergeListThic[[i]] ) } )
  lapply(models, function(y) { print(anova(y)); postHocs <- glht(y, linfct = mcp(Group = "Tukey")); print(summary(postHocs))})
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





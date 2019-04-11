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


# Pulling out covariates
for (i in 1:8) {
  VL[[i]]$Group <- VL[[i]]$Class
  TL[[i]]$Group <- TL[[i]]$Class
}

# demean ICV
for (i in 1:length(VL)) {
  VL[[i]]$ICV.demean <- VL[[i]]$EstimatedTotalIntraCranialVol - mean(VL[[i]]$EstimatedTotalIntraCranialVol, na.rm = TRUE)
}

# merge with age
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
names(mergeListVol) <- names(VL)

## merge Vol with S2L Vol

LeftList <- mergeListVol[c(1,3,5,7)]
s2mergeListvol <- list()
for (i in 1:length(LeftList)) {
  rel_col <- S2L[[i]][,c(1,4)]
  merge <- merge(LeftList[[i]], rel_col, by = "Subject" , all = TRUE)
  s2mergeListvol[[i]] <- merge
}
names <- read.csv("CorticalStats_names.csv", header = FALSE)
nams <- names[c(3,7,11,15),1]
names(s2mergeListvol) <- nams

allVol <- append(s2mergeListvol, mergeListVol[c(2,4,6,8)])

## demean thickness

for (i in 1:length(TL)) {
  TL[[i]]$MeanThick_demeaned <- TL[[i]][,grep("MeanThickness", colnames(TL[[1]]))] - mean(TL[[i]][,grep("MeanThickness", colnames(TL[[1]]))], na.rm = TRUE)
}

# merge with age

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

# merge thic with S2 thic

LeftList <- mergeListThic[c(1,3,5,7)]
s2mergeListthic <- list()
for (i in 1:length(LeftList)) {
  rel_col <- S2TL[[i]][,c(1,4)]
  merge <- merge(LeftList[[i]], rel_col, by = "Subject" , all = TRUE)
  s2mergeListthic[[i]] <- merge
}
names <- read.csv("CorticalStats_names.csv", header = FALSE)
nams <- names[c(1,5,9,13),1]
names(s2mergeListthic) <- nams

allThic <- append(s2mergeListthic, mergeListThic[c(2,4,6,8)])


##################### ANALYZING DATA


#run models for LH volume
lhvol <- allVol[c(1:4)]
for (i in 1:length(lhvol)) {
  lhvol[[i]]$Group <- as.factor(lhvol[[i]]$Group)
  sink(paste0("volumeresults-",names(lhvol[i]), ".txt"))
  volumeVars = names(lhvol[[i]][c(5:38,43)])
  models = lapply( volumeVars, function(x) { lm(substitute( j ~  ICV.demean + age.mri + Group, list(j=as.name(x)) ), data=lhvol[[i]] ) } )
  lapply(models, function(y) { 
    q <- names(y$model[1])
    cat("ROI: ", (as.name(q)), "\n \n")
    print(anova(y))
    cat("-------------------------\n")
    cat("Tukey Test for pairwise comparisons")
    postHocs <- glht(y, linfct = mcp(Group = "Tukey"))
    print(summary(postHocs))
    cat("-------------------------\n")
    cat("Pairwise comparisons (no correction)")
    postHocs2 <- glht(y, linfct = mcp(Group = "Tukey"))
    print(summary(postHocs2, test = adjusted("none")))
    cat("\n==========================\n\n")
    })
  sink()
}


#run models for RH volume
rhvol <- allVol[c(5:8)]
for (i in 1:length(rhvol)) {
  rhvol[[i]]$Group <- as.factor(rhvol[[i]]$Group)
  sink(paste0("volumeresults-",names(rhvol[i]), ".txt"))
  volumeVars = names(rhvol[[i]][c(5:38)])
  models = lapply( volumeVars, function(x) { lm(substitute( j ~  ICV.demean + age.mri + Group, list(j=as.name(x)) ), data=rhvol[[i]] ) } )
  lapply(models, function(y) { 
    q <- names(y$model[1])
    cat("ROI: ", (as.name(q)), "\n \n")
    print(anova(y))
    cat("-------------------------\n")
    cat("Tukey Test for pairwise comparisons")
    postHocs <- glht(y, linfct = mcp(Group = "Tukey"))
    print(summary(postHocs))
    cat("-------------------------\n")
    cat("Pairwise comparisons (no correction)")
    postHocs2 <- glht(y, linfct = mcp(Group = "Tukey"))
    print(summary(postHocs2, test = adjusted("none")))
    cat("\n==========================\n\n")
  })
  sink()
}


# run LH models for thickness
lhthic <- allThic[1:4]
for (i in 1:length(lhthic)) {
  lhthic[[i]]$Group <- as.factor(lhthic[[i]]$Group)
  sink(paste0("thicknessresults-",names(lhthic[i]), ".txt"))
  volumeVars = names(lhthic[[i]][c(4:37,44)])
  models = lapply( volumeVars, function(x) { lm(substitute( j ~  MeanThick_demeaned + age.mri + Group, list(j=as.name(x)) ), data=lhthic[[i]] ) } )
  lapply(models, function(y) { 
    q <- names(y$model[1])
    cat("ROI: ", (as.name(q)), "\n \n")
    print(anova(y))
    cat("-------------------------\n")
    cat("Tukey Test for pairwise comparisons")
    postHocs <- glht(y, linfct = mcp(Group = "Tukey"))
    print(summary(postHocs))
    cat("-------------------------\n")
    cat("Pairwise comparisons (no correction)")
    postHocs2 <- glht(y, linfct = mcp(Group = "Tukey"))
    print(summary(postHocs2, test = adjusted("none")))
    cat("\n==========================\n\n")
  })
  sink()
}

# run RH models for thickness
rhthic <- allThic[5:8]
for (i in 1:length(rhthic)) {
  rhthic[[i]]$Group <- as.factor(rhthic[[i]]$Group)
  sink(paste0("thicknessresults-",names(rhthic[i]), ".txt"))
  volumeVars = names(rhthic[[i]][c(4:37,43)])
  models = lapply( volumeVars, function(x) { lm(substitute( j ~  MeanThick_demeaned + age.mri + Group, list(j=as.name(x)) ), data=rhthic[[i]] ) } )
  lapply(models, function(y) { 
    q <- names(y$model[1])
    cat("ROI: ", (as.name(q)), "\n \n")
    print(anova(y))
    cat("-------------------------\n")
    cat("Tukey Test for pairwise comparisons")
    postHocs <- glht(y, linfct = mcp(Group = "Tukey"))
    print(summary(postHocs))
    cat("-------------------------\n")
    cat("Pairwise comparisons (no correction)")
    postHocs2 <- glht(y, linfct = mcp(Group = "Tukey"))
    print(summary(postHocs2, test = adjusted("none")))
    cat("\n==========================\n\n")
  })
  sink()
}

# run LH models for thickness - no mean thick
lhthic <- allThic[1:4]
for (i in 1:length(lhthic)) {
  lhthic[[i]]$Group <- as.factor(lhthic[[i]]$Group)
  sink(paste0("thicknessresults_noMeanThick-",names(lhthic[i]), ".txt"))
  volumeVars = names(lhthic[[i]][c(4:37,44)])
  models = lapply( volumeVars, function(x) { lm(substitute( j ~  age.mri + Group, list(j=as.name(x)) ), data=lhthic[[i]] ) } )
  lapply(models, function(y) { 
    q <- names(y$model[1])
    cat("ROI: ", (as.name(q)), "\n \n")
    print(anova(y))
    cat("-------------------------\n")
    cat("Tukey Test for pairwise comparisons")
    postHocs <- glht(y, linfct = mcp(Group = "Tukey"))
    print(summary(postHocs))
    cat("-------------------------\n")
    cat("Pairwise comparisons (no correction)")
    postHocs2 <- glht(y, linfct = mcp(Group = "Tukey"))
    print(summary(postHocs2, test = adjusted("none")))
    cat("\n==========================\n\n")
  })
  sink()
}

# run RH models for thickness - no mean thick
rhthic <- allThic[5:8]
for (i in 1:length(rhthic)) {
  rhthic[[i]]$Group <- as.factor(rhthic[[i]]$Group)
  sink(paste0("thicknessresults_NoMeanThick-",names(rhthic[i]), ".txt"))
  volumeVars = names(rhthic[[i]][c(4:37,43)])
  models = lapply( volumeVars, function(x) { lm(substitute( j ~  age.mri + Group, list(j=as.name(x)) ), data=rhthic[[i]] ) } )
  lapply(models, function(y) { 
    q <- names(y$model[1])
    cat("ROI: ", (as.name(q)), "\n \n")
    print(anova(y))
    cat("-------------------------\n")
    cat("Tukey Test for pairwise comparisons")
    postHocs <- glht(y, linfct = mcp(Group = "Tukey"))
    print(summary(postHocs))
    cat("-------------------------\n")
    cat("Pairwise comparisons (no correction)")
    postHocs2 <- glht(y, linfct = mcp(Group = "Tukey"))
    print(summary(postHocs2, test = adjusted("none")))
    cat("\n==========================\n\n")
  })
  sink()
}

################### PLOTS 

lh.vol.15_65 <- data.frame(allVol[[3]])
lh.vol.15_70 <- data.frame(allVol[[4]])
rh.vol.15_65 <- data.frame(allVol[[7]])
rh.vol.15_70 <- data.frame(allVol[[8]])

lh.vol.15_65$Group<- factor(lh.vol.15_65$Group,levels(lh.vol.15_65$Group)[c(3,1,2)])
rh.vol.15_65$Group<- factor(rh.vol.15_65$Group,levels(rh.vol.15_65$Group)[c(3,1,2)])
lh.vol.15_70$Group<- factor(lh.vol.15_70$Group,levels(lh.vol.15_70$Group)[c(3,1,2)])
rh.vol.15_70$Group<- factor(rh.vol.15_70$Group,levels(rh.vol.15_70$Group)[c(3,1,2)])


library(plyr)
library(ggplot2)
library(ggsignif)
library(gridExtra)
grh.is<-ddply(lh.vol.15_65, c("Group"), summarize,
             AVERAGE=mean(lh_isthmuscingulate_volume, na.rm = TRUE),
             SE=sqrt(var(lh_isthmuscingulate_volume, na.rm = TRUE)/length(lh_isthmuscingulate_volume)))

lhis_1565 <- ggplot(lh.vol.15_65, aes(Group, lh_isthmuscingulate_volume)) + geom_violin(fill = "gray") +
  xlab("Group") + ylab("Gray Matter Volume") +
  theme(title = element_text(size = 20), text = element_text(size = 18)) + 
  geom_point(aes(y = AVERAGE), size = 2, position = position_dodge((width = 0.90)), data = grh.is) +
  ggtitle("LH Isthmus Cingulate (15/65)") + 
  theme_bw() +
  geom_signif(comparisons = list(c("UPC", "UGC")), annotations="*", tip_length = 0.025) + 
  geom_errorbar(aes(y = AVERAGE, ymin = AVERAGE-SE, ymax = AVERAGE+SE), 
                width = 0.20, position = position_dodge((width = 0.90)), data = grh.is) +
  ylim(NA, 5000)
lhis_1565

grh.is<-ddply(lh.vol.15_70, c("Group"), summarize,
              AVERAGE=mean(lh_isthmuscingulate_volume, na.rm = TRUE),
              SE=sqrt(var(lh_isthmuscingulate_volume, na.rm = TRUE)/length(lh_isthmuscingulate_volume)))

lhis15_70 <- ggplot(lh.vol.15_70, aes(Group, lh_isthmuscingulate_volume)) + geom_violin(fill = "gray") +
  xlab("Group") + ylab("Gray Matter Volume") +
  theme(title = element_text(size = 20), text = element_text(size = 18)) + 
  geom_point(aes(y = AVERAGE), size = 2, position = position_dodge((width = 0.90)), data = grh.is) +
  ggtitle("LH Isthmus Cingulate (15/70)") + 
  theme_bw() +
  geom_signif(comparisons = list(c("UPC", "UGC")), annotations=".", tip_length = 0.025) + 
  geom_errorbar(aes(y = AVERAGE, ymin = AVERAGE-SE, ymax = AVERAGE+SE), 
                width = 0.20, position = position_dodge((width = 0.90)), data = grh.is) +
  ylim(NA, 5000) + 
  geom_signif(comparisons = list(c("EAC", "UGC")), annotations=".", tip_length = 0, y_position = 4900)
lhis15_70

grid.arrange(lhis_1565, lhis15_70, ncol=2)


grh.po<-ddply(lh.vol.15_65, c("Group"), summarize,
              AVERAGE=mean(lh_parsorbitalis_volume, na.rm = TRUE),
              SE=sqrt(var(lh_parsorbitalis_volume, na.rm = TRUE)/length(lh_parsorbitalis_volume)))

lhpo_1565 <- ggplot(lh.vol.15_65, aes(Group, lh_parsorbitalis_volume)) + geom_violin(fill = "gray") +
  xlab("Group") + ylab("Gray Matter Volume") +
  theme(title = element_text(size = 20), text = element_text(size = 18)) + 
  geom_point(aes(y = AVERAGE), size = 2, position = position_dodge((width = 0.90)), data = grh.po) +
  ggtitle("LH Pars Orbitalis (15/65)") + 
  theme_bw() +
  geom_signif(comparisons = list(c("UPC", "UGC")), annotations=".", tip_length = 0.025) +
  geom_signif(comparisons = list(c("EAC", "UGC")), annotations=".", tip_length = 0, y_position = 4400) + 
  geom_errorbar(aes(y = AVERAGE, ymin = AVERAGE-SE, ymax = AVERAGE+SE), 
                width = 0.20, position = position_dodge((width = 0.90)), data = grh.po) +
  ylim(NA, 4500)
lhpo_1565

grh.po<-ddply(lh.vol.15_70, c("Group"), summarize,
              AVERAGE=mean(lh_parsorbitalis_volume, na.rm = TRUE),
              SE=sqrt(var(lh_parsorbitalis_volume, na.rm = TRUE)/length(lh_parsorbitalis_volume)))

lhpo_1570 <- ggplot(lh.vol.15_70, aes(Group, lh_parsorbitalis_volume)) + geom_violin(fill = "gray") +
  xlab("Group") + ylab("Gray Matter Volume") +
  theme(title = element_text(size = 20), text = element_text(size = 18)) + 
  geom_point(aes(y = AVERAGE), size = 2, position = position_dodge((width = 0.90)), data = grh.po) +
  ggtitle("LH Pars Orbitalis (15/70)") + 
  theme_bw() +
  geom_signif(comparisons = list(c("UPC", "UGC")), annotations="*", tip_length = 0.025) +
  geom_signif(comparisons = list(c("EAC", "UGC")), annotations=".", tip_length = 0, y_position = 4400) + 
  geom_errorbar(aes(y = AVERAGE, ymin = AVERAGE-SE, ymax = AVERAGE+SE), 
                width = 0.20, position = position_dodge((width = 0.90)), data = grh.po) +
  ylim(NA, 4500)
lhpo_1570


grh.po<-ddply(rh.vol.15_65, c("Group"), summarize,
              AVERAGE=mean(rh_parsorbitalis_volume, na.rm = TRUE),
              SE=sqrt(var(rh_parsorbitalis_volume, na.rm = TRUE)/length(rh_parsorbitalis_volume)))

rhpo_1565 <- ggplot(rh.vol.15_65, aes(Group, rh_parsorbitalis_volume)) + geom_violin(fill = "gray") +
  xlab("Group") + ylab("Gray Matter Volume") +
  theme(title = element_text(size = 20), text = element_text(size = 18)) + 
  geom_point(aes(y = AVERAGE), size = 2, position = position_dodge((width = 0.90)), data = grh.po) +
  ggtitle("RH Pars Orbitalis (15/65)") + 
  theme_bw() +
  geom_signif(comparisons = list(c("UPC", "EAC")), annotations="*", tip_length = 0) +
  geom_signif(comparisons = list(c("UPC", "UGC")), annotations=".", tip_length = 0.025, y_position = 4750) + 
  geom_errorbar(aes(y = AVERAGE, ymin = AVERAGE-SE, ymax = AVERAGE+SE), 
                width = 0.20, position = position_dodge((width = 0.90)), data = grh.po) +
  ylim(NA, 5000)
rhpo_1565

grh.po<-ddply(rh.vol.15_70, c("Group"), summarize,
              AVERAGE=mean(rh_parsorbitalis_volume, na.rm = TRUE),
              SE=sqrt(var(rh_parsorbitalis_volume, na.rm = TRUE)/length(rh_parsorbitalis_volume)))

rhpo_1570 <- ggplot(rh.vol.15_70, aes(Group, rh_parsorbitalis_volume)) + geom_violin(fill = "gray") +
  xlab("Group") + ylab("Gray Matter Volume") +
  theme(title = element_text(size = 20), text = element_text(size = 18)) +
  geom_point(aes(y = AVERAGE), size = 2, position = position_dodge((width = 0.90)), data = grh.po) +
  ggtitle("RH Pars Orbitalis (15/70)") + 
  theme_bw() +
  geom_signif(comparisons = list(c("UPC", "UGC")), annotations="*", tip_length = 0.025) +
  geom_signif(comparisons = list(c("EAC", "UPC")), annotations=".", tip_length = 0, y_position = 4750) + 
  geom_errorbar(aes(y = AVERAGE, ymin = AVERAGE-SE, ymax = AVERAGE+SE), 
                width = 0.20, position = position_dodge((width = 0.90)), data = grh.po) +
  ylim(NA, 5000)
rhpo_1570

grid.arrange(lhpo_1565, rhpo_1565, lhpo_1570, rhpo_1570, ncol=2)


grh.pre<-ddply(lh.vol.15_65, c("Group"), summarize,
              AVERAGE=mean(lh_precuneus_volume, na.rm = TRUE),
              SE=sqrt(var(lh_precuneus_volume, na.rm = TRUE)/length(lh_precuneus_volume)))

lhpre_1565 <- ggplot(lh.vol.15_65, aes(Group, lh_precuneus_volume)) + geom_violin(fill = "gray") +
  xlab("Group") + ylab("Gray Matter Volume") +
  theme(title = element_text(size = 20), text = element_text(size = 18)) + 
  geom_point(aes(y = AVERAGE), size = 2, position = position_dodge((width = 0.90)), data = grh.pre) +
  ggtitle("LH Precuneus (15/65)") + 
  theme_bw() +
  geom_signif(comparisons = list(c("UPC", "UGC")), annotations="*", tip_length = 0.025) +
  geom_errorbar(aes(y = AVERAGE, ymin = AVERAGE-SE, ymax = AVERAGE+SE), 
                width = 0.20, position = position_dodge((width = 0.90)), data = grh.pre) +
  ylim(NA, 17500)
lhpre_1565

grh.pre<-ddply(lh.vol.15_70, c("Group"), summarize,
              AVERAGE=mean(lh_precuneus_volume, na.rm = TRUE),
              SE=sqrt(var(lh_precuneus_volume, na.rm = TRUE)/length(lh_precuneus_volume)))

lhpre_1570 <- ggplot(lh.vol.15_70, aes(Group, lh_precuneus_volume)) + geom_violin(fill = "gray") +
  xlab("Group") + ylab("Gray Matter Volume") +
  theme(title = element_text(size = 20), text = element_text(size = 18)) + 
  geom_point(aes(y = AVERAGE), size = 2, position = position_dodge((width = 0.90)), data = grh.pre) +
  ggtitle("LH Precuneus (15/70)") + 
  theme_bw() +
  geom_signif(comparisons = list(c("UPC", "UGC")), annotations="*", tip_length = 0.025) +
  geom_errorbar(aes(y = AVERAGE, ymin = AVERAGE-SE, ymax = AVERAGE+SE), 
                width = 0.20, position = position_dodge((width = 0.90)), data = grh.pre) +
  ylim(NA, 17500)
lhpre_1570


grh.pre<-ddply(rh.vol.15_65, c("Group"), summarize,
              AVERAGE=mean(rh_precuneus_volume, na.rm = TRUE),
              SE=sqrt(var(rh_precuneus_volume, na.rm = TRUE)/length(rh_precuneus_volume)))

rhpre_1565 <- ggplot(rh.vol.15_65, aes(Group, rh_precuneus_volume)) + geom_violin(fill = "gray") +
  xlab("Group") + ylab("Gray Matter Volume") +
  theme(title = element_text(size = 20), text = element_text(size = 18)) + 
  geom_point(aes(y = AVERAGE), size = 2, position = position_dodge((width = 0.90)), data = grh.pre) +
  ggtitle("RH Precuneus (15/65)") + 
  theme_bw() +
  geom_signif(comparisons = list(c("UPC", "EAC")), annotations=".", tip_length = 0, y_position = 18000) +
  geom_signif(comparisons = list(c("UPC", "UGC")), annotations="*", tip_length = 0.025, y_position = 19000) + 
  geom_errorbar(aes(y = AVERAGE, ymin = AVERAGE-SE, ymax = AVERAGE+SE), 
                width = 0.20, position = position_dodge((width = 0.90)), data = grh.pre) +
  ylim(NA, 20000)
rhpre_1565

grh.pre<-ddply(rh.vol.15_70, c("Group"), summarize,
              AVERAGE=mean(rh_precuneus_volume, na.rm = TRUE),
              SE=sqrt(var(rh_precuneus_volume, na.rm = TRUE)/length(rh_precuneus_volume)))

rhpre_1570 <- ggplot(rh.vol.15_70, aes(Group, rh_precuneus_volume)) + geom_violin(fill = "gray") +
  xlab("Group") + ylab("Gray Matter Volume") +
  theme(title = element_text(size = 20), text = element_text(size = 18)) +
  geom_point(aes(y = AVERAGE), size = 2, position = position_dodge((width = 0.90)), data = grh.pre) +
  ggtitle("RH Precuneus (15/70)") + 
  theme_bw() +
  geom_signif(comparisons = list(c("UPC", "UGC")), annotations="**", tip_length = 0.025) +
  geom_signif(comparisons = list(c("EAC", "UGC")), annotations="*", tip_length = 0, y_position = 19500) + 
  geom_errorbar(aes(y = AVERAGE, ymin = AVERAGE-SE, ymax = AVERAGE+SE), 
                width = 0.20, position = position_dodge((width = 0.90)), data = grh.pre) +
  ylim(NA, 20000)
rhpre_1570

grid.arrange(lhpre_1565, rhpre_1565, lhpre_1570, rhpre_1570, ncol = 2)
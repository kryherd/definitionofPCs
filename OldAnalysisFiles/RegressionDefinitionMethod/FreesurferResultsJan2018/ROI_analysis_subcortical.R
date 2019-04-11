#################### ORGANZING DATA
setwd("~/Dropbox/Definition_of_PCs/RegressionDefinitionMethod/FreesurferResultsJan2018")
# read in packages
library(xlsx)

beh <- read.csv("FullData_FinalMay23_2017.csv")
age <- data.frame(beh$SubjectID, beh$StructuralMRI.ID, beh$age.mri)
names(age) <- c("SubID", "MRI.ID", "age.mri")
# read all of the sheets in to a list of dataframes

VolumeList <- list()
for (i in 1:4) {
  new <- read.xlsx("Subcortical_SegmentationStats_Volume_WJ.xlsx", i) 
  VolumeList[[i]] <- new
}
VL <- VolumeList[unlist(lapply(VolumeList,length)!=0)] 

# give each dataframe in the list its name
names <- read.csv("SubcorticalStats_names.csv", header = FALSE)
nams <- names[, 1]
names(VL) <-nams

# Pulling out covariates
for (i in 1:4) {
  VL[[i]]$Group <- substr(VL[[i]]$Class, 1, 3)
}

mergeListVol <- list()
for (i in 1:length(VL)) {
  some <- VL[[i]][substr(VL[[i]]$Subject,1,1) == "h",]
  some$Subject2 <- substr(some$Subject, 2, 5)
  merged1 <- merge(some, age, by.x = "Subject2", by.y = "SubID", all.x = TRUE)
  merged1 <- merged1[, -c(1, 69)]
  others <- VL[[i]][substr(VL[[i]]$Subject,1,1) == "t",]
  merged2 <- merge(others, age, by.x = "Subject", by.y = "MRI.ID", all.x = TRUE)
  merged2 <- merged2[,-68]
  merge_total <- rbind(merged1, merged2)
  mergeListVol[[i]] <- merge_total
}

names(mergeListVol) <- nams

################################## ANALYZING DATA

#run models for volume

for (i in 1:length(mergeListVol)) {
  sink(paste0("subcort-volumeresults-",names(mergeListVol[i]), ".txt"))
  mergeListVol[[i]]$ICV.demean <- mergeListVol[[i]]$EstimatedTotalIntraCranialVol - mean(mergeListVol[[i]]$EstimatedTotalIntraCranialVol, na.rm = TRUE)
  volumeVars = names(mergeListVol[[i]][c(6, 24, 14, 29)])
  models = lapply( volumeVars, function(x) { lm(substitute( i ~  ICV.demean + age.mri + Group, list(i=as.name(x)) ), data=mergeListVol[[i]] ) } )
  lapply(models, function(y) { print(anova(y))})
  sink()
}

# follow-up t-tests

KTEANoInt15_65_imputed <- data.frame(mergeListVol["KTEANoInt15_65-imputed"])
names(KTEANoInt15_65_imputed) <- names(mergeListVol[[1]])
pairwise.t.test(KTEANoInt15_65_notimputed$Right.Hippocampus, KTEANoInt15_65_notimputed$Group, p.adjust.method = "none")
pairwise.t.test(KTEANoInt15_65_imputed$Right.Cerebellum.Cortex, KTEANoInt15_65_imputed$Group, p.adjust.method = "none")
pairwise.t.test(KTEANoInt15_65_imputed$Left.Cerebellum.Cortex, KTEANoInt15_65_imputed$Group, p.adjust.method = "none")


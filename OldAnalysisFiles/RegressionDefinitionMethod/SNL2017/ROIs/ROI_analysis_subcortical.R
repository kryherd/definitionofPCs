#################### ORGANZING DATA

# read in packages
library(xlsx)

beh <- read.csv("FullData_FinalMay23_2017.csv")
age <- data.frame(beh$SubjectID, beh$StructuralMRI.ID, beh$age.mri)
names(age) <- c("SubID", "MRI.ID", "age.mri")
# read all of the sheets in to a list of dataframes

VolumeList <- list()
for (i in 1:5) {
  new <- read.xlsx("Subcortical_SegmentationStats_Volume.xlsx", i) 
  VolumeList[[i]] <- new
}
VL <- VolumeList[unlist(lapply(VolumeList,length)!=0)] 

# give each dataframe in the list its name
names <- read.csv("SubcorticalStats_names.csv", header = FALSE)
nams <- names[, 1]
names(VL) <-nams

# Pulling out covariates
for (i in 1:5) {
  VL[[i]]$Group <- substr(VL[[i]]$Class, 1, 3)
  VL[[i]]$Sex <- substr(VL[[i]]$Class, 4, 4)
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

ICV <- data.frame()
for (i in 1:length(mergeListVol)) {
each <- data.frame(mergeListVol[[i]]$Subject, mergeListVol[[i]]$EstimatedTotalIntraCranialVol)
ICV <- rbind(ICV, each)
}

ICV.all <- unique(ICV)
names(ICV.all) <- c("Subject", "ICV")
write.csv(ICV.all, "ICV.csv")

names(mergeListVol) <- nams

################################## ANALYZING DATA

#run models for volume

for (i in 1:length(mergeListVol)) {
  sink(paste0("subcort-volumeresults-",names(mergeListVol[i]), ".txt"))
  mergeListVol[[i]]$ICV.demean <- mergeListVol[[i]]$EstimatedTotalIntraCranialVol - mean(mergeListVol[[i]]$EstimatedTotalIntraCranialVol, na.rm = TRUE)
  volumeVars = names(mergeListVol[[i]][c(6, 24, 14, 29)])
  models = lapply( volumeVars, function(x) { lm(substitute( i ~  ICV.demean + Sex + age.mri + Group, list(i=as.name(x)) ), data=mergeListVol[[i]] ) } )
  lapply(models, function(y) { print(anova(y))})
  sink()
}

# follow-up t-tests

KTEANoInt15_65_imputed <- data.frame(mergeListVol["KTEANoInt15_65-imputed"])
names(KTEANoInt15_65_imputed) <- names(mergeListVol[[1]])
pairwise.t.test(KTEANoInt15_65_notimputed$Right.Hippocampus, KTEANoInt15_65_notimputed$Group, p.adjust.method = "none")
pairwise.t.test(KTEANoInt15_65_imputed$Right.Cerebellum.Cortex, KTEANoInt15_65_imputed$Group, p.adjust.method = "none")
pairwise.t.test(KTEANoInt15_65_imputed$Left.Cerebellum.Cortex, KTEANoInt15_65_imputed$Group, p.adjust.method = "none")


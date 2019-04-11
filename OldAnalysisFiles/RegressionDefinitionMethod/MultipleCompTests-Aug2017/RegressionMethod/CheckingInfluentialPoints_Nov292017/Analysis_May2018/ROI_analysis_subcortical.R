#################### ORGANZING DATA
setwd("~/Dropbox/Definition_of_PCs/RegressionDefinitionMethod/MultipleCompTests-Aug2017/RegressionMethod/CheckingInfluentialPoints_Nov292017/Analysis_May2018")
# read in packages
library(gdata)

beh <- read.csv("FullData_Dec5_2017.csv")
age <- data.frame(beh$SubjectID, beh$StructuralMRI.ID, beh$age.mri)
names(age) <- c("SubID", "MRI.ID", "age.mri")
# read all of the sheets in to a list of dataframes
subcort <- read.csv("Subcortical_stats.csv")
merge_subcort <- merge(subcort, age, by.x = "Measure.volume", by.y = "MRI.ID", all.x = FALSE, all.y = TRUE)

mergea182 <- subcort[substr(subcort$Measure.volume,1,1) == "h",]
mergea182$FreesurferID <- mergea182$Measure.volume
mergea182$SubID <- substr(mergea182$Measure.volume,2,5)
mergea182_2 <- merge(mergea182, age, by.x = "SubID", by.y = "SubID")
mergea182_2 <- mergea182_2[,-c(2,40)]

mergeothers <- subcort[substr(subcort$Measure.volume,1,1) != "h",]
mergeothers_2 <- merge(mergeothers, age, by.x = "Measure.volume", by.y = "MRI.ID")
colnames(mergeothers_2)[47] <- "SubID"
colnames(mergeothers_2)[1] <- "FreesurferID"
merge <- rbind(mergea182_2, mergeothers_2)

test <- merge(age, merge, by = "SubID", all = TRUE)

write.csv(test$SubID[is.na(test$Left.Inf.Lat.Vent)], "missingdata.csv")


#### CHANGE MODEL NAME HERE
model <- "WJ3NoImputeInt-15_65"



mod <- read.csv(paste0(model,".csv"))
WJ3_MRI <- merge(mod, age, by.x = "SubjectID", by.y = "SubID")
full_data <- merge(merge_subcort, WJ3_MRI, all.x = FALSE, all.y = TRUE, by.x = "Measure.volume", by.y = "MRI.ID")
full_data <- full_data[full_data$CI_group != "EPC",]
full_data <- drop.levels(full_data)

################################## ANALYZING DATA


### MAKE SURE TO CHANGE TO LME's
#run models
sink(paste0("subcort-volumeresults-",model, ".txt"))
full_data$ICV.demean <- full_data$EstimatedTotalIntraCranialVol - mean(full_data$EstimatedTotalIntraCranialVol, na.rm = TRUE)
volumeVars = names(full_data[-c(1,15,35:78)])
models = lapply( volumeVars, function(x) { lm(substitute( i ~  ICV.demean + age.mri + Group, list(i=as.name(x)) ), data=full_data ) } )
lapply(models, function(y) { print(anova(y))})
sink()




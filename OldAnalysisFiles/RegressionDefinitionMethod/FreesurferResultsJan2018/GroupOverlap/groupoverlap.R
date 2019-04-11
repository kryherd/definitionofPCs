setwd("~/Dropbox/Definition_of_PCs/RegressionDefinitionMethod/FreesurferResultsJan2018/GroupOverlap")

Int15_65 <- read.csv("MRIGroups_WJ3Int15_65.csv")
Int15_70 <- read.csv("MRIGroups_WJ3Int15_70.csv")
NoInt15_65 <- read.csv("MRIGroups_WJ3NoInt15_65.csv")
NoInt15_70 <- read.csv("MRIGroups_WJ3NoInt15_70.csv")

merge <- merge(Int15_65, Int15_70, by = "SubjectID", all =TRUE)
merge2 <- merge(NoInt15_65, NoInt15_70, by = "SubjectID", all =TRUE)
all_merge <- merge(merge, merge2, by = "SubjectID", all = TRUE)

clean <- all_merge[,c(1,3,7,11,15,17)]
names(clean) <- c("SubjectID","Int15_65", "Int15_70", "NoInt15_65", "NoInt15_70", "MRI_ID")

write.csv(clean, "OverlapClassifierJan2018.csv")
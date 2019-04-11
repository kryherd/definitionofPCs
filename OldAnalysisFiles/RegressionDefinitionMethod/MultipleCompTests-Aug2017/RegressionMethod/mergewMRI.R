MRI <- read.csv("FullData_FinalMay23_2017.csv")

res <- WJ3_2[,c(1, 25)]

merged <- merge (res, MRI, by = "SubjectID", all=TRUE)

write.csv(merged, "MergedData.csv")

merge2 <- read.csv("MergedData.csv")

merge2 <- merge2[complete.cases(merge2$StructuralMRI.ID),]

table(merge2$CI_group)

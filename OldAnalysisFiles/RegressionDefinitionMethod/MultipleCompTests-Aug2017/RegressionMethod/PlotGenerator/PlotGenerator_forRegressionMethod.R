## This script creates a boxplot showing how the 3 groups (EAC, UGC, UPC) differ on a given individual differences measure for a certain model

## Edit the variables below and then press Control + Shift + Enter to generate the plot

# Change this variable to see the model you wish to look at.
# The model names are the same as the ones in the spreadsheet.
ModelName <- "KTEANoInt-15_65"

# Change this variable to see the measure you wish to look at.
# Options: Age, PPVT, MatrixReasoning, KTEA, WJ3, Decoding
DiffMeasure <- "Age"




### everything below this line makes the plot. don't edit
setwd("~/Dropbox/Definition_of_PCs/RegressionDefinitionMethod/MultipleCompTests-Aug2017/RegressionMethod")
MRI <- read.csv("FullData_FinalMay23_2017.csv")
dfs <- list("WJInt-20_70.csv", "WJNoInt-20_70.csv", "KTEAInt-20_70.csv", "KTEANoInt-20_70.csv",
            "WJInt-15_70.csv", "WJNoInt-15_70.csv", "KTEAInt-15_70.csv", "KTEANoInt-15_70.csv",
            "WJInt-15_60.csv", "WJNoInt-15_60.csv", "KTEAInt-15_60.csv", "KTEANoInt-15_60.csv",
            "WJInt-15_65.csv", "WJNoInt-15_65.csv", "KTEAInt-15_65.csv", "KTEANoInt-15_65.csv")
data.groups <- lapply(dfs, read.csv)

# remove EPCs
newList <- list()
for (i in 1:length(data.groups)) {
  new <- subset(data.groups[[i]], CI_group != "EPC")
  new$CI_group <- factor(new$CI_group)
  newList[[i]] <- new
}

# merge MRI IDs
mergeList <- list()
for (i in 1:length(newList)) {
  merged <- merge(newList[[i]], MRI, by.x = "SubjectID", by.y = "SubjectID", all = TRUE)
  merged2 <- merged[,c(1, 4, 14, 16, 18, 20, 22, 26, 45)]
  merged3 <- merged2[complete.cases(merged2$StructuralMRI.ID),]
  merged4 <- merged3[complete.cases(merged3$CI_group),]
  mergeList[[i]] <- merged4
}


Model <- gsub('.csv', '', dfs)
names(mergeList) <- Model

plot.data <- data.frame(mergeList[ModelName])
names(plot.data) <- c("SubjectID", "Age", "PPVT", "MatrixReasoning", "KTEA", "WJ3", "Decoding", "CI_group", "MRI.ID")

p <- ggplot(data = plot.data, aes_string(x = "CI_group", y = DiffMeasure)) + geom_boxplot() + theme_bw() + xlab("Group") + ylab(DiffMeasure) + ggtitle(ModelName)
p




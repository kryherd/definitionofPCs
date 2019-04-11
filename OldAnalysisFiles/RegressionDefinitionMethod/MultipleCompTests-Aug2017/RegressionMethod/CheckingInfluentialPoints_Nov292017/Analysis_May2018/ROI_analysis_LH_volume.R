### pick your model
# expecting a df with *at least* SubjectIDs and CI_group

model <- "WJ3NoImputeInt-20_70"

######## ORGANIZING DATA
setwd("~/Dropbox/Definition_of_PCs/RegressionDefinitionMethod/MultipleCompTests-Aug2017/RegressionMethod/CheckingInfluentialPoints_Nov292017/Analysis_May2018")
# read in packages
library(gdata)
library(car)
library(fBasics)

beh <- read.csv("FullData_Dec5_2017.csv")
age <- data.frame(beh$SubjectID, beh$StructuralMRI.ID, beh$age.mri)
names(age) <- c("SubID", "MRI.ID", "age.mri")

### Data cleaning
## Merge age with structural stats
lh_vol <- read.csv("LH_Volume.csv")
merge_lhvol <- merge(lh_vol, age, by.x = "lh.aparc.volume", by.y = "MRI.ID", all.x = FALSE, all.y = TRUE)

mergea182 <- lh_vol[substr(lh_vol$lh.aparc.volume,1,1) == "h",]
mergea182$FreesurferID <- mergea182$lh.aparc.volume
mergea182$SubID <- substr(mergea182$lh.aparc.volume,2,5)
mergea182_2 <- merge(mergea182, age, by.x = "SubID", by.y = "SubID")
mergea182_2 <- mergea182_2[,-c(2,40)]

mergeothers <- lh_vol[substr(lh_vol$lh.aparc.volume,1,1) != "h",]
mergeothers_2 <- merge(mergeothers, age, by.x = "lh.aparc.volume", by.y = "MRI.ID")
colnames(mergeothers_2)[38] <- "SubID"
colnames(mergeothers_2)[1] <- "FreesurferID"
merge <- rbind(mergea182_2, mergeothers_2)
MRI_Stats <- merge(age, merge, by = "SubID", all = TRUE)
MRI_Stats <- MRI_Stats[,-41]
colnames(MRI_Stats)[3] <- "age.mri"

## Merge with Groups

groups <- read.csv(paste0(model, ".csv"))
group_merge <- merge(MRI_Stats, groups, by.x = "SubID", by.y = "SubjectID")
table(group_merge$CI_group) ## this should match the group sizes you are expecting. Very important to check!!

## Merge with ICV
ICV <- read.csv("ICV.csv")
all_merge <- merge(group_merge, ICV, by.x = "FreesurferID", by.y = "Measure.volume")
table(all_merge$CI_group) ## re-check group sizes here
all_merge$ICV.demean <- all_merge$EstimatedTotalIntraCranialVol - mean(all_merge$EstimatedTotalIntraCranialVol, na.rm = TRUE)


##################### ANALYZING DATA

all_merge <- all_merge[all_merge$CI_group != "EPC",]
all_merge$CI_group <- droplevels(all_merge$CI_group)

#run models 

sink(paste0("lh_volumeresults-",model, ".txt"))
volumeVars <- names(all_merge[c(5:38)])
models <- lapply( volumeVars, function(x) { lm(substitute( j ~  ICV.demean + age.mri + CI_group, list(j=as.name(x)) ), data=all_merge)})
lapply(models, function(y) { 
    q <- names(y$model[1])
    cat("ROI: ", (as.name(q)), "\n \n")
    print(Anova(y))
    cat("-------------------------\n")
    l <- eval(substitute(leveneTest(all_merge$k, all_merge$CI_group), list(k=as.name(q))))
    if (l$`Pr(>F)`[1] <= .05) {
      cat("Levene's Test for Homogeneity of Variance")
      cat("Significant -- unequal variances **")
      cat("p-value:")
      print(l$`Pr(>F)`[1])} else {cat("Levene's Test for Homogeneity of Variance\n\n")
        cat("Not Significant -- Equal variances\n")
        cat("p-value: ")
        cat(l$`Pr(>F)`[1], "\n\n")
      }
    cat("-------------------------\n")
    sw <- eval(substitute(by(all_merge$m, all_merge$CI_group, shapiroTest), list(m=as.name(q))))
    EAC.p <- sw[1]$EAC@test$p.value
    UGC.p <- sw[2]$UGC@test$p.value
    UPC.p <- sw[3]$UPC@test$p.value
    if (EAC.p <= .05) {
      EAC.mark <- "**"
    } else {EAC.mark <- ""}
    if (UGC.p <= .05) {
      UGC.mark <- "**"
    } else {UGC.mark <- ""}
    if (UPC.p <= .05) {
      UPC.mark <- "**"
    } else {UPC.mark <- ""}
    cat("Shapiro-Wilk test for Normality (for each group)\n\n")
    cat("","Group", "P-val\n", "EAC", EAC.p, EAC.mark, "\n", "UGC", UGC.p, UGC.mark, "\n", "UPC", UPC.p, UPC.mark)
    cat("\n\n-------------------------\n")
    cat("Cook's Distance\nSubjects whose data for this measure may be an outlier\n(specifically, whose Cook's d is 4x the mean cook's d)\n\n")
    cds <- cooks.distance(y)
    fourmean.cd <- 4*mean(cds)
    print(cds[cds>fourmean.cd])
    cat("\n==========================\n\n")
    })
sink()


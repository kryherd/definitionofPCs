######## ORGANIZING DATA
setwd("~/definitionofPCs")
# read in packages
library(xlsx)
library(phia)
library(multcomp)
library(DTK)
library(tidyverse)

# read in structural data
struc <- read.csv("./sMRIData/structural_data.csv")
# read in behavioral data (including groups)
temp <- list.files(path = "./group_output", pattern="*.csv", full.names = TRUE)
names <- list.files(path = "./group_output", pattern="*.csv")
for (i in 1:length(names)){
  if (substr(names[i], nchar(names[i]) - 5, nchar(names[1])) == "ed.csv"){
    names[i] <- paste(substr(names[i], 1, nchar(names[i])-4), "_not_imputed")
  } else{
    names[i] <- substr(names[i], 1, nchar(names[i])-4)
  }
}
beh <- lapply(temp, read.csv)

# demean ICV
struc$ICV_demeaned <- struc$EstimatedTotalIntraCranialVol - mean(struc$EstimatedTotalIntraCranialVol, na.rm = TRUE)

# demean thickness
struc$lh_meanthick_demeaned <- struc$lh_MeanThickness_thickness - mean(struc$lh_MeanThickness_thickness, na.rm = TRUE)
struc$rh_meanthick_demeaned <- struc$rh_MeanThickness_thickness - mean(struc$rh_MeanThickness_thickness, na.rm = TRUE)

# merge data
allList <- list()
for (i in 1:length(beh)){
  df <- beh[[i]]
  merged <- merge(struc, df, all.x = FALSE, all.y= TRUE, by = "SubjectID")
  allList[[i]] <- merged
}
names(allList) <- names

# create separate dfs for volume and thickness

volumeList <- list()
for (i in 1:length(allList)){
  df <- allList[[i]]
  df <- dplyr::select(df, SubjectID, mri_id, ends_with("volume"), ICV_demeaned, pred:age.c)
  volumeList[[i]] <- df
}
names(volumeList) <- names

thicknessList <- list()
for (i in 1:length(allList)){
  df <- allList[[i]]
  df <- dplyr::select(df, SubjectID, mri_id, ends_with("thickness"), contains("meanthick"), pred:age.c)
  thicknessList[[i]] <- df
}
names(thicknessList) <- names

##################### ANALYZING DATA

######### PRINT OUT ALL RESULTS

#run models for volume

for (i in 1:length(volumeList)) {
  df <- volumeList[[i]]
  name <- names[i]
  sink(paste0("./struc_output/all_results/volumeresults-",name, ".txt"))
  volumeVars <- names(select(df, ends_with("_volume")))
  models <- lapply(volumeVars, function(x) { lm(substitute(j ~  ICV_demeaned + age.mri + groups, list(j=as.name(x))), data=df)})
  lapply(models, function(y) {
    q <- names(y$model[1])
    cat("ROI: ", (as.name(q)), "\n \n")
    print(anova(y))
    cat("-------------------------\n")
    cat("Tukey Test for pairwise comparisons")
    postHocs <- glht(y, linfct = mcp(groups = "Tukey"))
    print(summary(postHocs))
    cat("-------------------------\n")
    cat("Pairwise comparisons (no correction)")
    postHocs2 <- glht(y, linfct = mcp(groups = "Tukey"))
    print(summary(postHocs2, test = adjusted("none")))
    cat("-------------------------\n")
    cat("Pairwise Mann–Whitney U-tests")
    print(eval(substitute(pairwise.wilcox.test(y$model$i,
                         y$model$groups,
                         p.adjust.method="none"), list(i=as.name(q)))))
    cat("\n==========================\n\n")
    })
  sink()
}

# run LH models for thickness
for (i in 1:length(thicknessList)) {
  df <- thicknessList[[i]]
  name <- names[i]
  sink(paste0("./struc_output/all_results/lh_thickresults-",name, ".txt"))
  thicknessVars <- names(select(df, starts_with("lh")))[-c(35:36)]
  models <- lapply(thicknessVars, function(x) { lm(substitute(j ~ age.mri + groups, list(j=as.name(x))), data=df)})
  lapply(models, function(y) { 
    q <- names(y$model[1])
    cat("ROI: ", (as.name(q)), "\n \n")
    print(anova(y))
    cat("-------------------------\n")
    cat("Tukey Test for pairwise comparisons")
    postHocs <- glht(y, linfct = mcp(groups = "Tukey"))
    print(summary(postHocs))
    cat("-------------------------\n")
    cat("Pairwise comparisons (no correction)")
    postHocs2 <- glht(y, linfct = mcp(groups = "Tukey"))
    print(summary(postHocs2, test = adjusted("none")))
    cat("-------------------------\n")
    cat("Pairwise Mann–Whitney U-tests")
    print(eval(substitute(pairwise.wilcox.test(y$model$i,
                                               y$model$groups,
                                               p.adjust.method="none"), list(i=as.name(q)))))
    cat("\n==========================\n\n")
  })
  sink()
}

# run RH models for thickness
for (i in 1:length(thicknessList)) {
  df <- thicknessList[[i]]
  name <- names[i]
  sink(paste0("./struc_output/all_results/rh_thickresults-",name, ".txt"))
  thicknessVars <- names(select(df, starts_with("rh")))[-c(35:36)]
  models <- lapply(thicknessVars, function(x) { lm(substitute(j ~ age.mri + groups, list(j=as.name(x))), data=df)})
  lapply(models, function(y) { 
    q <- names(y$model[1])
    cat("ROI: ", (as.name(q)), "\n \n")
    print(anova(y))
    cat("-------------------------\n")
    cat("Tukey Test for pairwise comparisons")
    postHocs <- glht(y, linfct = mcp(groups = "Tukey"))
    print(summary(postHocs))
    cat("-------------------------\n")
    cat("Pairwise comparisons (no correction)")
    postHocs2 <- glht(y, linfct = mcp(groups = "Tukey"))
    print(summary(postHocs2, test = adjusted("none")))
    cat("-------------------------\n")
    cat("Pairwise Mann–Whitney U-tests")
    print(eval(substitute(pairwise.wilcox.test(y$model$i,
                                               y$model$groups,
                                               p.adjust.method="none"), list(i=as.name(q)))))
    cat("\n==========================\n\n")
  })
  sink()
}

######### PRINT OUT ONLY SIGNIFICANT RESULTS

for (i in 1:length(volumeList)) {
  df <- volumeList[[i]]
  name <- names[i]
  sink(paste0("./struc_output/sig_results/volumeresults-",name, ".txt"))
  volumeVars <- names(select(df, ends_with("_volume")))
  models <- lapply(volumeVars, function(x) { lm(substitute(j ~  ICV_demeaned + age.mri + groups, list(j=as.name(x))), data=df)})
  lapply(models, function(y) { 
    q <- names(y$model[1])
    group.pval <- anova(y)$`Pr(>F)`[3]
    if (group.pval <= 0.10){
      cat("ROI: ", (as.name(q)), "\n \n")
      print(anova(y))
      cat("-------------------------\n")
      cat("Tukey Test for pairwise comparisons")
      postHocs <- glht(y, linfct = mcp(groups = "Tukey"))
      print(summary(postHocs))
      cat("-------------------------\n")
      cat("Pairwise comparisons (no correction)")
      postHocs2 <- glht(y, linfct = mcp(groups = "Tukey"))
      print(summary(postHocs2, test = adjusted("none")))
      cat("-------------------------\n")
      cat("Pairwise Mann–Whitney U-tests")
      print(eval(substitute(pairwise.wilcox.test(y$model$i,
                                                 y$model$groups,
                                                 p.adjust.method="none"), list(i=as.name(q)))))
      cat("\n==========================\n\n")
    }
  })
  sink()
}

for (i in 1:length(thicknessList)) {
  df <- thicknessList[[i]]
  name <- names[i]
  sink(paste0("./struc_output/sig_results/lh_thickresults-",name, ".txt"))
  thicknessVars <- names(select(df, starts_with("lh")))[-c(35:36)]
  models <- lapply(thicknessVars, function(x) { lm(substitute(j ~ age.mri + groups, list(j=as.name(x))), data=df)})
  lapply(models, function(y) { 
    q <- names(y$model[1])
    group.pval <- anova(y)$`Pr(>F)`[2]
    if (group.pval <= 0.10){
      cat("ROI: ", (as.name(q)), "\n \n")
      print(anova(y))
      cat("-------------------------\n")
      cat("Tukey Test for pairwise comparisons")
      postHocs <- glht(y, linfct = mcp(groups = "Tukey"))
      print(summary(postHocs))
      cat("-------------------------\n")
      cat("Pairwise comparisons (no correction)")
      postHocs2 <- glht(y, linfct = mcp(groups = "Tukey"))
      print(summary(postHocs2, test = adjusted("none")))
      cat("-------------------------\n")
      cat("Pairwise Mann–Whitney U-tests")
      print(eval(substitute(pairwise.wilcox.test(y$model$i,
                                                 y$model$groups,
                                                 p.adjust.method="none"), list(i=as.name(q)))))
      cat("\n==========================\n\n")
    }
  })
  sink()
}

for (i in 1:length(thicknessList)) {
  df <- thicknessList[[i]]
  name <- names[i]
  sink(paste0("./struc_output/sig_results/rh_thickresults-",name, ".txt"))
  thicknessVars <- names(select(df, starts_with("rh")))[-c(35:36)]
  models <- lapply(thicknessVars, function(x) { lm(substitute(j ~ age.mri + groups, list(j=as.name(x))), data=df)})
  lapply(models, function(y) { 
    q <- names(y$model[1])
    group.pval <- anova(y)$`Pr(>F)`[2]
    if (group.pval <= 0.10){
      cat("ROI: ", (as.name(q)), "\n \n")
      print(anova(y))
      cat("-------------------------\n")
      cat("Tukey Test for pairwise comparisons")
      postHocs <- glht(y, linfct = mcp(groups = "Tukey"))
      print(summary(postHocs))
      cat("-------------------------\n")
      cat("Pairwise comparisons (no correction)")
      postHocs2 <- glht(y, linfct = mcp(groups = "Tukey"))
      print(summary(postHocs2, test = adjusted("none")))
      cat("-------------------------\n")
      cat("Pairwise Mann–Whitney U-tests")
      print(eval(substitute(pairwise.wilcox.test(y$model$i,
                                                 y$model$groups,
                                                 p.adjust.method="none"), list(i=as.name(q)))))
      cat("\n==========================\n\n")
    }
  })
  sink()
}


write.csv(names, "model_names.csv", row.names = FALSE)

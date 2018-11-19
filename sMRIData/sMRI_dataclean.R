# read in data
library(openxlsx)
library(dplyr)
setwd("~/definitionofPCs/sMRIData")
# one sheet for each hemisphere + measure -- some subjects
lh_vol_1 <- read.xlsx("missingdata_stats.xlsx", 2)
lh_thic_1 <- read.xlsx("missingdata_stats.xlsx", 3)
rh_vol_1 <- read.xlsx("missingdata_stats.xlsx", 4)
rh_thic_1 <- read.xlsx("missingdata_stats.xlsx", 5)
vol_extra_1 <- read.xlsx("missingdata_stats.xlsx", 6)

# one sheet for each hemisphere + measure -- the rest of the subjects
lh_vol_2 <- read.xlsx("FullData_Dec5_2017_structuralStats.xlsx", 3)
lh_thic_2 <- read.xlsx("FullData_Dec5_2017_structuralStats.xlsx", 4)
rh_vol_2 <- read.xlsx("FullData_Dec5_2017_structuralStats.xlsx", 5)
rh_thic_2 <- read.xlsx("FullData_Dec5_2017_structuralStats.xlsx", 6)
vol_extra_2 <- read.xlsx("FullData_Dec5_2017_structuralStats.xlsx", 2)

# stack the data together, get rid of extra columns
lh_vol <- rbind(lh_vol_1, lh_vol_2)
colnames(lh_vol)[1] <- "mri_id"
lh_vol <- lh_vol[,-c(36,37)]
rh_vol <- rbind(rh_vol_1, rh_vol_2)
colnames(rh_vol)[1] <- "mri_id"
rh_vol <- rh_vol[,-c(36,37)]
lh_thic <- rbind(lh_thic_1, lh_thic_2)
colnames(lh_thic)[1] <- "mri_id"
lh_thic <- lh_thic[,-c(37.38)]
rh_thic <- rbind(rh_thic_1, rh_thic_2)
colnames(rh_thic)[1] <- "mri_id"
rh_thic <- rh_thic[,-c(37,38)]

## use this code if you get the full ICV for the "FullData_Dec5_2017_structuralStats.xlsx" file
vol_extra_1 <- vol_extra_1[,c(1,64)]
vol_extra_2 <- vol_extra_2[,c(1,47)]
vol_extra <- rbind(vol_extra_1, vol_extra_2)
colnames(vol_extra)[1] <- "mri_id"

# grab behavioral data
data <- read.csv("../FullData_Sept27_2018.csv")
# grab linking file
sub_mri_IDs <- read.csv("sub_mri_IDs.csv")

# merge behavioral IDs to linking file to make sure they're the same
data_mri <- merge(data, sub_mri_IDs, by = "SubjectID", all = TRUE)

# select only the IDs who have MRI data
data_mri2 <- data_mri %>%
  filter(!is.na(mri_id))

# merge everything together
data_ids <- select(data_mri2, SubjectID, mri_id)
lh_ids <- merge(data_ids, lh_vol, by = "mri_id")
lh_rh_ids <- merge(lh_ids, rh_vol, by = "mri_id")
lh_rh_lh_ids <-merge(lh_rh_ids, lh_thic, by = "mri_id")
vol_thic_ids <-merge(lh_rh_lh_ids, rh_thic, by = "mri_id")
all_vars <- merge(vol_thic_ids, vol_extra, by= "mri_id", all.x = TRUE)

# write to csv
write.csv(all_vars, "structural_data.csv")



setwd("~/Dropbox/Definition_of_PCs/RegressionDefinitionMethod/ClassifierwithImputedData_July2018")
FullData_Dec5_2017 <- read.csv("~/Dropbox/Definition_of_PCs/RegressionDefinitionMethod/ClassifierwithImputedData_July2018/FullData_Dec5_2017.csv")
All_IMPUTE_COMP <- read.csv("~/Dropbox/Definition_of_PCs/RegressionDefinitionMethod/ClassifierwithImputedData_July2018/All_IMPUTE_COMP.csv")
FullData_Dec5_2017$SubjectID <- as.factor(FullData_Dec5_2017$SubjectID)

merge <- merge(FullData_Dec5_2017, All_IMPUTE_COMP, by = "SubjectID", all = TRUE)

merge$ppvt_check <- merge$ppvt.raw.x - merge$ppvt.raw.y
merge[merge$ppvt_check != 0 & !is.na(merge$ppvt_check),]
merge$ppvt.raw.x[is.na(merge$ppvt.raw.x)] <- merge$ppvt.raw.y[is.na(merge$ppvt.raw.x)]
merge <- merge[,-29]
colnames(merge)[6] <- "ppvt.raw"

merge$wasi_check <- merge$wasi.matr.raw.x - merge$wasi.matr.raw.y
merge[merge$wasi_check != 0 & !is.na(merge$wasi_check),]
merge$wasi.matr.raw.x[is.na(merge$wasi.matr.raw.x)] <- merge$wasi.matr.raw.y[is.na(merge$wasi.matr.raw.x)]
merge <- merge[,-29]
colnames(merge)[8] <- "wasi.matr.raw"

merge$wj3.watt_check <- merge$wj3.watt.raw.x - merge$wj3.watt.raw.y
merge[merge$wj3.watt_check != 0 & !is.na(merge$wj3.watt_check),]
merge$wj3.watt.raw.x[is.na(merge$wj3.watt.raw.x)] <- merge$wj3.watt.raw.y[is.na(merge$wj3.watt.raw.x)]
merge <- merge[,-28]
colnames(merge)[10] <- "wj3.watt.raw"

merge$wj3.wid_check <- merge$wj3.wid.raw.x - merge$wj3.wid.raw.y
merge[merge$wj3.wid_check != 0 & !is.na(merge$wj3.wid_check),]
merge$wj3.wid.raw.x[is.na(merge$wj3.wid.raw.x)] <- merge$wj3.wid.raw.y[is.na(merge$wj3.wid.raw.x)]
merge <- merge[,-27]
colnames(merge)[11] <- "wj3.wid.raw"

merge$wj3.rcomp_check <- merge$wj3.rcomp.raw.x - merge$wj3.rcomp.raw.y
merge[merge$wj3.rcomp_check != 0 & !is.na(merge$wj3.rcomp_check),]
merge$wj3.rcomp.raw.x[is.na(merge$wj3.rcomp.raw.x)] <- merge$wj3.rcomp.raw.y[is.na(merge$wj3.rcomp.raw.x)]
merge <- merge[,-27]
colnames(merge)[12] <- "wj3.rcomp.raw"

merge$towre.w.ipm_check <- merge$towre.w.ipm.x - merge$towre.w.ipm.y
merge[merge$towre.w.ipm_check != 0 & !is.na(merge$towre.w.ipm_check),]
merge$towre.w.ipm.x[is.na(merge$towre.w.ipm.x)] <- merge$towre.w.ipm.y[is.na(merge$towre.w.ipm.x)]
merge <- merge[,-25]
colnames(merge)[16] <- "towre.w.ipm"
merge$towre.w.ipm[merge$towre.w.ipm_check != 0 & !is.na(merge$towre.w.ipm_check)] <- NA

merge$towre.nw.ipm_check <- merge$towre.nw.ipm.x - merge$towre.nw.ipm.y
merge[merge$towre.nw.ipm_check != 0 & !is.na(merge$towre.nw.ipm_check),]
merge$towre.nw.ipm.x[is.na(merge$towre.nw.ipm.x)] <- merge$towre.nw.ipm.y[is.na(merge$towre.nw.ipm.x)]
merge <- merge[,-25]
colnames(merge)[17] <- "towre.nw.ipm"
merge$towre.nw.ipm[merge$towre.nw.ipm_check != 0 & !is.na(merge$towre.nw.ipm_check)] <- NA


merge$age.beh[merge$age.beh == "N/A"] <- NA
merge$age.beh <- as.numeric(levels(merge$age.beh))[merge$age.beh]
merge$age_check <- round(merge$age.beh,2) - merge$age.tested
merge$age.beh[abs(merge$age_check) > 0.2 & !is.na(merge$age_check)] <- NA
merge$age.tested[abs(merge$age_check) > 0.2 & !is.na(merge$age_check)] <- NA

merge <- merge[,-c(28:35)]

write.csv(merge, "MergedData_sept24_2018.csv", row.names = FALSE)

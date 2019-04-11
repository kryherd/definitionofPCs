OC.LM1 <- data.frame(lmoc.cc$SubjectID, lmoc.cc$CI_group)
names(OC.LM1) <- c("SubjectID", "OC.LM1")

OC.RLM1 <- data.frame(rlmoc.cc$SubjectID, rlmoc.cc$CI_group)
names(OC.RLM1) <- c("SubjectID", "OC.RLM1")

PPVT.LM2 <- data.frame(lmpp.cc$SubjectID, lmpp.cc$CI_group)
names(PPVT.LM2) <- c("SubjectID", "PPVT.LM2")

PPVT.RLM2 <- data.frame(rlmpp.cc$SubjectID, rlmpp.cc$CI_group)
names(PPVT.RLM2) <- c("SubjectID", "PPVT.RLM2")

OC <- merge(OC.LM1, OC.RLM1, by = "SubjectID")
PPVT <- merge(PPVT.LM2, PPVT.RLM2, by = "SubjectID")

all <- merge(OC, PPVT, by = "SubjectID", all = TRUE)

all.data <- merge(all, data, by = "SubjectID", all.x = TRUE)
data$wj3.rf.ipm <- as.numeric(levels(data$wj3.rf.ipm))[data$wj3.rf.ipm]

dagoTest(data$age) 
dagoTest(data$sspan.raw)
dagoTest(data$wasi.vocab.raw)
dagoTest(data$wasi.matr.raw)
dagoTest(data$wasi.iq)
dagoTest(data$wj3.watt.raw)
dagoTest(data$wj3.wid.raw)
dagoTest(data$wj3.rcomp.raw)
dagoTest(data$wj3.oralcomp.raw)
dagoTest(data$celf.rs.raw)
dagoTest(data$celf.fs.raw)
dagoTest(data$towre.w.ipm)
dagoTest(data$towre.nw.ipm)
dagoTest(data$wj3.rf.ipm)

vars_tf <- c("sspan.raw", "wasi.matr.raw", "wasi.iq", "wj3.watt.raw", "wj3.wid.raw", "wj3.rcomp.raw", 
             "wj3.oralcomp.raw", "celf.rs.raw", "celf.fs.raw", "towre.w.ipm", "towre.nw.ipm", "wj3.rf.ipm")
pp_md_tf <- preProcess(data[,vars_tf], method = c("center", "scale", "YeoJohnson"), na.remove=T)

tf_data <- predict(pp_md_tf, data[,vars_tf])

data$sspan.tf <- tf_data$sspan.raw
data$wasi.matr.tf <- tf_data$wasi.matr.raw
data$wasi.iq.tf <- tf_data$wasi.iq
data$wj3.watt.tf <- tf_data$wj3.watt.raw
data$wj3.wid.tf <- tf_data$wj3.wid.raw
data$wj3.rcomp.tf <- tf_data$wj3.rcomp.raw
data$wj3.oralcomp.tf <- tf_data$wj3.oralcomp.raw
data$celf.rs.tf <- tf_data$celf.rs.raw
data$celf.fs.tf <- tf_data$celf.fs.raw
data$towre.w.tf <- tf_data$towre.w.ipm
data$towre.nw.tf <- tf_data$towre.nw.ipm
data$wj3.rf.tf <- tf_data$wj3.rf.ipm

data$age.c <- data$age - mean(data$age, na.rm = TRUE)

data$sspan.cs <- scale(data$sspan.raw)
data$wasi.matr.cs <- scale(data$wasi.matr.raw)
data$wasi.iq.cs <- scale(data$wasi.iq)
data$wj3.watt.cs <- scale(data$wj3.watt.raw)
data$wj3.wid.cs <- scale(data$wj3.wid.raw)
data$wj3.rcomp.cs <- scale(data$wj3.rcomp.raw)
data$wj3.oralcomp.cs <- scale(data$wj3.oralcomp.raw)
data$celf.rs.cs <- scale(data$celf.rs.raw)
data$celf.fs.cs <- scale(data$celf.fs.raw)
data$towre.w.cs <- scale(data$towre.w.ipm)
data$towre.nw.cs <- scale(data$towre.nw.ipm)
data$wj3.rf.cs <- scale(data$wj3.rf.ipm)

write.csv(all.data, "FourModelsExport_Dec21_2016.csv")



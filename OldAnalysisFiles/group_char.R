# Haskins Cutoffs
mean.BD_Raw <- data.frame(tapply(CI_Analysis2$BlockDesign_Raw, CI_Analysis2$Group, mean))
names(mean.BD_Raw) <- "BD_Raw"
mean.Matrices_Raw <- data.frame(tapply(CI_Analysis2$Matrices_Raw, CI_Analysis2$Group, mean))
names(mean.Matrices_Raw) <- "Matrices_Raw"
mean.WA_Raw <- data.frame(tapply(CI_Analysis2$WA_Raw, CI_Analysis2$Group, mean))
names(mean.WA_Raw) <- "WA_Raw"
mean.LW_Raw <- data.frame(tapply(CI_Analysis2$LW_Raw, CI_Analysis2$Group, mean))
names(mean.LW_Raw) <- "LW_Raw"
mean.PPVT_Raw <- data.frame(tapply(CI_Analysis2$PPVT_raw, CI_Analysis2$Group, mean))
names(mean.PPVT_Raw) <- "PPVT_Raw"
mean.KTEA_Raw <- data.frame(tapply(CI_Analysis2$KTEA_Raw, CI_Analysis2$Group, mean))
names(mean.KTEA_Raw) <- "KTEA_Raw"

raw.scores <- data.frame(mean.BD_Raw, mean.Matrices_Raw, mean.WA_Raw, mean.LW_Raw, mean.PPVT_Raw, mean.KTEA_Raw)
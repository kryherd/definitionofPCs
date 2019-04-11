library(ggplot2)

nowmc <- read.csv("NoWMCresid.csv")
wmc <- read.csv("WMCresid.csv")

nowmc <- nowmc[,-1]
names(nowmc) <- c("Subj", "nowmc.resid")
wmc <- wmc[,-1]
names(wmc) <- c("Subj", "wmc.resid")

new <- merge(nowmc, wmc, by="Subj", all=TRUE)

beh <- read.csv("UPCClassifierProject.csv")

new.beh <- merge(new, beh, by.x="Subj", by.y="subject", all.x = TRUE, all.y=FALSE)
new.beh$sspan.raw[new.beh$sspan.raw==4160] <- 41
new.beh$wj3.rf.ipm <- as.numeric(levels(new.beh$wj3.rf.ipm))[new.beh$wj3.rf.ipm]
# Reading Comprehension
cor.test(new.beh$nowmc.resid, new.beh$wj3.rcomp.raw)
p1 <- ggplot(new.beh, aes(x=nowmc.resid, y=wj3.rcomp.raw)) +
  geom_point(shape=1)  + xlab("Residual") + ylab("Reading Comprehension (WJ-III)") + theme_bw() +
  geom_smooth(method=lm, se=FALSE, color="black", size=.5)
print(p1)
cor.test(new.beh$wmc.resid, new.beh$wj3.rcomp.raw)
p1 <- ggplot(new.beh, aes(x=wmc.resid, y=wj3.rcomp.raw)) +
  geom_point(shape=1)  + xlab("Residual") + ylab("Reading Comprehension (WJ-III)") + theme_bw() +
  geom_smooth(method=lm, se=FALSE, color="black", size=.5)
print(p1)
# Vocabulary - WASI
cor.test(new.beh$nowmc.resid, new.beh$wasi.vocab.raw)
p1 <- ggplot(new.beh, aes(x=nowmc.resid, y=wasi.vocab.raw)) +
  geom_point(shape=1)  + xlab("Residual") + ylab("Vocabulary (WASI)") + theme_bw() +
  geom_smooth(method=lm, se=FALSE, color="black", size=.5)
print(p1)
cor.test(new.beh$wmc.resid, new.beh$wasi.vocab.raw)
p1 <- ggplot(new.beh, aes(x=wmc.resid, y=wasi.vocab.raw)) +
  geom_point(shape=1)  + xlab("Residual") + ylab("Vocabulary (WASI)") + theme_bw() +
  geom_smooth(method=lm, se=FALSE, color="black", size=.5)
print(p1)
# Matrix Reasoning
cor.test(new.beh$nowmc.resid, new.beh$wasi.matr.raw)
p1 <- ggplot(new.beh, aes(x=nowmc.resid, y=wasi.matr.raw)) +
  geom_point(shape=1)  + xlab("Residual") + ylab("Matrix Reasoning (WASI)") + theme_bw() +
  geom_smooth(method=lm, se=FALSE, color="black", size=.5)
print(p1)
cor.test(new.beh$wmc.resid, new.beh$wasi.matr.raw)
p1 <- ggplot(new.beh, aes(x=wmc.resid, y=wasi.matr.raw)) +
  geom_point(shape=1)  + xlab("Residual") + ylab("Matrix Reasoning (WASI)") + theme_bw() +
  geom_smooth(method=lm, se=FALSE, color="black", size=.5)
print(p1)
# Letter Word
cor.test(new.beh$nowmc.resid, new.beh$wj3.wid.raw)
p1 <- ggplot(new.beh, aes(x=nowmc.resid, y=wj3.wid.raw)) +
  geom_point(shape=1)  + xlab("Residual") + ylab("Word Decoding (WJ-III)") + theme_bw() +
  geom_smooth(method=lm, se=FALSE, color="black", size=.5)
print(p1)
cor.test(new.beh$wmc.resid, new.beh$wj3.wid.raw)
p1 <- ggplot(new.beh, aes(x=wmc.resid, y=wj3.wid.raw)) +
  geom_point(shape=1)  + xlab("Residual") + ylab("Word Decoding (WJ-III)") + theme_bw() +
  geom_smooth(method=lm, se=FALSE, color="black", size=.5)
print(p1)
# Word Attack
cor.test(new.beh$nowmc.resid, new.beh$wj3.watt.raw)
p1 <- ggplot(new.beh, aes(x=nowmc.resid, y=wj3.watt.raw)) +
  geom_point(shape=1)  + xlab("Residual") + ylab("Nonword Decoding (WJ-III)") + theme_bw() +
  geom_smooth(method=lm, se=FALSE, color="black", size=.5)
print(p1)
cor.test(new.beh$wmc.resid, new.beh$wj3.watt.raw)
p1 <- ggplot(new.beh, aes(x=wmc.resid, y=wj3.watt.raw)) +
  geom_point(shape=1)  + xlab("Residual") + ylab("Nonword Decoding (WJ-III)") + theme_bw() +
  geom_smooth(method=lm, se=FALSE, color="black", size=.5)
print(p1)
# Working Memory
cor.test(new.beh$nowmc.resid, new.beh$sspan.raw)
p1 <- ggplot(new.beh, aes(x=nowmc.resid, y=sspan.raw)) +
  geom_point(shape=1)  + xlab("Residual") + ylab("Working Memory (Sentence Span)") + theme_bw() +
  geom_smooth(method=lm, se=FALSE, color="black", size=.5)
print(p1)
cor.test(new.beh$wmc.resid, new.beh$sspan.raw)
p1 <- ggplot(new.beh, aes(x=wmc.resid, y=sspan.raw)) +
  geom_point(shape=1)  + xlab("Residual") + ylab("Working Memory (Sentence Span)") + theme_bw() +
  geom_smooth(method=lm, se=FALSE, color="black", size=.5)
print(p1)
# Age
cor.test(new.beh$nowmc.resid, new.beh$age)
p1 <- ggplot(new.beh, aes(x=nowmc.resid, y=age)) +
  geom_point(shape=1)  + xlab("Residual") + ylab("Age") + theme_bw() +
  geom_smooth(method=lm, se=FALSE, color="black", size=.5)
print(p1)
cor.test(new.beh$wmc.resid, new.beh$age)
p1 <- ggplot(new.beh, aes(x=wmc.resid, y=age)) +
  geom_point(shape=1)  + xlab("Residual") + ylab("Age") + theme_bw() +
  geom_smooth(method=lm, se=FALSE, color="black", size=.5)
print(p1)
# Vocabulary - PPVT
cor.test(new.beh$nowmc.resid, new.beh$ppvt.raw)
p1 <- ggplot(new.beh, aes(x=nowmc.resid, y=ppvt.raw)) +
  geom_point(shape=1)  + xlab("Residual") + ylab("Vocabulary (PPVT)") + theme_bw() +
  geom_smooth(method=lm, se=FALSE, color="black", size=.5)
print(p1)
cor.test(new.beh$wmc.resid, new.beh$ppvt.raw)
p1 <- ggplot(new.beh, aes(x=wmc.resid, y=ppvt.raw)) +
  geom_point(shape=1)  + xlab("Residual") + ylab("Vocabulary (PPVT)") + theme_bw() +
  geom_smooth(method=lm, se=FALSE, color="black", size=.5)
print(p1)
# WASI IQ
cor.test(new.beh$nowmc.resid, new.beh$wasi.iq)
p1 <- ggplot(new.beh, aes(x=nowmc.resid, y=wasi.iq)) +
  geom_point(shape=1)  + xlab("Residual") + ylab("IQ (WASI)") + theme_bw() +
  geom_smooth(method=lm, se=FALSE, color="black", size=.5)
print(p1)
cor.test(new.beh$wmc.resid, new.beh$wasi.iq)
p1 <- ggplot(new.beh, aes(x=wmc.resid, y=wasi.iq)) +
  geom_point(shape=1)  + xlab("Residual") + ylab("IQ (WASI)") + theme_bw() +
  geom_smooth(method=lm, se=FALSE, color="black", size=.5)
print(p1)
# TOWRE Words
cor.test(new.beh$nowmc.resid, new.beh$towre.w.ipm)
p1 <- ggplot(new.beh, aes(x=nowmc.resid, y=towre.w.ipm)) +
  geom_point(shape=1)  + xlab("Residual") + ylab("Word Fluency (IPM - TOWRE)") + theme_bw() +
  geom_smooth(method=lm, se=FALSE, color="black", size=.5)
print(p1)
cor.test(new.beh$wmc.resid, new.beh$towre.w.ipm)
p1 <- ggplot(new.beh, aes(x=wmc.resid, y=towre.w.ipm)) +
  geom_point(shape=1)  + xlab("Residual") + ylab("Word Fluency (IPM - TOWRE)") + theme_bw() +
  geom_smooth(method=lm, se=FALSE, color="black", size=.5)
print(p1)
# TOWRE Nonwords
cor.test(new.beh$nowmc.resid, new.beh$towre.nw.ipm)
p1 <- ggplot(new.beh, aes(x=nowmc.resid, y=towre.nw.ipm)) +
  geom_point(shape=1)  + xlab("Residual") + ylab("Nonword Fluency (IPM - TOWRE)") + theme_bw() +
  geom_smooth(method=lm, se=FALSE, color="black", size=.5)
print(p1)
cor.test(new.beh$wmc.resid, new.beh$towre.nw.ipm)
p1 <- ggplot(new.beh, aes(x=wmc.resid, y=towre.nw.ipm)) +
  geom_point(shape=1)  + xlab("Residual") + ylab("Nonword Fluency (IPM - TOWRE)") + theme_bw() +
  geom_smooth(method=lm, se=FALSE, color="black", size=.5)
print(p1)
# CELF Recalling Sentences
cor.test(new.beh$nowmc.resid, new.beh$celf.rs.raw)
p1 <- ggplot(new.beh, aes(x=nowmc.resid, y=celf.rs.raw)) +
  geom_point(shape=1)  + xlab("Residual") + ylab("CELF RS") + theme_bw() +
  geom_smooth(method=lm, se=FALSE, color="black", size=.5)
print(p1)
cor.test(new.beh$wmc.resid, new.beh$celf.rs.raw)
p1 <- ggplot(new.beh, aes(x=wmc.resid, y=celf.rs.raw)) +
  geom_point(shape=1)  + xlab("Residual") + ylab("CELF RS") + theme_bw() +
  geom_smooth(method=lm, se=FALSE, color="black", size=.5)
print(p1)
# CELF Formulating Sentences
cor.test(new.beh$nowmc.resid, new.beh$celf.fs.raw)
p1 <- ggplot(new.beh, aes(x=nowmc.resid, y=celf.fs.raw)) +
  geom_point(shape=1)  + xlab("Residual") + ylab("CELF FS") + theme_bw() +
  geom_smooth(method=lm, se=FALSE, color="black", size=.5)
print(p1)
cor.test(new.beh$wmc.resid, new.beh$celf.fs.raw)
p1 <- ggplot(new.beh, aes(x=wmc.resid, y=celf.fs.raw)) +
  geom_point(shape=1)  + xlab("Residual") + ylab("CELF FS") + theme_bw() +
  geom_smooth(method=lm, se=FALSE, color="black", size=.5)
print(p1)
# Reading Fluency
cor.test(new.beh$nowmc.resid, new.beh$wj3.rf.ipm)
p1 <- ggplot(new.beh, aes(x=nowmc.resid, y=wj3.rf.ipm)) +
  geom_point(shape=1)  + xlab("Residual") + ylab("Reading Fluency (IPM - WJ-III)") + theme_bw() +
  geom_smooth(method=lm, se=FALSE, color="black", size=.5)
print(p1)
cor.test(new.beh$wmc.resid, new.beh$wj3.rf.ipm)
p1 <- ggplot(new.beh, aes(x=wmc.resid, y=wj3.rf.ipm)) +
  geom_point(shape=1)  + xlab("Residual") + ylab("Reading Fluency (IPM - WJ-III)") + theme_bw() +
  geom_smooth(method=lm, se=FALSE, color="black", size=.5)
print(p1)
# Oral Comprehension
cor.test(new.beh$nowmc.resid, new.beh$wj3.oralcomp.raw)
p1 <- ggplot(new.beh, aes(x=nowmc.resid, y=wj3.oralcomp.raw)) +
  geom_point(shape=1)  + xlab("Residual") + ylab("Oral Comprehension (WJ-III)") + theme_bw() +
  geom_smooth(method=lm, se=FALSE, color="black", size=.5)
print(p1)
cor.test(new.beh$wmc.resid, new.beh$wj3.oralcomp.raw)
p1 <- ggplot(new.beh, aes(x=wmc.resid, y=wj3.oralcomp.raw)) +
  geom_point(shape=1)  + xlab("Residual") + ylab("Oral Comprehension (WJ-III)") + theme_bw() +
  geom_smooth(method=lm, se=FALSE, color="black", size=.5)
print(p1)




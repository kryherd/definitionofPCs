
x <- density(data$vocab.comp, bw="SJ", na.rm=T)
plot(x)
dagoTest(data$vocab.comp)
x <- density(data$ppvt.raw, bw="SJ", na.rm=T)
plot(x)
dagoTest(data$ppvt.raw)

data$ppvt.raw[data$ppvt.raw < 143.72] <- NA

x <- density(data$wasi.vocab.raw, bw="SJ", na.rm=T)
plot(x)
dagoTest(data$wasi.vocab.raw)

x <- density(data$wasi.matr.raw, bw="SJ", na.rm=T)
plot(x)
dagoTest(data$wasi.matr.raw)

x <- density(data$wj3.rcomp.raw, bw="SJ", na.rm=T)
plot(x)
dagoTest(data$wj3.rcomp.raw)

x <- density(data$wj3.oralcomp.raw, bw="SJ", na.rm=T)
plot(x)
dagoTest(data$wj3.oralcomp.raw)

data$wj3.oralcomp.raw[data$wj3.oralcomp.raw < 16.31] <- NA

x <- density(data$decoding.comp, bw="SJ", na.rm=T)
plot(x)
dagoTest(data$decoding.comp)

write.csv(data, "FullDataDec15_2016.csv")


data <- read.csv("FullData_FinalMay23_2017.csv")
data <- data[,-c(21:22)]
vocab.comp <-read.csv("vocab_composite.csv")
decoding.comp <- read.csv("decoding_composite.csv")
data2 <- merge(data, decoding.comp, by = "SubjectID", all.x = TRUE)
data3 <- merge(data2, vocab.comp, by = "SubjectID", all.x = TRUE)
data3 <- data3[,-c(21, 23)]

write.csv(data3, "FullData_FinalMay23_2017.csv")
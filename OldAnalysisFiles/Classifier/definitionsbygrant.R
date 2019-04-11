nowmc <- read.csv("df.groups.nowmc.KR.csv")
wmc <- read.csv("df.groups.wmc.KR.csv")

table(nowmc[, c("CI_group", "cohort")])
table(wmc[, c("CI_group", "cohort")])
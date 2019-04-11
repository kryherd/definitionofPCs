setwd("~/definitionofPCs")
library(tidyverse)

data <- read.csv("FullData_Sept27_2018.csv")

# total with MRI
data %>% 
  select(SubjectID, Project, age.mri, age.beh, gender, StructuralMRI.ID) %>%
  na.omit() %>%
  summarise(count = n())

# by gender
data %>% 
  select(SubjectID, Project, age.mri, age.beh, gender, StructuralMRI.ID) %>%
  na.omit() %>%
  group_by(gender) %>%
  summarise(count = n())

# age distribution
data %>% 
  select(SubjectID, Project, age.mri, age.beh, gender, StructuralMRI.ID) %>%
  na.omit() %>%
  ggplot(., aes(age.mri)) + geom_histogram(binwidth = 1, color = "black") + 
  theme_bw(14) + xlab("age at MRI") + ylab("count") +
  scale_x_continuous(breaks=c(13,14,15,16,17,18,19,20,21,22,23,24,25))

# age distribution by gender
data %>% 
  select(SubjectID, Project, age.mri, age.beh, gender, StructuralMRI.ID) %>%
  na.omit() %>%
  ggplot(., aes(age.mri)) + geom_histogram(binwidth = 1, color = "black") + 
  theme_bw(14) + xlab("age at MRI") + ylab("count") +
  scale_x_continuous(breaks=c(13,14,15,16,17,18,19,20,21,22,23,24,25)) +
  facet_grid(.~gender)




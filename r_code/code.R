library(readr)
library(tidyverse)
library(modeest)

StudentsPerformance <- read_csv("../StudentsPerformance.csv")

getmode <- function(data) {
  uniqdata <- unique(data)
  uniqdata[which.max(tabulate(match(data, uniqdata)))]
}

if (any(duplicated(StudentsPerformance))) {
  StudentsPerformance <- unique(StudentsPerformance)
  
  print("Duplicate data removed.")
} else {
  print("There is no duplicate data.")
}

if (any(is.na(StudentsPerformance))) {
  StudentsPerformance <- na.omit(StudentsPerformance)
  
  print("Missing data removed.")
} else {
  print("There is no missing data.")
}

sprintf("Mean of Math Score: %.3f", mean(StudentsPerformance$`math score`))

sprintf("Median of Math Score: %d", median(StudentsPerformance$`math score`))

sprintf("Mode of Math Score: %d", getmode(StudentsPerformance$`math score`))

sprintf("Mean of Reading Score: %.3f", mean(StudentsPerformance$`reading score`))

sprintf("Median of Reading Score: %d", median(StudentsPerformance$`reading score`))

sprintf("Mode of Reading Score: %d", getmode(StudentsPerformance$`reading score`))

sprintf("Mean of Writing Score: %.3f", mean(StudentsPerformance$`writing score`))

sprintf("Median of Writing Score: %d", median(StudentsPerformance$`writing score`))

sprintf("Mode of Writing Score: %d", getmode(StudentsPerformance$`writing score`))

# Start Here
library(readr)
library(tidyverse)
library(modeest)
library(ggplot2)
library(corrplot)

StudentsPerformance <- read_csv("../StudentsPerformance.csv")

getmode <- function(data) {
  uniqdata <- unique(data)
  uniqdata[which.max(tabulate(match(data, uniqdata)))]
}

# Data Pre-processing
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

# Measures of Central Tendency
sprintf("Mean of Math Scores: %.3f", mean(StudentsPerformance$`math score`))

sprintf("Median of Math Scores: %d", median(StudentsPerformance$`math score`))

sprintf("Mode of Math Scores: %d", getmode(StudentsPerformance$`math score`))

sprintf("Range of Math Scores: %d", max(StudentsPerformance$`math score`) - 
          min(StudentsPerformance$`math score`))

sprintf("IQR of Math Scores: %d", IQR(StudentsPerformance$`math score`))

sprintf("Variance of Math Scores: %.3f", var(StudentsPerformance$`math score`))

print("A summary of math scores:-")

summary(StudentsPerformance$`math score`)

sprintf("Mean of Reading Scores: %.3f", mean(StudentsPerformance$`reading score`))

sprintf("Median of Reading Scores: %d", median(StudentsPerformance$`reading score`))

sprintf("Mode of Reading Scores: %d", getmode(StudentsPerformance$`reading score`))

sprintf("Range of Reading Scores: %d", max(StudentsPerformance$`reading score`) - 
          min(StudentsPerformance$`reading score`))

sprintf("IQR of Reading Scores: %d", IQR(StudentsPerformance$`reading score`))

sprintf("Variance of Reading Scores: %.3f", var(StudentsPerformance$`reading score`))

print("A summary of Reading scores:-")

summary(StudentsPerformance$`reading score`)

sprintf("Mean of Writing Scores: %.3f", mean(StudentsPerformance$`writing score`))

sprintf("Median of Writing Scores: %d", median(StudentsPerformance$`writing score`))

sprintf("Mode of Writing Scores: %d", getmode(StudentsPerformance$`writing score`))

sprintf("Range of Writing Scores: %d", max(StudentsPerformance$`writing score`) - 
          min(StudentsPerformance$`writing score`))

sprintf("IQR of Writing Scores: %.3f", IQR(StudentsPerformance$`writing score`))

sprintf("Variance of Writing Scores: %.3f", var(StudentsPerformance$`writing score`))

print("A summary of Writing scores:-")

summary(StudentsPerformance$`writing score`)

# Box Plots
boxplot(StudentsPerformance$`math score`, StudentsPerformance$`reading score`,
        StudentsPerformance$`writing score`, main = "Scores BoxPlot",
        horizontal = TRUE,
        names = c("Math", "Reading", "Writing"),
        col = c("blue", "red", "orange"),
        ylab = "Tests", xlab = "Scores")

boxplot(StudentsPerformance$`math score` ~ StudentsPerformance$`gender`,
        main = "Male & Female Math Score BoxPlot",
        horizontal = TRUE,
        col = c("blue", "red"),
        ylab = "Gender", xlab = "Test Score")

boxplot(StudentsPerformance$`math score` ~ StudentsPerformance$`race/ethnicity`,
        main = "Group Math Score BoxPlot",
        horizontal = TRUE,
        col = c("blue", "red"),
        ylab = "Gender", xlab = "Test Score")

# Bar Charts
BarTable <- table(StudentsPerformance$`race/ethnicity`)

barplot(BarTable, main = "Participation of Groups", 
        ylab = "Number of Participants", xlab = "Group Name",
        density = 10)

BarTable <- table(StudentsPerformance$`parental level of education`)

barplot(BarTable, main = "Parental Level of Education", 
        ylab = "Number of Students", xlab = "Education Level",
        density = 10)

# Histograms
hist(StudentsPerformance$`math score`, 
     main = "Math Score Histogram",
     col = "blue",
     xlab = "Test Score")

hist(StudentsPerformance$`reading score`, 
     main = "Reading Score Histogram",
     col = "red",
     xlab = "Test Score")

hist(StudentsPerformance$`writing score`, 
     main = "Writing Score Histogram",
     col = "orange",
     xlab = "Test Score")

# Correlation Heat Matrix
corrplot(cor(StudentsPerformance[, sapply(StudentsPerformance, is.numeric)]), 
         method = "color",addCoef.col = "white", tl.col = "black")

# Scatter Plot
ggplot(StudentsPerformance, aes(x = StudentsPerformance$`reading score`, 
                                y = StudentsPerformance$`writing score`)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Reading/Writing Scatter Plot", x = "Reading Score", 
       y = "Writing Score")

# Regression Model
WritingScore <- StudentsPerformance$`writing score`
ReadingScore <- StudentsPerformance$`reading score`

RegressionModel <- lm(WritingScore ~ ReadingScore, StudentsPerformance)

summary(RegressionModel)

# Regression Model Predictions
InputData = data.frame(ReadingScore = c(72, 90, 95, 44, 2, 77, 86, 7))

View(predict(RegressionModel, InputData))

View(predict(RegressionModel, InputData, interval = "confidence", level = 0.95))

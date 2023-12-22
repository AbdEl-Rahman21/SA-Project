
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

sprintf("Mean of Math Scores: %.3f", mean(StudentsPerformance$`math score`))

sprintf("Median of Math Scores: %d", median(StudentsPerformance$`math score`))

sprintf("Mode of Math Scores: %d", getmode(StudentsPerformance$`math score`))

sprintf("Range of Math Scores: %d", max(StudentsPerformance$`math score`) - 
          min(StudentsPerformance$`math score`))

sprintf("IQR of Math Scores: %d", IQR(StudentsPerformance$`math score`))

sprintf("Variance of Math Scores: %.3f", var(StudentsPerformance$`math score`))

print("A summary of math scores:-")

summary(StudentsPerformance$`math score`)

boxplot(StudentsPerformance$`math score`, 
        main = "Math Score BoxPlot",
        horizontal = TRUE,
        xlab = "Test Score")

boxplot(StudentsPerformance$`math score` ~ StudentsPerformance$`gender`, 
        main = "Male & Female Math Score BoxPlot",
        horizontal = TRUE,
        ylab = "Gender", xlab = "Test Score")

GroupCount <- table(StudentsPerformance$`race/ethnicity`)

barplot(GroupCount, main = "Participation of Groups", 
        ylab = "Number of Participants", xlab = "Group Name",
        density = 10)

hist(StudentsPerformance$`math score`, 
     main = "Math Score Histogram",
     xlab = "Test Score")

corrplot(cor(StudentsPerformance[, sapply(StudentsPerformance, is.numeric)]), 
         method = "color",addCoef.col = "white", tl.col = "black")

ggplot(StudentsPerformance, aes(x = StudentsPerformance$`math score`, 
                                y = StudentsPerformance$`reading score`)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Math/Reading Scatter Plot", x = "Math Score", 
       y = "Reading Score")

model <- lm(StudentsPerformance$`reading score` ~ 
              StudentsPerformance$`math score`, StudentsPerformance)

summary(model)

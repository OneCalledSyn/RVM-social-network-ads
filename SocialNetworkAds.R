library(tidyverse)
library(ggplot2)
library(data.table)
library(caTools)
library(e1071)
library(ElemStatLearn)

#Read in dataset
ads <- fread("C:/Users/jays/Desktop/social.csv")

#Examine structure of the data
head(ads)
str(ads)

#Truncate columns to examine purchases by solely age and salary
ads <- ads[ , 3:5]
ads

#Purchased needs to be converted into a factor
ads$Purchased <- factor(ads$Purchased, levels = c(0,1))
ads

#Set a seed for model reproducibility
set.seed(123)

#Split dataset into training set and test set
split <- sample.split(ads$Purchased, SplitRatio = 0.75)
training <- subset(ads, split == TRUE)
test <- subset(ads, split == FALSE)

#Feature scaling can reduce time to find support vectors
training[ , 1] <- scale(training[ , 1])
test[ , 1] <- scale(test[ , 1])

test
training

training[ , 2] <- scale(training[ , 2])
test[ , 2] <- scale(test[ , 2])

test
training

#Fit Support Vector Machine to the training set
classifier = svm(formula = Purchased ~ ., 
                 data = training,
                 type = 'C-classification',
                 kernel = 'linear')

#Predicting test set results
test_pred <- predict(classifier, newdata = test[ , 1:2])

#Manually create a confusion matrix without special package
#test[ , 3]
#test_pred
#confusionMatrix <- table(test[ , c(1,2)], test_pred)

#Plotting results for the training set

X1 = seq(min(training[ , 1]) - 1, max(training[ , 1]) + 1, by = 0.01)
X2 = seq(min(training[ , 2]) - 1, max(training[ , 2]) + 1, by = 0.01)

grid_training <- expand.grid(X1, X2)
colnames(grid_training) <- c('Age', 'EstimatedSalary')
y_grid <- predict(classifier, newdata = grid_training)

plot(training[ , c(1,2)],
     main = 'SVM Training Set',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))

contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)

points(grid_training, pch = '.', col = ifelse(y_grid == 1, '#c8cbcf', '#b8cff5'))

points(training, pch = 21, bg = ifelse(training[ , 3] == 1, 'green4', 'red3'))

#Plotting results for the test set

X1 = seq(min(test[ , 1]) - 1, max(test[ , 1]) + 1, by = 0.01)
X2 = seq(min(test[ , 2]) - 1, max(test[ , 2]) + 1, by = 0.01)

grid_test <- expand.grid(X1, X2)
colnames(grid_test) <- c('Age', 'EstimatedSalary')
y_grid <- predict(classifier, newdata = grid_test)

plot(test[ , c(1,2)],
     main = 'SVM Test Set',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))

contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)

points(grid_test, pch = '.', col = ifelse(y_grid == 1, '#c8cbcf', '#b8cff5'))

points(test, pch = 21, bg = ifelse(training[ , 3] == 1, 'green4', 'red3'))

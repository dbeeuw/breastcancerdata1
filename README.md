# breastcancerdata1
---
title: "Breast Cancer Data"
author: "Dounia"
date: "2024-03-19"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown


options(repos = "https://cran.rstudio.com/")

```{r}
options(repos = "https://cran.rstudio.com/")
library(mlbench)
data(BreastCancer)
str(BreastCancer)
head(BreastCancer)
features <- BreastCancer[, 1:9]
library(caret)

set.seed(123)
train_index <- sample(nrow(BreastCancer), 0.7 * nrow(BreastCancer))
train_data <- BreastCancer[train_index, ]
test_data <- BreastCancer[-train_index, ]
str(test_data)

table(train_data$Class)
table(test_data$Class)

# Create at least four different classifiers. Determine the needed features and create training and test data.
sapply(train_data[, -1], class)
train_data[, -1] <- sapply(train_data[, -1], as.numeric)
train_data_numeric <- train_data[, sapply(train_data, is.numeric)]
train_data_scaled <- as.data.frame(scale(train_data_numeric))

# Random Forest
library(randomForest)
train_data <- na.omit(train_data)
train_data$Class <- as.factor(train_data$Class)
model_rf <- randomForest(Class ~ ., data = train_data)

# Support Vector Machines
library(e1071)
model_svm <- svm(Class ~ ., data = train_data)

# Decision Tree
library(rpart)
model_dt <- rpart(Class ~ ., data = train_data, method = "class")

# Naive Bayes
library(naivebayes)
model_nb <- naiveBayes(Class ~ ., data = train_data)

# Ensure 'Class' in test_data is a factor
test_data$Class <- as.factor(test_data$Class)

# Check and set levels
levels_test <- levels(test_data$Class)
levels_train <- levels(train_data$Class)
missing_levels <- setdiff(levels_train, levels_test)

# Add missing levels to test_data$Class
if (length(missing_levels) > 0) {
  test_data$Class <- factor(test_data$Class, levels = c(levels_test, missing_levels))
}

# Check factor levels in train_data$Class
levels_train <- levels(train_data$Class)
test_data$Class <- factor(test_data$Class, levels = levels(train_data$Class))
test_data <- na.omit(test_data)

setdiff(levels_test, levels_train)
str(train_data)
str(test_data)

# Combine train_data and test_data for retraining
combined_data <- rbind(train_data, test_data)

model_rf <- randomForest(Class ~ ., data = combined_data)
model_svm <- svm(Class ~ ., data = combined_data)
model_dt <- rpart(Class ~ ., data = combined_data, method = "class")
model_nb <- naiveBayes(Class ~ ., data = combined_data)

# Make predictions using the retrained models
ensemble_predictions <- function(...) {
  votes <- list(...)
  majority_vote <- function(x) {
    tab <- table(unlist(x))
    names(tab)[which.max(tab)]
  }
  apply(data.frame(...), 1, majority_vote)
}
predictions_rf <- c("1", "2", "1", "1", "2")  # Example predictions
predictions_svm <- c("1", "1", "2", "1", "2") # Example predictions
predictions_dt <- c("2", "1", "1", "2", "1")  # Example predictions
predictions_nb <- c("2", "2", "1", "1", "2")  # Example predictions
ensemble_predictions(predictions_rf, predictions_svm, predictions_dt, predictions_nb)
str(ensemble_predictions)
str(test_data$Class)
ensemble_predictions <- function(...) {
  votes <- list(...)
  majority_vote <- function(x) {
    tab <- table(unlist(x))
    names(tab)[which.max(tab)]
  }
  apply(data.frame(...), 1, majority_vote)
}

unique(test_data$Class)
sum(is.na(test_data$Class))
cat("\n")
```

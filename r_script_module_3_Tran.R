# Trang Tran, ALY6015, Module 3 Practice, May 3

cat("\014")  # clears console
rm(list = ls())  # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # clears packages
options(scipen = 100) # disables scientific notion for entire R session

library(pacman)
library(pROC)
p_load(tidyverse, caret, ISLR, skimr)

# import the data
attach(College)
head(College)
colnames(College)

# Create Train and Test set - maintain % of event rate (80/20 split)
set.seed(3456)
trainIndex <- createDataPartition(College$Private, p = 0.8, list = FALSE, times = 1)
caret_train <- College[ trainIndex,]
caret_test <- College[-trainIndex,]

# Perform EDA on the train set by using descriptive statistics and plots
skim(caret_train) #data types, missing values, min, max, mean, sd, hist

## bar plot of 'Private'
ggplot(data = caret_train, aes(x = Private, fill = Private)) +
  geom_bar()

## Boxplot of 'Outstate' by 'Private'
boxplot(Outstate ~ Private, data = caret_train) 

# Use the glm() function in the ‘stats’ package to fit a logistic regression model
#to the training set using at least two predictors.
model1 <- glm(Private ~ Top10perc + Outstate + PhD + S.F.Ratio,
              data = caret_train, family = binomial(link = 'logit'))
summary(model1)

# Create a confusion matrix and report the results of your model for the train set
## Predict the train set by model 1
pred_train <- predict(model1, data = caret_train, type = "response")
pred.classes.min <- as.factor(ifelse(pred_train >= 0.5, 'Yes', 'No'))

## Create confusion matrix for train set
conf_matrix_train <- confusionMatrix(pred.classes.min, caret_train$Private,
                                     positive = 'Yes')
print(conf_matrix_train)

## Report and interpret metrics for Accuracy, Precision, Recall, and Specificity
conf_matrix_train$overall["Accuracy"]
conf_matrix_train$byClass["Precision"]
conf_matrix_train$byClass["Recall"]
conf_matrix_train$byClass["Specificity"]

# Create a confusion matrix and report the results of your model for the test set.
## Predict the test set by model 1
pred_test <- predict(model1, newdata = caret_test, type = "response")
pred.classes.min.test <- as.factor(ifelse(pred_test >= 0.5, 'Yes', 'No'))

## Create confusion matrix for test set
conf_matrix_test <- confusionMatrix(pred.classes.min.test, caret_test$Private,
                                     positive = 'Yes')
print(conf_matrix_test)

## Report and interpret metrics for Accuracy, Precision, Recall, and Specificity
conf_matrix_test$overall["Accuracy"]
conf_matrix_test$byClass["Precision"]
conf_matrix_test$byClass["Recall"]
conf_matrix_test$byClass["Specificity"]

# Plot and interpret the ROC curve for the test set
ROC1 <- roc(caret_test$Private, pred_test) 
plot(ROC1, col="blue", ylab = 'Sensitivity - TP Rate', xlab = 'Specificity - FP Rate')
abline(a=0, b=1, lty=2, col="gray")

# Calculate and interpret the AUC for the test set
auc1 <- auc(ROC1)
auc1

install.packages("caTools")
install.packages("caret")
install.packages("stats")
install.packages("ROCR")
library(stats)
library(ggplot2)
library(caTools)
library(ROCR)
library(dplyr)
library(tidyr)
library(caret)

#loading the dataset
print(new_dataset)

log <- na.omit(new_dataset)


# Replace "FW" with 1 and "MF" or "MF, FW" with 0 to make Pos a categorical 
#variable for logistic regression
log$Pos <- ifelse(log$Pos == "FW", 1, 0)
log

#Splitting the data using to test and train using logistic regression
set.seed(123)

# Split the data into training and testing sets
train_indices.log <- sample.split(log$Pos, SplitRatio = 0.80)

train_data.log <- log[train_indices.log, ]
test_data.log <- log[-train_indices.log, ]

#Performing Logistic Regression to train data
train_data.log <- na.omit(train_data.log)
log_model <- glm(Pos ~ Sh + SoT + Dist + FK + PKatt + Gls, data = train_data.log)
log_model
summary(log_model)

#Make predictions on test data using trained data
predictions.log <- predict(log_model, newdata = test_data.log, type = "response")
predictions.log

#creating confusion matrix
predicted_data <- ifelse(predictions.log > 0.5, 1, 0)
predicted_data

#evaluation of the logistic model using confusion matrix
confusion_matrix <- table(predicted_data, test_data.log$Pos)
confusion_matrix
#checking accuracy using confusion matrix
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy #Getting 89.352% accuracy which means that the logistic model is working fine

##This logistic model predicts the position of the player based on variables like
#Sh, SoT, Gls, Dist, PKatt
#to do this, first the position varaiable is turned into binary varaiable where 
#1 represents forwards and 0 represents non-FW
#there is 79.329% accuracy which means that the model predicts the position correctly about 87.8787%






install.packges("caret")
install.packages("lattice")
library(stats)
library(dplyr)
library(ggplot2)
library(caret)
library(lattice)


set.seed(123)


# Split the data into training and testing sets
# Assuming your dataset is stored in a variable called "final_data"


# Generate random indices for the training set
train_indices <- sample(nrow(final_data), size = 0.8 * nrow(final_data), replace = FALSE)

# Create the training and test datasets
train_data <- final_data[train_indices, ]
test_data <- final_data[-train_indices, ]



#Training the predictive model using linear regression
train_data <- na.omit(train_data)
linear.model <- train(Gls ~ Sh + SoT + Dist + FK + PKatt, data = train_data, method = "lm")
linear.model
summary(linear.model)


#Making predictions using training data in test data
prediction.LM <- predict(linear.model, newdata = test_data)
prediction.LM


#Evaluationg the model using Means Squared Error
MSE.LM <- mean((test_data$Gls - prediction.LM)^2)
MSE.LM


#Evaluating the model using mean absolute error
MAE.LM <- mean(abs(test_data$Gls - prediction.LM))
MAE.LM





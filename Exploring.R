library(dplyr)
library(tidyr)


football.shooting.stats <- read.csv("C://Users//hanzalah//OneDrive//Desktop//DSF Final Project Folder//football.shooting.stats.csv", na.strings = c(""))
football.shooting.stats

View(football.shooting.stats)
str(football.shooting.stats)
dim(football.shooting.stats)
head(football.shooting.stats, 20)
tail(football.shooting.stats, 20)
summary(football.shooting.stats)

dim(football.shooting.stats)

#Wrangling in R:
library(dplyr)


#Selecting only the required columns to work on

new_dataset <- football.shooting.stats %>% select(Player, Nation, Squad, Pos, Sh, SoT, Dist, FK, PK, PKatt, Gls)
new_dataset
##rename columns

#Now filtering according to position as we only need to analyze shooting of offensive players
filterd_data <- new_dataset  %>% filter(Pos == "MF" | Pos == "FW" | Pos == "FW,MF")
filterd_data
dim(filterd_data)

#Tidying in R:
library(tidyverse)
library(dplyr)
library(stats)
library(tidyr)

#separating nationality of the player into two columns to short form and long form

separate_data <- filterd_data %>% separate(Nation, into = c("Prefix", "Nationality"), sep = " ")
separate_data

#now removing prefix column as it is unnecessaray
final_data <- separate_data %>% select(Player, Nationality, Squad, Pos, Sh, SoT, Dist, FK, PKatt, PK, Gls)

final_data



#Linear Regression - Goals:
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



#Visualizaiton in R:
library(ggplot2)

# Create a data frame with actual and predicted values given by LM model
plot_data.LM <- data.frame(Actual = test_data$Gls, Predicted = prediction.LM)


# scatter plot of predicted goals vs actual goals using linear regression



ggplot(plot_data.LM, aes(x = Actual, y = Predicted)) +
  geom_point(color = "purple", alpha = 0.6, size = 4) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "yellow", size = 1) +
  labs(x = "Actual Goals", y = "Predicted Goals") +
  ggtitle("Predicted Goals vs Actual Goals (Linear Model)") +
  theme_dark() +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    axis.text = element_text(size = 12, color = "black"),
    legend.position = "none",
    panel.background = element_rect(fill = "black"))




# line plot of predicted goals vs actual goals using linear regression
ggplot(plot_data.LM, aes(x = 1:nrow(plot_data.LM), y = Actual, group = 1)) +
  geom_line(color = "blue", size = 1) + #actual goals
  geom_line(aes(y = Predicted), color = "red", size = 1) + #predicted goals
  labs(x = "Index", y = "Goals") +
  ggtitle("Predicted Goals vs Actual Goals (Linear Model") +
  theme_dark() +
  theme(plot.title = element_text(size = 16, face = "bold", color = "black"),
        axis.title = element_text(size = 14, color = "black"),
        axis.text = element_text(size = 12, color = "black"),
        panel.background = element_rect(fill = "black"))


# Reshape data into long format #tidying
plot_data_long <- plot_data.LM %>%
  gather(key = "Variable", value = "Value", Predicted, Actual)

# Boxplot of Predicted and Actual Goals Using LM Model
ggplot(plot_data_long, aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot() +
  labs(x = "Variable", y = "Goals") +
  ggtitle("Predicted Goals vs Actual Goals (Linear Model)") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.background = element_rect(fill = "gray"))



#Logistic Regression in R:
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



#Visualization of Logistic Regression:
library(ggplot2)
library(caTools)
library(caret)
library(ROCR)
library(pROC)

predicted_prob <- predict(log_model, newdata = test_data.log, type = "response")
predicted_prob


pred_obj <- prediction(predicted_prob, test_data.log$Pos)

#creating ROC Curve
roc_data <- performance(pred_obj, "tpr", "fpr")
#plotting
plot(roc_data, main = "ROC Curve", col = "darkblue", lwd = 3, xlab = "Specificity", ylab = "Sensitivity")  

# Calculate the AUC value
auc_value <- performance(pred_obj, "auc")@y.values[[1]] #area under the curve
auc_value #84.91% area is under the roc curve which means that the model is working v well
#high AUC value means better performance



plot_data.log <- data.frame(Actual = test_data.log$Pos, Predicted = predictions.log)
plot_data.log

#visualizing using probability distribution
ggplot(plot_data.log, aes(x = Predicted, fill = factor(Actual))) +
  geom_density(alpha = 0.6) +
  xlab("Predicted Probability") +
  ylab("Density") +
  ggtitle("Probability Distribution Plot") +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    axis.text = element_text(size = 12, color = "black"),
    legend.position = "none",
    panel.background = element_rect(fill = "white"))

#boxplot
ggplot(plot_data.log, aes(x = Actual, y = Predicted)) +
  geom_boxplot() + 
  xlab("Actual") +
  ylab("Predicted Probabilities") +
  ggtitle("Box Plot")


#Visualization and different relationships:
library(ggplot2)
library(dplyr)
library(tidyr)

print(final_data)

final_data$Player <- iconv(final_data$Player, from = "UTF-8", to = "UTF-8")

##Goals-Shots relationship
ggplot(final_data, aes(x = Sh, y = Gls)) + 
  geom_point(color = "darkred", size = 3) +
  geom_smooth(method = "lm", color = "green", size = 1.5) +
  labs(x = "Shots Taken", y = "Goals Scored", title = "Goals-Shot relationship") +
  theme(panel.background = element_rect(fill = "yellow"),
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10, face = "bold"))

##Goals-Shots on target relationship
ggplot(final_data, aes(x = SoT, y = Gls)) + 
  geom_point(color = "yellow", size = 3) +
  geom_smooth(method = "lm", color = "green", size = 1.5) +
  labs(x = "Shots on Target", y = "Goals Scored", title = "Goals-Shot on Target relationship") +
  theme(panel.background = element_rect(fill = "purple"),
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10, face = "bold"))

##Goals-Distance relationship
ggplot(final_data, aes(x = Dist, y = Gls)) + 
  geom_point(color = "white", size = 3) +
  geom_smooth(method = "lm", color = "cyan", size = 1.5) +
  labs(x = "Distance From", y = "Goals Scored", title = "Goals-Distance From Goal relationship") +
  theme(panel.background = element_rect(fill = "brown"),
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10, face = "bold"))

##Goals-Freekick Relationship
ggplot(final_data, aes(x = FK, y = Gls)) + 
  geom_point(color = "red", size = 3) +
  geom_smooth(method = "lm", color = "yellow", size = 1.5) +
  labs(x = "Freekicks", y = "Goals Scored", title = "Goals-Freekicks relationship") +
  theme(panel.background = element_rect(fill = "darkgreen"),
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10, face = "bold"))

##Goals-Penalties Taken
ggplot(final_data, aes(x = PKatt, y = Gls)) + 
  geom_point(color = "yellow", size = 3) +
  geom_smooth(method = "lm", color = "navy", size = 1.5) +
  labs(x = "Penalties Taken", y = "Goals Scored", title = "Goals-Penalty Kicks Taken relationship") +
  theme(panel.background = element_rect(fill = "darkslategray"),
        plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10, face = "bold"))

#Comparing the regression models:
# Compare Linear Regression and Logistic Regression models
comparison <- data.frame(Model = c("Linear Regression", "Logistic Regression"),
                         MSE = c(MSE.LM, "NA"),
                         MAE = c(MAE.LM, "NA"),
                         Accuracy = c("NA", accuracy),
                         AUC = c("NA", auc_value))



comparison

#Top 10 scorers:
library(dplyr)
library(ggplot2)
library(tidyr)
library(stats)
install.packages("data.table")
library(data.table)

#printing the original final data table
print(final_data)
dim(final_data)

#removing all missing values from final data table and creating a new dataset so it has same 
#number of rows as the number of predicted values
final_data2 <- final_data[complete.cases(final_data[, c("Sh", "SoT", "Dist", "FK", "PKatt")]), ]

#performing linear regression
lm2 <- lm(Gls ~ Sh +SoT + Dist + FK + PKatt, data = final_data2)
#checking predicted goals according to Linear regression
preGoals <- predict(lm2)
#mutating predicted goals values into the final_data2 dataset that we created that has same no. of rows as 
#no of predicted values
predicted_final <- final_data2 %>% mutate(predicted_Gls = preGoals)
predicted_final

#creating a new dataset that only has top 10 goalscorers
arranged_desc <- predicted_final %>% arrange(desc(Gls))
top10 <- head(arranged_desc, 10)
top10







# Convert character encoding to UTF-8
top10$Player <- iconv(top10$Player, from = "UTF-8", to = "UTF-8")

# Create the bar char
ggplot(top10, aes(x = Player)) +
  geom_bar(aes(y = Gls, fill = "Actual Goals"), stat = "identity", width = 0.4) +
  geom_bar(aes(y = predicted_Gls, fill = "Predicted Goals"), stat = "identity", width = 0.4) +
  scale_fill_manual(values = c("Actual Goals" = "darkgreen", "Predicted Goals" = "gold")) +
  labs(x = "Player", y = "Goals") +
  ggtitle("Actual Goals vs Predicted Goals") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.title = element_blank(),
    legend.position = "top",
    panel.background = element_rect(fill = "lightblue"))



#K means in our dataset:
#K means:
install.packages("factoextra")
library(ggplot2)
library(factoextra)
install.packages("ggfortify")
library(ggfortify)


noNa <- na.omit(new_dataset)
noNa

noNa <- noNa %>% select(-PK, - PKatt)

noNa <- noNa %>% filter(Pos == "MF" | Pos == "FW")
noNa
position <- noNa$Pos
position

table(position)

#extracting the numeric columns
k_data <- noNa[5:9]
k_data



#filtering a sample


#Scaled data 
k_data_scaled <- scale(k_data)
k_data_scaled

#distance 
k_data <- dist(k_data_scaled)
k_data


#checking optimal no of clusters
fviz_nbclust(k_data_scaled, kmeans, method = "wss") + 
  labs(subtitle = "Elbow Graph")


#choosing 3 clusters

k_out <- kmeans(k_data_scaled, centers = 3, nstart = 10)
k_out
#between_SS / total_SS = 61.9% which means that 61.9% of data is similar to other values in the cluster

k_cluster <- k_out$cluster

fviz_cluster(list(data = k_data_scaled, cluster = k_cluster))


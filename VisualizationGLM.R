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


  

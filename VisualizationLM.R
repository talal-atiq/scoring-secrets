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






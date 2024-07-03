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


















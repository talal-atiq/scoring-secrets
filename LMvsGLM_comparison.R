# Compare Linear Regression and Logistic Regression models
comparison <- data.frame(Model = c("Linear Regression", "Logistic Regression"),
                         MSE = c(MSE.LM, "NA"),
                         MAE = c(MAE.LM, "NA"),
                         Accuracy = c("NA", accuracy),
                         AUC = c("NA", auc_value))



comparison


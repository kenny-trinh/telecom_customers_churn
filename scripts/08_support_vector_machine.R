# Load necessary libraries
library(e1071)
library(caret)
library(ggplot2)
library(pROC)
library(dplyr)

# Load the cleaned dataset
data <- read_csv(
  file.path(cleaned_data_dir, "telecom_customers_churn_cleaned.csv")
)

# Check for missing values
sum(is.na(data))

# Convert target variable to a factor
data$Churn <- as.factor(data$Churn)

# Split the dataset into training and testing sets
set.seed(123) # For reproducibility
trainIndex <- createDataPartition(data$Churn, p = 0.8, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Train the SVM model
svm_model <- svm(Churn ~ ., data = trainData, kernel = "radial", cost = 1, 
                 gamma = 0.1)

# Make predictions on the test data
predictions <- predict(svm_model, testData)

# Evaluate the model
confusionMatrix(predictions, testData$Churn)

# Tune the SVM model for optimal hyperparameters
set.seed(123)
tuned_parameters <- tune(svm, Churn ~ ., data = trainData, 
                         ranges = list(cost = c(0.1, 1, 10), 
                                       gamma = c(0.01, 0.1, 1)))

# Best model
best_model <- tuned_parameters$best.model

# Make predictions with the best model
best_predictions <- predict(best_model, testData)

# Evaluate the best model
confusionMatrix(best_predictions, testData$Churn)

# Check the best parameters chosen during tuning
best_parameters <- tuned_parameters$best.parameters
print(best_parameters)


# ---Evaluate additional metrics for the tuned model---
# Ensure the predicted and actual values are factors
best_predictions <- as.factor(best_predictions)
testData$Churn <- as.factor(testData$Churn)

# Generate the confusion matrix using caret explicitly
conf_matrix <- caret::confusionMatrix(data = best_predictions,
                                      reference = testData$Churn)

# Extract Accuracy
accuracy <- conf_matrix$overall["Accuracy"]
cat("Accuracy:", accuracy, "\n")

# Extract Precision, Recall, and F1-Score
precision <- conf_matrix$byClass["Pos Pred Value"]
recall <- conf_matrix$byClass["Sensitivity"]
f1_score <- 2 * (precision * recall) / (precision + recall)

cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1-Score:", f1_score, "\n")

# ---Generate the ROC curve and calculate AUC---
# Get decision values (raw scores) for predictions
svm_decision_values <- predict(best_model, testData, decision.values = TRUE)

# Extract decision values for the ROC curve
decision_values <- as.numeric(attr(svm_decision_values, "decision.values"))

# Recalculate the ROC curve
roc_curve <- roc(response = testData$Churn,
                 predictor = decision_values,
                 levels = rev(levels(testData$Churn)))

# Plot the corrected ROC curve
plot(roc_curve, col = "blue", main = "Corrected ROC Curve for Tuned SVM Model", lwd = 2)
auc_value <- auc(roc_curve)
cat("Corrected AUC:", auc_value, "\n")


# ---Extracting Churn Probabilities---

# Train the SVM model with probability estimates enabled
svm_model <- svm(Churn ~ ., data = trainData, kernel = "radial", 
                 cost = 1, gamma = 0.1, probability = TRUE)

# Tune the SVM model with probability
# tuned_parameters <- tune(svm, Churn ~ ., data = trainData,
#                          ranges = list(cost = c(0.1, 1, 10), 
#                                        gamma = c(0.01, 0.1, 1)),
#                          probability = TRUE)

# run with best parameters cost 10 and gamma 0.1 instead of tuning it again
tuned_parameters <- tune(svm, Churn ~ ., data = trainData,
                         ranges = list(cost = 10, 
                                       gamma = 0.1),
                         probability = TRUE)

# Best model with probabilities
best_model <- tuned_parameters$best.model


# Check the best parameters chosen during tuning
best_parameters <- tuned_parameters$best.parameters
print(best_parameters)


# Predict churn probabilities using the best model
probabilities <- predict(best_model, testData, probability = TRUE)

# Extract probabilities
probabilities_df <- attr(probabilities, "probabilities")
head(probabilities_df)  # View predicted probabilities for both classes


# ---Segmenting Customers Based on Risk Levels---
# Add churn probabilities and predicted class to the test data
testData$Churn_Prob <- probabilities_df[, "Yes"]
testData$Risk_Group <- cut(testData$Churn_Prob, 
                           breaks = c(0, 0.3, 0.7, 1),
                           labels = c("Low Risk", "Medium Risk", "High Risk"))

# View segmented data
head(testData[, c("Churn_Prob", "Risk_Group")])

# ---Visualizing Customer Segmentation---
# Plot churn probability by risk group
ggplot(testData, aes(x = Risk_Group, y = Churn_Prob)) +
  geom_boxplot(aes(fill = Risk_Group)) +
  ggtitle("Customer Segmentation by Churn Risk") +
  xlab("Risk Group") +
  ylab("Churn Probability") +
  theme_minimal()

# ---Grouping Customers by Key Features---
# Summarize average monthly charges by risk group
segmentation_summary <- testData %>%
  group_by(Risk_Group) %>%
  summarise(
    Avg_MonthlyCharges = mean(MonthlyCharges),
    Avg_Tenure = mean(tenure),
    Count = n()
  )

print(segmentation_summary)

# Save the full test dataset with churn probabilities and risk groups
write.csv(testData, file = file.path(output_dir, "segmented_test_data.csv"),
          row.names = FALSE)

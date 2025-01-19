# Load necessary libraries
library(e1071)
library(caret)
library(ggplot2)
library(pROC)
library(dplyr)

# ---- Preparation ----

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

# ---- Initial SVM Model ----
# Train the SVM model
svm_model <- svm(Churn ~ ., data = trainData, kernel = "radial", cost = 1, 
                 gamma = 0.1)

# Make predictions on the test data
predictions <- predict(svm_model, testData)

# Evaluate the initial model
cat("\nConfusion Matrix for Initial SVM Model:\n")
confusionMatrix(predictions, testData$Churn)

# Paths to save tuned model and parameters
tuned_model_path <- "output/svm_tuned_model.rds"
tuned_params_path <- "output/svm_tuned_parameters.rds"

# ---- Tuned SVM Model ----

# Tune the SVM model for optimal hyperparameters or load if already saved
set.seed(123)
if (!file.exists(tuned_model_path) || !file.exists(tuned_params_path)) {
  cat("\nTuning SVM Model...\n")
  tuned_parameters <- tune(svm, Churn ~ ., data = trainData,
                           ranges = list(cost = c(0.1, 1, 10), 
                                         gamma = c(0.01, 0.1, 1)),
                           probability = TRUE)
  best_model <- tuned_parameters$best.model
  saveRDS(best_model, tuned_model_path)
  saveRDS(tuned_parameters, tuned_params_path)
} else {
  cat("\nLoading Tuned SVM Model...\n")
  best_model <- readRDS(tuned_model_path)
  tuned_parameters <- readRDS(tuned_params_path)
}

# Make predictions with the best model
best_predictions <- predict(best_model, testData, probability = TRUE)

# Evaluate the best model
cat("\nConfusion Matrix for Tuned SVM Model:\n")
conf_matrix <- confusionMatrix(best_predictions, testData$Churn)
print(conf_matrix)

# Extract probabilities
probabilities <- attr(predict(best_model, testData, probability = TRUE), "probabilities")

# ---- BEST PARAMETERS ----
# Check the best parameters chosen during tuning
cat("\nBest Parameters Chosen During Tuning:\n")
print(tuned_parameters$best.parameters)

# ---- PROBABILITIES ----
# Extract probabilities
probabilities <- attr(best_predictions, "probabilities")
cat("\nSample of Predicted Probabilities:\n")
print(head(probabilities))  # Display a sample of probabilities

# ---- Evaluate additional metrics for the tuned model ----
# Ensure the predicted and actual values are factors
best_predictions <- as.factor(best_predictions)
testData$Churn <- as.factor(testData$Churn)

# Generate the confusion matrix using caret explicitly
conf_matrix <- caret::confusionMatrix(data = best_predictions,
                                      reference = testData$Churn)

# Extract and print key metrics
accuracy <- conf_matrix$overall["Accuracy"]
precision <- conf_matrix$byClass["Pos Pred Value"]
recall <- conf_matrix$byClass["Sensitivity"]
f1_score <- 2 * (precision * recall) / (precision + recall)

cat("Accuracy:", round(accuracy, 4), "\n")
cat("Precision:", round(precision, 4), "\n")
cat("Recall:", round(recall, 4), "\n")
cat("F1-Score:", round(f1_score, 4), "\n")

# ---- Generate the ROC curve and calculate AUC ----
# Get decision values (raw scores) for predictions
svm_decision_values <- predict(best_model, testData, decision.values = TRUE)

# Extract decision values for the ROC curve
decision_values <- as.numeric(attr(svm_decision_values, "decision.values"))

# Recalculate and plot the ROC curve with the tuned SVM model
roc_curve <- roc(response = testData$Churn,
                 predictor = decision_values,
                 levels = rev(levels(testData$Churn)))

# Plot the ROC curve for the tuned SVM model
plot(roc_curve, col = "blue", main = "ROC Curve for Tuned SVM Model", lwd = 2)
auc_value <- auc(roc_curve)
cat("AUC:", round(auc_value, 4), "\n")


# ---- Segmenting Customers Based on Risk Levels ----
# Add churn probabilities and predicted class to the test data
testData$Churn_Prob <- probabilities[, "Yes"]
testData$Risk_Group <- cut(testData$Churn_Prob, 
                           breaks = c(0, 0.3, 0.7, 1),
                           labels = c("Low Risk", "Medium Risk", "High Risk"))

# View segmented data
head(testData[, c("Churn_Prob", "Risk_Group")])

# ---- Visualizing Customer Segmentation ----
# Plot churn probability by risk group
ggplot(testData, aes(x = Risk_Group, y = Churn_Prob)) +
  geom_boxplot(aes(fill = Risk_Group)) +
  ggtitle("Customer Segmentation by Churn Risk") +
  xlab("Risk Group") +
  ylab("Churn Probability") +
  theme_minimal()

# ---- Grouping Customers by Key Features ----
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

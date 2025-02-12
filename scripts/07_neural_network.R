# Load necessary libraries
library(caret)
library(nnet)
library(pROC)
library(ggplot2)
library(dplyr)

# ---- Preparation ----

# Load the cleaned dataset
data <- read_csv(
  file.path(cleaned_data_dir, "telecom_customers_churn_cleaned.csv")
)

# Convert categorical variables to factors
data$Churn <- as.factor(ifelse(data$Churn == "Yes", 1, 0))

# Check and convert other categorical variables
data$gender <- as.factor(data$gender)
data$Partner <- as.factor(data$Partner)
data$PhoneService <- as.factor(data$PhoneService)
data$MultipleLines <- as.factor(data$MultipleLines)
data$InternetService <- as.factor(data$InternetService)
data$OnlineSecurity <- as.factor(data$OnlineSecurity)
data$OnlineBackup <- as.factor(data$OnlineBackup)
data$DeviceProtection <- as.factor(data$DeviceProtection)
data$TechSupport <- as.factor(data$TechSupport)
data$StreamingTV <- as.factor(data$StreamingTV)
data$StreamingMovies <- as.factor(data$StreamingMovies)
data$Contract <- as.factor(data$Contract)
data$PaperlessBilling <- as.factor(data$PaperlessBilling)
data$PaymentMethod <- as.factor(data$PaymentMethod)

# Convert TotalCharges to numeric (handle any non-numeric issues)
data$TotalCharges <- as.numeric(as.character(data$TotalCharges))

data <- na.omit(data)  # Remove rows with missing values

# Split the data into training and testing sets
set.seed(42)
trainIndex <- createDataPartition(data$Churn, p = 0.8, list = FALSE)
trainData <- data[trainIndex, ]
testData <- data[-trainIndex, ]

# Preprocess the data: scale numeric columns
preProc <- preProcess(trainData[, c("tenure", "MonthlyCharges", "TotalCharges")], method = c("center", "scale"))
trainData[, c("tenure", "MonthlyCharges", "TotalCharges")] <- predict(preProc, trainData[, c("tenure", "MonthlyCharges", "TotalCharges")])
testData[, c("tenure", "MonthlyCharges", "TotalCharges")] <- predict(preProc, testData[, c("tenure", "MonthlyCharges", "TotalCharges")])



# ---- Train the Neural Network ----
nn_model <- nnet(
  Churn ~ ., data = trainData,
  size = 2, decay = 0.04, maxit = 300, trace = FALSE
)

# Predict on the test set
nn_prob <- predict(nn_model, testData, type = "raw")


# ---- Evaluate the model ----
# Generate predictions (binary classification at 0.5 threshold)
nn_pred <- as.factor(ifelse(nn_prob > 0.5, 1, 0))
testData$Churn <- as.factor(testData$Churn)

# Ensure levels of nn_pred and testData$Churn match
levels(nn_pred) <- levels(testData$Churn)

# Compute the confusion matrix
conf_matrix <- caret::confusionMatrix(data = nn_pred, reference = testData$Churn)

# Check the structure of conf_matrix
cat("Confusion Matrix:\n")
print(conf_matrix$table)  # Access the confusion table specifically

# Extract performance metrics
accuracy <- conf_matrix$overall["Accuracy"]
sensitivity <- conf_matrix$byClass["Sensitivity"]
specificity <- conf_matrix$byClass["Specificity"]
balanced_accuracy <- mean(c(sensitivity, specificity))  # Manually compute balanced accuracy if needed

# Display metrics
cat("\nPerformance Metrics:\n")
cat("Accuracy:", round(accuracy, 4), "\n")
cat("Sensitivity:", round(sensitivity, 4), "\n")
cat("Specificity:", round(specificity, 4), "\n")
cat("Balanced Accuracy:", round(balanced_accuracy, 4), "\n")

# Compute and plot the ROC curve
nn_roc <- pROC::roc(response = testData$Churn, predictor = as.numeric(nn_prob), 
                    levels = rev(levels(testData$Churn)))
# Plot the ROC curve
plot(nn_roc, col = "blue", main = "Neural Network ROC Curve", lwd = 2)
abline(a = 0, b = 1, col = "gray", lty = 2)  # Add diagonal reference line

# Calculate and display the AUC
auc_value <- auc(nn_roc)
cat("Neural Network AUC:", round(auc_value, 4), "\n")


# ---- Adjusting the Threshold ----

# Adjust the threshold to improve specificity
threshold <- 0.54  # Adjust to a higher value to improve specificity
nn_pred_adjusted <- as.factor(ifelse(nn_prob > threshold, 1, 0))

# Ensure matching levels between predicted and actual
levels(nn_pred_adjusted) <- levels(testData$Churn)

# Generate a confusion matrix for the adjusted threshold
conf_matrix_adjusted <- caret::confusionMatrix(data = nn_pred_adjusted, reference = testData$Churn)

# Display confusion matrix
cat("Confusion Matrix for Adjusted Threshold (", threshold, "):\n", sep = "")
print(conf_matrix_adjusted$table)

# Extract performance metrics
accuracy_adjusted <- conf_matrix_adjusted$overall["Accuracy"]
sensitivity_adjusted <- conf_matrix_adjusted$byClass["Sensitivity"]
specificity_adjusted <- conf_matrix_adjusted$byClass["Specificity"]
precision_adjusted <- conf_matrix_adjusted$byClass["Pos Pred Value"]
balanced_accuracy_adjusted <- mean(c(sensitivity_adjusted, specificity_adjusted))

# Print metrics
cat("\nPerformance Metrics for Adjusted Threshold:\n")
cat("Accuracy:", round(accuracy_adjusted, 4), "\n")
cat("Sensitivity:", round(sensitivity_adjusted, 4), "\n")
cat("Specificity:", round(specificity_adjusted, 4), "\n")
cat("Precision:", round(precision_adjusted, 4), "\n")
cat("Balanced Accuracy:", round(balanced_accuracy_adjusted, 4), "\n")

# Recalculate and plot the ROC curve for the adjusted predictions
nn_roc_adjusted <- pROC::roc(response = testData$Churn, predictor = as.numeric(nn_prob), 
                             levels = rev(levels(testData$Churn)))

# Plot the adjusted ROC curve
plot(nn_roc_adjusted, col = "red", 
     main = "Neural Network ROC Curve (Adjusted Threshold)", lwd = 2)
abline(a = 0, b = 1, col = "gray", lty = 2)  # Add diagonal reference line

# Calculate and display AUC
auc_value_adjusted <- auc(nn_roc_adjusted)
cat("Neural Network AUC (Adjusted Threshold):", round(auc_value_adjusted, 4), "\n")



# ---- Cost-Sensitive Learning with Weighted Resampling with caret ----

# Assign weights to each class (focus on minority class)
weights <- ifelse(trainData$Churn == 1, 1.5, 1)  # Adjust the weight ratio as needed

# Perform weighted resampling
trainData_resampled <- trainData[rep(1:nrow(trainData), times = weights), ]

# Train a Neural Network model on the resampled data
nn_model_resampled <- nnet(
  Churn ~ ., data = trainData_resampled,
  size = 2, decay = 0.04, maxit = 300, trace = FALSE
)

# Predict on the test set
nn_prob_resampled <- predict(nn_model_resampled, testData, type = "raw")

# Evaluate model performance
nn_pred_resampled <- as.factor(ifelse(nn_prob_resampled > 0.5, 1, 0))
conf_matrix_resampled <- confusionMatrix(nn_pred_resampled, testData$Churn)
print(conf_matrix_resampled)

# Plot the ROC Curve for the resampled model
nn_roc_resampled <- roc(testData$Churn, as.numeric(nn_prob_resampled), levels = rev(levels(testData$Churn)))
plot(nn_roc_resampled, col = "blue", main = "Neural Network ROC Curve (Resampled Data)")
auc_resampled <- auc(nn_roc_resampled)
cat("Neural Network AUC (Resampled Data):", auc_resampled, "\n")



# ---- Simple Removal Analysis ----

# Initialize a data frame to store performance metrics for each feature
feature_performance <- data.frame(Feature = colnames(trainData)[-which(colnames(trainData) == "Churn")],
                                  AUC = numeric(length(colnames(trainData)) - 1))

# Loop through each feature and train the model without that feature
for (i in seq_along(feature_performance$Feature)) {
  # Exclude one feature
  excluded_feature <- feature_performance$Feature[i]
  train_data_temp <- trainData[, !colnames(trainData) %in% excluded_feature]
  
  # Train the neural network
  nn_model_temp <- nnet(
    Churn ~ ., data = train_data_temp,
    size = 2, decay = 0.04, maxit = 300, trace = FALSE
  )
  
  # Predict on the test set (excluding the same feature)
  test_data_temp <- testData[, !colnames(testData) %in% excluded_feature]
  nn_prob_temp <- predict(nn_model_temp, test_data_temp, type = "raw")
  
  # Calculate AUC for the model without this feature
  roc_temp <- roc(testData$Churn, as.numeric(nn_prob_temp), levels = rev(levels(testData$Churn)))
  feature_performance$AUC[i] <- auc(roc_temp)
}

# Add a baseline AUC for comparison
baseline_model <- nnet(
  Churn ~ ., data = trainData,
  size = 2, decay = 0.04, maxit = 300, trace = FALSE
)
baseline_prob <- predict(baseline_model, testData, type = "raw")
baseline_roc <- roc(testData$Churn, as.numeric(baseline_prob), levels = rev(levels(testData$Churn)))
baseline_auc <- auc(baseline_roc)

# Display the performance results
feature_performance$Delta_AUC <- baseline_auc - feature_performance$AUC
feature_performance <- feature_performance[order(-feature_performance$Delta_AUC), ]  # Sort by impact
print(feature_performance)

# Visualize the impact of excluding features

ggplot(feature_performance, aes(x = reorder(Feature, Delta_AUC), y = Delta_AUC)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Feature Importance via Simple Removal Analysis",
       x = "Feature", y = "AUC Drop (Delta)") +
  theme_minimal()


# ---- Feature Correlation Analysis ----

# Separate numerical and categorical features
numerical_features <- select_if(trainData, is.numeric)
categorical_features <- select_if(trainData, is.factor)

# Initialize a data frame to store correlations
correlation_output <- data.frame(Feature = character(), Correlation = numeric())

# Calculate correlation for numerical features
for (feature in colnames(numerical_features)) {
  if (feature != "Churn") {
    corr <- cor(numerical_features[[feature]], as.numeric(trainData$Churn), method = "pearson")
    correlation_output <- rbind(correlation_output, data.frame(Feature = feature, Correlation = corr))
  }
}

# Handle categorical features (convert to dummy variables)
for (feature in colnames(categorical_features)) {
  if (feature != "Churn") {
    # Convert to dummy variables
    dummies <- model.matrix(~ ., data = categorical_features[, feature, drop = FALSE])[, -1, drop = FALSE]  # Exclude intercept
    if (ncol(dummies) > 0) {  # Skip if no valid levels
      feature_correlations <- apply(dummies, 2, function(col) cor(col, as.numeric(trainData$Churn), method = "pearson"))
      mean_corr <- mean(feature_correlations, na.rm = TRUE)  # Average correlation for all levels
      correlation_output <- rbind(correlation_output, data.frame(Feature = feature, Correlation = mean_corr))
    } else {
      warning(paste("Feature", feature, "has no valid levels and was skipped."))
    }
  }
}

# Sort by absolute correlation
correlation_output <- correlation_output %>%
  mutate(AbsoluteCorrelation = abs(Correlation)) %>%
  arrange(desc(AbsoluteCorrelation))

# Print results
print(correlation_output)

# Plot the correlation
ggplot(correlation_output, aes(x = reorder(Feature, AbsoluteCorrelation), y = AbsoluteCorrelation)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Feature Correlation with Output (Churn)",
       x = "Feature", y = "Absolute Correlation") +
  theme_minimal()



# ---- Experimenting with Learning Rate and Maximum Iterations ----
learning_rates <- c(0.05, 0.1, 0.2, 0.3)  # Learning rates to test
max_iterations <- c(300, 500, 1000)  # Maximum iterations to test

# Initialize a data frame to store results
tuning_results <- data.frame(
  LearningRate = numeric(),
  MaxIterations = integer(),
  Accuracy = numeric(),
  Sensitivity = numeric(),
  Specificity = numeric(),
  Precision = numeric()
)

# Loop through each combination of learning rate and max iterations
for (lr in learning_rates) {
  for (iter in max_iterations) {
    cat("Testing with Learning Rate:", lr, "and Max Iterations:", iter, "\n")
    
    # Train the model with the specified parameters
    nn_model_tuned <- mlp(
      x = train_data_numeric, 
      y = train_labels,
      size = c(20, 10, 5),  # Best architecture
      maxit = iter,
      learnFuncParams = c(lr, 0.001),  # Adjust learning rate and L2 regularization
      hiddenActFunc = "Act_Logistic"  # Logistic activation function
    )
    
    # Predict on the test data
    nn_pred_tuned <- predict(nn_model_tuned, test_data_numeric)
    nn_pred_class_tuned <- as.factor(ifelse(nn_pred_tuned > 0.5, 1, 0))
    
    # Confusion matrix and metrics
    confusion_matrix_tuned <- table(test_labels, nn_pred_class_tuned)
    TP <- confusion_matrix_tuned[2, 2]
    FN <- confusion_matrix_tuned[2, 1]
    FP <- confusion_matrix_tuned[1, 2]
    TN <- confusion_matrix_tuned[1, 1]
    
    accuracy <- (TP + TN) / sum(confusion_matrix_tuned)
    sensitivity <- TP / (TP + FN)  # Recall
    specificity <- TN / (TN + FP)
    precision <- TP / (TP + FP)
    
    # Store results
    tuning_results <- rbind(
      tuning_results,
      data.frame(
        LearningRate = lr,
        MaxIterations = iter,
        Accuracy = round(accuracy, 4),
        Sensitivity = round(sensitivity, 4),
        Specificity = round(specificity, 4),
        Precision = round(precision, 4)
      )
    )
  }
}

# Print the results
print(tuning_results)

ggplot(tuning_results, aes(x = MaxIterations, y = Accuracy, color = as.factor(LearningRate))) +
  geom_point(size = 3) +
  geom_line() +
  labs(title = "Accuracy Across Learning Rates and Iterations",
       x = "Maximum Iterations",
       y = "Accuracy",
       color = "Learning Rate") +
  theme_minimal()

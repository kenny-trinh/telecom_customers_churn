# Load necessary libraries
library(caret)
library(nnet)
library(pROC)
library(ggplot2)
library(dplyr)
library(RSNNS)

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

# Train the Neural Network
nn_model <- nnet(
  Churn ~ ., data = trainData,
  size = 2, decay = 0.04, maxit = 300, trace = FALSE
)

# Predict on the test set
nn_prob <- predict(nn_model, testData, type = "raw")

# Evaluate the model
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

#--- Adjusting the Threshold ---

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


# ---Cost-Sensitive Learning with Weighted Resampling with caret ---

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


# --- Simple Removal Analysis ---

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

# --- Feature Correlation Analysis ---

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


# --- Neural Network with RSNNS ---

# Convert categorical variables to dummy variables
train_data_numeric <- model.matrix(~ . - 1, data = trainData[, -which(names(trainData) == "Churn")])  # Remove intercept
test_data_numeric <- model.matrix(~ . - 1, data = testData[, -which(names(testData) == "Churn")])    # Remove intercept

# Ensure the labels are numeric binary
train_labels <- as.numeric(trainData$Churn) - 1
test_labels <- as.numeric(testData$Churn) - 1


# Train the neural network with 2 hidden layers and adjusted neurons
nn_model_adjusted <- mlp(
  x = train_data_numeric, y = train_labels,
  size = c(10, 5),   # Two hidden layers: 10 neurons in the first, 5 in the second
  maxit = 300,
  learnFuncParams = c(0.1),  # Learning rate
  hiddenActFunc = "Act_Logistic"  # Default logistic activation function
)

# Predict on the test data
nn_pred_adjusted <- predict(nn_model_adjusted, test_data_numeric)
nn_pred_class <- as.factor(ifelse(nn_pred_adjusted > 0.5, 1, 0))

# Evaluate the model
conf_matrix_adjusted <- confusionMatrix(nn_pred_class, as.factor(test_labels))
print(conf_matrix_adjusted)

# Extract values from the confusion matrix
confusion_matrix <- table(test_labels, nn_pred_class)

TP <- confusion_matrix[2, 2]  # True Positives
FN <- confusion_matrix[2, 1]  # False Negatives
FP <- confusion_matrix[1, 2]  # False Positives
TN <- confusion_matrix[1, 1]  # True Negatives

# Calculate the metrics
accuracy <- (TP + TN) / (TP + TN + FP + FN)
sensitivity <- TP / (TP + FN)  # Recall
specificity <- TN / (TN + FP)
precision <- TP / (TP + FP)

# Print the metrics
cat("Accuracy:", round(accuracy, 4), "\n")
cat("Sensitivity (Recall):", round(sensitivity, 4), "\n")
cat("Specificity:", round(specificity, 4), "\n")
cat("Precision:", round(precision, 4), "\n")



# --- Neural Network with RSNNS: Trying Alternative Activation Functions ---

# Convert categorical variables to dummy variables
train_data_numeric <- model.matrix(~ . - 1, data = trainData[, -which(names(trainData) == "Churn")])  # Remove intercept
test_data_numeric <- model.matrix(~ . - 1, data = testData[, -which(names(trainData) == "Churn")])    # Remove intercept

# Ensure the labels are numeric binary
train_labels <- as.numeric(trainData$Churn) - 1
test_labels <- as.numeric(testData$Churn) - 1

# Experiment with Act_TanH activation function
nn_model_tanh <- mlp(
  x = train_data_numeric, 
  y = train_labels,
  size = c(10, 5),   # Two hidden layers: 10 neurons in the first, 5 in the second
  maxit = 300,
  learnFuncParams = c(0.1),  # Learning rate
  hiddenActFunc = "Act_TanH"  # Tanh activation function
)

# Predict and evaluate for Act_TanH
nn_pred_tanh <- predict(nn_model_tanh, test_data_numeric)
nn_pred_class_tanh <- as.factor(ifelse(nn_pred_tanh > 0.5, 1, 0))
conf_matrix_tanh <- confusionMatrix(nn_pred_class_tanh, as.factor(test_labels))
print("Confusion Matrix (Act_TanH):")
print(conf_matrix_tanh)

# Experiment with Act_ReLU activation function
nn_model_relu <- mlp(
  x = train_data_numeric, 
  y = train_labels,
  size = c(10, 5),   # Two hidden layers: 10 neurons in the first, 5 in the second
  maxit = 300,
  learnFuncParams = c(0.1),  # Learning rate
  hiddenActFunc = "Act_ReLU"  # ReLU activation function
)

# Predict and evaluate for Act_ReLU
nn_pred_relu <- predict(nn_model_relu, test_data_numeric)
nn_pred_class_relu <- as.factor(ifelse(nn_pred_relu > 0.5, 1, 0))
conf_matrix_relu <- confusionMatrix(nn_pred_class_relu, as.factor(test_labels))
print("Confusion Matrix (Act_ReLU):")
print(conf_matrix_relu)


# Extract values from the confusion matrix for Act_TanH
confusion_matrix_tanh <- table(test_labels, nn_pred_class_tanh)
TP_tanh <- confusion_matrix_tanh[2, 2]  # True Positives
FN_tanh <- confusion_matrix_tanh[2, 1]  # False Negatives
FP_tanh <- confusion_matrix_tanh[1, 2]  # False Positives
TN_tanh <- confusion_matrix_tanh[1, 1]  # True Negatives

# Calculate metrics for Act_TanH
accuracy_tanh <- (TP_tanh + TN_tanh) / sum(confusion_matrix_tanh)
sensitivity_tanh <- TP_tanh / (TP_tanh + FN_tanh)  # Recall
specificity_tanh <- TN_tanh / (TN_tanh + FP_tanh)
precision_tanh <- TP_tanh / (TP_tanh + FP_tanh)

# Print metrics for Act_TanH
cat("Metrics for Act_TanH:\n")
cat("Accuracy:", round(accuracy_tanh, 4), "\n")
cat("Sensitivity (Recall):", round(sensitivity_tanh, 4), "\n")
cat("Specificity:", round(specificity_tanh, 4), "\n")
cat("Precision:", round(precision_tanh, 4), "\n\n")

# Extract values from the confusion matrix for Act_ReLU
confusion_matrix_relu <- table(test_labels, nn_pred_class_relu)
TP_relu <- confusion_matrix_relu[2, 2]  # True Positives
FN_relu <- confusion_matrix_relu[2, 1]  # False Negatives
FP_relu <- confusion_matrix_relu[1, 2]  # False Positives
TN_relu <- confusion_matrix_relu[1, 1]  # True Negatives

# Calculate metrics for Act_ReLU
accuracy_relu <- (TP_relu + TN_relu) / sum(confusion_matrix_relu)
sensitivity_relu <- TP_relu / (TP_relu + FN_relu)  # Recall
specificity_relu <- TN_relu / (TN_relu + FP_relu)
precision_relu <- TP_relu / (TP_relu + FP_relu)

# Print metrics for Act_ReLU
cat("Metrics for Act_ReLU:\n")
cat("Accuracy:", round(accuracy_relu, 4), "\n")
cat("Sensitivity (Recall):", round(sensitivity_relu, 4), "\n")
cat("Specificity:", round(specificity_relu, 4), "\n")
cat("Precision:", round(precision_relu, 4), "\n")


# --- Neural Network with Regularization and Adjusted Architecture ---

# Convert categorical variables to dummy variables
train_data_numeric <- model.matrix(~ . - 1, data = trainData[, -which(names(trainData) == "Churn")])  # Remove intercept
test_data_numeric <- model.matrix(~ . - 1, data = testData[, -which(names(testData) == "Churn")])    # Remove intercept

# Ensure the labels are numeric binary
train_labels <- as.numeric(trainData$Churn) - 1
test_labels <- as.numeric(testData$Churn) - 1

# Train the neural network with 3 hidden layers, regularization, and adjusted neurons
nn_model_regularized <- mlp(
  x = train_data_numeric, 
  y = train_labels,
  size = c(15, 10, 5),       # Three hidden layers: 15, 10, and 5 neurons
  maxit = 300,
  learnFuncParams = c(0.1, 0.001),  # Learning rate and weight decay (L2 regularization)
  hiddenActFunc = "Act_Logistic"    # Logistic activation function
)

# Predict and evaluate
nn_pred_regularized <- predict(nn_model_regularized, test_data_numeric)
nn_pred_class_regularized <- as.factor(ifelse(nn_pred_regularized > 0.5, 1, 0))
conf_matrix_regularized <- confusionMatrix(nn_pred_class_regularized, as.factor(test_labels))

# Extract metrics
confusion_matrix_regularized <- table(test_labels, nn_pred_class_regularized)
TP <- confusion_matrix_regularized[2, 2]
FN <- confusion_matrix_regularized[2, 1]
FP <- confusion_matrix_regularized[1, 2]
TN <- confusion_matrix_regularized[1, 1]

accuracy <- (TP + TN) / sum(confusion_matrix_regularized)
sensitivity <- TP / (TP + FN)  # Recall
specificity <- TN / (TN + FP)
precision <- TP / (TP + FP)

# Print results
cat("Metrics for Regularized Neural Network with Adjusted Architecture:\n")
cat("Accuracy:", round(accuracy, 4), "\n")
cat("Sensitivity (Recall):", round(sensitivity, 4), "\n")
cat("Specificity:", round(specificity, 4), "\n")
cat("Precision:", round(precision, 4), "\n")


# --- Neural Network Experimentation with Different Architectures ---

# Define a list of architectures to test
architectures <- list(
  c(20, 10, 5),
  c(10, 5),
  c(25, 15, 10)
)

# Initialize a data frame to store results
architecture_results <- data.frame(
  Architecture = character(),
  Accuracy = numeric(),
  Sensitivity = numeric(),
  Specificity = numeric(),
  Precision = numeric()
)

# Loop through each architecture
for (arch in architectures) {
  cat("Testing architecture:", paste(arch, collapse = "-"), "\n")
  
  # Train the model with the given architecture
  nn_model <- mlp(
    x = train_data_numeric, 
    y = train_labels,
    size = arch,  # Set architecture
    maxit = 300,
    learnFuncParams = c(0.1, 0.001),  # Learning rate and L2 regularization
    hiddenActFunc = "Act_Logistic"  # Activation function
  )
  
  # Predict on the test data
  nn_pred <- predict(nn_model, test_data_numeric)
  nn_pred_class <- as.factor(ifelse(nn_pred > 0.5, 1, 0))
  
  # Confusion matrix and metrics
  confusion_matrix <- table(test_labels, nn_pred_class)
  TP <- confusion_matrix[2, 2]
  FN <- confusion_matrix[2, 1]
  FP <- confusion_matrix[1, 2]
  TN <- confusion_matrix[1, 1]
  
  accuracy <- (TP + TN) / sum(confusion_matrix)
  sensitivity <- TP / (TP + FN)  # Recall
  specificity <- TN / (TN + FP)
  precision <- TP / (TP + FP)
  
  # Store results
  architecture_results <- rbind(
    architecture_results,
    data.frame(
      Architecture = paste(arch, collapse = "-"),
      Accuracy = round(accuracy, 4),
      Sensitivity = round(sensitivity, 4),
      Specificity = round(specificity, 4),
      Precision = round(precision, 4)
    )
  )
}

# Print the results
print(architecture_results)

# Visualize the results
ggplot(architecture_results, aes(x = Architecture)) +
  geom_bar(aes(y = Accuracy), stat = "identity", fill = "steelblue") +
  geom_point(aes(y = Sensitivity), color = "red", size = 3) +
  geom_point(aes(y = Specificity), color = "green", size = 3) +
  geom_point(aes(y = Precision), color = "purple", size = 3) +
  labs(title = "Performance Metrics by Architecture", x = "Architecture", y = "Metrics") +
  theme_minimal()


# --- Experimenting with Learning Rate and Maximum Iterations ---
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


# --- Adding Dropout Regularization ---
nn_model_dropout <- mlp(
  x = train_data_numeric, 
  y = train_labels,
  size = c(20, 10, 5),  # Best architecture identified
  maxit = 300,
  learnFuncParams = c(0.05, 0.001),  # Best learning rate and regularization
  hiddenActFunc = "Act_Logistic",  # Logistic activation
  dropout = c(0.2, 0.2, 0.2)  # Dropout rates for each hidden layer
)

# Predict and evaluate
nn_pred_dropout <- predict(nn_model_dropout, test_data_numeric)
nn_pred_class_dropout <- as.factor(ifelse(nn_pred_dropout > 0.5, 1, 0))
conf_matrix_dropout <- confusionMatrix(nn_pred_class_dropout, as.factor(test_labels))

# Extract metrics for comparison
confusion_matrix_dropout <- table(test_labels, nn_pred_class_dropout)
TP <- confusion_matrix_dropout[2, 2]
FN <- confusion_matrix_dropout[2, 1]
FP <- confusion_matrix_dropout[1, 2]
TN <- confusion_matrix_dropout[1, 1]

accuracy_dropout <- (TP + TN) / sum(confusion_matrix_dropout)
sensitivity_dropout <- TP / (TP + FN)
specificity_dropout <- TN / (TN + FP)
precision_dropout <- TP / (TP + FP)

# Print results
cat("Metrics for Neural Network with Dropout Regularization:\n")
cat("Accuracy:", round(accuracy_dropout, 4), "\n")
cat("Sensitivity (Recall):", round(sensitivity_dropout, 4), "\n")
cat("Specificity:", round(specificity_dropout, 4), "\n")
cat("Precision:", round(precision_dropout, 4), "\n")


# Adjust the decision threshold
threshold <- 0.4  # Example threshold
nn_pred_class_threshold <- as.factor(ifelse(nn_pred_dropout > threshold, 1, 0))

# Evaluate with adjusted threshold
conf_matrix_threshold <- confusionMatrix(nn_pred_class_threshold, as.factor(test_labels))
confusion_matrix_threshold <- table(test_labels, nn_pred_class_threshold)
TP_thresh <- confusion_matrix_threshold[2, 2]
FN_thresh <- confusion_matrix_threshold[2, 1]
FP_thresh <- confusion_matrix_threshold[1, 2]
TN_thresh <- confusion_matrix_threshold[1, 1]

accuracy_thresh <- (TP_thresh + TN_thresh) / sum(confusion_matrix_threshold)
sensitivity_thresh <- TP_thresh / (TP_thresh + FN_thresh)
specificity_thresh <- TN_thresh / (TN_thresh + FP_thresh)
precision_thresh <- TP_thresh / (TP_thresh + FP_thresh)

# Print adjusted metrics
cat("Metrics with Adjusted Threshold (", threshold, "):\n", sep = "")
cat("Accuracy:", round(accuracy_thresh, 4), "\n")
cat("Sensitivity (Recall):", round(sensitivity_thresh, 4), "\n")
cat("Specificity:", round(specificity_thresh, 4), "\n")
cat("Precision:", round(precision_thresh, 4), "\n")


# --- Threshold Analysis ---
# Define thresholds to test
thresholds <- seq(0.1, 0.9, by = 0.05)

# Initialize a data frame to store results
threshold_results <- data.frame(
  Threshold = numeric(),
  Accuracy = numeric(),
  Sensitivity = numeric(),
  Specificity = numeric(),
  Precision = numeric()
)

# Loop through thresholds
for (thresh in thresholds) {
  # Apply the threshold to predictions
  nn_pred_class_thresh <- as.factor(ifelse(nn_pred_dropout > thresh, 1, 0))
  
  # Confusion matrix and metrics
  confusion_matrix_thresh <- table(test_labels, nn_pred_class_thresh)
  TP_thresh <- confusion_matrix_thresh[2, 2]
  FN_thresh <- confusion_matrix_thresh[2, 1]
  FP_thresh <- confusion_matrix_thresh[1, 2]
  TN_thresh <- confusion_matrix_thresh[1, 1]
  
  # Calculate metrics
  accuracy_thresh <- (TP_thresh + TN_thresh) / sum(confusion_matrix_thresh)
  sensitivity_thresh <- TP_thresh / (TP_thresh + FN_thresh)
  specificity_thresh <- TN_thresh / (TN_thresh + FP_thresh)
  precision_thresh <- TP_thresh / (TP_thresh + FP_thresh)
  
  # Add results to the data frame
  threshold_results <- rbind(
    threshold_results,
    data.frame(
      Threshold = thresh,
      Accuracy = round(accuracy_thresh, 4),
      Sensitivity = round(sensitivity_thresh, 4),
      Specificity = round(specificity_thresh, 4),
      Precision = round(precision_thresh, 4)
    )
  )
}

# Print the results
print(threshold_results)

# Visualize the results
ggplot(threshold_results, aes(x = Threshold)) +
  geom_line(aes(y = Accuracy, color = "Accuracy")) +
  geom_line(aes(y = Sensitivity, color = "Sensitivity")) +
  geom_line(aes(y = Specificity, color = "Specificity")) +
  geom_line(aes(y = Precision, color = "Precision")) +
  labs(title = "Performance Metrics Across Thresholds",
       x = "Threshold",
       y = "Metrics",
       color = "Metrics") +
  theme_minimal()

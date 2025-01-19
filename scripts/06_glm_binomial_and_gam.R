# Load required libraries
library(ggplot2)
library(dplyr)
library(mgcv)
library(pROC)
library(caret)

# ---- Preparation ----

# Load the data
data <- read.csv("telecom_customers_churn_cleaned.csv")
data$Churn <- ifelse(data$Churn == "Yes", 1, 0)
# Ensure variables are factors
data$Contract <- as.factor(data$Contract)
data$PaymentMethod <- as.factor(data$PaymentMethod)
data$Churn <- as.factor(data$Churn)
data$TotalCharges <- log(data$TotalCharges)
data$MonthlyCharges <- log(data$MonthlyCharges)
data$InternetService <- as.factor(data$InternetService)

# ---- Exploratory Data Analysis ----
# Plot: Tenure vs. TotalCharges by Churn
ggplot(data, aes(x = tenure, y = TotalCharges, color = Churn)) +
  geom_point(alpha = 0.6) +
  labs(
    title = "Relationship Between Tenure and Total Charges by Churn",
    x = "Tenure (Months)",
    y = "Total Charges"
  ) +
  scale_color_manual(values = c("blue", "red"), labels = c("No Churn", "Churn")) +
  theme_minimal()

# Plot: Tenure by Contract Type
ggplot(data, aes(x = Contract, y = tenure, fill = Contract)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Tenure by Contract Type",
    x = "Contract Type",
    y = "Tenure (Months)"
  ) +
  scale_fill_manual(values = c("blue", "orange", "purple")) +
  theme_minimal()

# Plot: Churn by Payment Method

ggplot(data, aes(x = PaymentMethod, fill = Churn)) +
  geom_bar(position = "fill", alpha = 0.8) +
  labs(
    title = "Proportion of Churn by Payment Method",
    x = "Payment Method",
    y = "Proportion"
  ) +
  scale_fill_manual(values = c("blue", "red"), labels = c("No Churn", "Churn")) +
  theme_minimal() +
  coord_flip()

# Plot: Churn Proportion by Internet Service
# Create a dataset for proportions and counts
proportion_df <- data %>%
  group_by(InternetService, Churn) %>%
  summarise(Count = n()) %>%
  mutate(Total = sum(Count),
         Proportion = Count / Total)

# Plot the bar chart with counts displayed
ggplot(proportion_df, aes(x = InternetService, y = Proportion, fill = as.factor(Churn))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Count), position = position_dodge(width = 0.9), vjust = -0.2, size = 3) +
  labs(
    title = "Proportion of Churn by Internet Service with Counts",
    x = "Internet Service Type",
    y = "Proportion",
    fill = "Churn"
  ) +
  scale_fill_manual(values = c("blue", "red"), labels = c("No", "Yes")) +
  theme_minimal()

# ---- Fit the GLM model ----
# Set 'No service' as the reference level for InternetService
data$InternetService <- relevel(data$InternetService, ref = "No")
# Confirm reference level
cat("Reference level for 'Contract':", levels(data$Contract)[1], "\n")
cat("Reference level for 'InternetService':", levels(data$InternetService)[1], "\n")
# Fit the GLM with interaction terms
glm_model_interaction <- glm(Churn ~ SeniorCitizen + Contract + PaymentMethod + 
                               tenure + TotalCharges + 
                               InternetService + OnlineSecurity + TechSupport + 
                               PaperlessBilling + MultipleLines, 
                             family = binomial, data = data)
# Check the summary
summary(glm_model_interaction)



# Check for model overfitting by evaluating on training and test dataset

# Split data into training and test sets
set.seed(123)
train_indices <- sample(1:nrow(data), 0.7 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Predict on training and test data
train_preds <- predict(glm_model_interaction, train_data, type = "response")
test_preds <- predict(glm_model_interaction, test_data, type = "response")

# Calculate performance metrics
library(pROC)
train_auc <- roc(train_data$Churn, train_preds)$auc
test_auc <- roc(test_data$Churn, test_preds)$auc

cat("Train AUC:", train_auc, "\n")
cat("Test AUC:", test_auc, "\n")


# ---- Cross-Validation ----
# Set up cross-validation
control <- trainControl(method = "cv", number = 10) # 10-fold cross-validation

# Train the model
cv_model <- train(Churn ~ SeniorCitizen + Contract + PaymentMethod + 
                    tenure + MonthlyCharges + TotalCharges + 
                    InternetService + InternetService:MonthlyCharges, 
                  data = data, method = "glm", family = "binomial", trControl = control)

# Check cross-validation results
print(cv_model)


# --- GAM with non-linear patterns ---
data$TotalCharges_centered <- data$TotalCharges- mean(data$TotalCharges)
cat("Reference level for 'Contract':", levels(data$Contract)[1], "\n")
cat("Reference level for 'InternetService':", levels(data$InternetService)[1], "\n")
gam_smoothing_spline <- gam(Churn ~ SeniorCitizen + Contract + PaymentMethod + 
                              s(tenure) + s(TotalCharges_centered) + 
                              InternetService + OnlineSecurity + TechSupport + 
                              PaperlessBilling + MultipleLines, 
                            family = binomial, data = data, select=TRUE)
summary(gam_smoothing_spline)

# ---- Compare models ----

# Compare ROC curves
# Predictions for glm_model
pred1 <- predict(glm_model_interaction, type = "response")
roc1 <- roc(data$Churn, pred1)

# Predictions for gam_model
pred2 <- predict(gam_smoothing_spline, type = "response")
roc2 <- roc(data$Churn, pred2)

# Plot ROC curves
plot(roc1, col = "blue", lwd = 2, main = "ROC Curve Comparison")
lines(roc2, col = "red", lwd = 2)
legend("bottomright", legend = c("glm_model_interaction", "gam_smoothing_spline"),
       col = c("blue", "red"), lwd = 2)

auc1 <- auc(roc1)
auc2 <- auc(roc2)
cat("AUC of Model glm_model_interaction:", auc1, "\n")
cat("AUC of Model gam_smoothing_spline:", auc2, "\n")

# Compare the models using the Anova test
anova(glm_model_interaction, gam_smoothing_spline, test = "Chisq")

# Compare the models using AIC
AIC(glm_model_interaction, gam_smoothing_spline)
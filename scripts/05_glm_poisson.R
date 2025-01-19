# Load required libraries
library(ggplot2)

# eda
ggplot(d.cleaned_telecom_customer_churn, aes(x = tenure)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Tenure",
       x = "Tenure (Months)",
       y = "Frequency") +
  theme_minimal()

# poisson model. 
# Fit Poisson regression
glm_poisson <- glm(formula = tenure ~ InternetService + Contract + PaymentMethod + 
      StreamingTV + StreamingMovies + SeniorCitizen + Partner + 
      log(MonthlyCharges), 
    family = poisson(link = "log"),
    data = d.cleaned_telecom_customer_churn)

summary(glm_poisson)

# Predict tenure using the Poisson model
d.cleaned_telecom_customer_churn$predicted_tenure <- predict(glm_poisson, type = "response")

ggplot(d.cleaned_telecom_customer_churn, aes(x = tenure, y = predicted_tenure)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red") +
  theme_minimal() +
  labs(title = "Observed vs. Predicted Tenure",
       x = "Observed Tenure (Months)",
       y = "Predicted Tenure (Poisson Model)")

# Boxplot for Contract 
ggplot(d.cleaned_telecom_customer_churn, aes(x = Contract, y = tenure, fill = Contract)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Tenure vs Contract Type",
       x = "Contract Type",
       y = "Tenure (Months)")


# Fit Poisson regression with interactions
ggplot(d.cleaned_telecom_customer_churn, aes(x = log(MonthlyCharges), y = tenure)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red") +
  theme_minimal() +
  labs(title = "Log Monthly Charges vs Tenure",
       x = "Log(Monthly Charges)",
       y = "Tenure (Months)")


# Fit Quasi-Poisson Model
glm_quasi <- glm(tenure ~ InternetService + Contract + PaymentMethod + 
                   StreamingTV + StreamingMovies + SeniorCitizen + Partner + 
                   log(MonthlyCharges), 
                 family = quasipoisson(link = "log"), 
                 data = d.cleaned_telecom_customer_churn)

# Display model summary
summary(glm_quasi)



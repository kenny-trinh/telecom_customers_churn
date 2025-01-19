# Load required libraries
library(ggplot2)
library(gridExtra)

lm_model <- lm(log(MonthlyCharges) ~ tenure + InternetService + Contract + 
                 StreamingTV + StreamingMovies + OnlineSecurity + 
                 OnlineBackup + DeviceProtection + TechSupport + PhoneService, 
               data = d.cleaned_telecom_customer_churn)

summary(lm_model)


# Plot 1: Log Monthly Charges vs Tenure by Internet Service
plot1 <- ggplot(d.cleaned_telecom_customer_churn, aes(x = tenure, y = log(MonthlyCharges), color = InternetService)) +
  geom_point(alpha = 0.4) + 
  geom_smooth(method = "loess", se = TRUE) + 
  theme_minimal() +
  labs(title = "Log Monthly Charges vs Tenure by Internet Service",
       x = "Tenure (months)", y = "Log Monthly Charges")

# Plot 2: Log Monthly Charges vs Tenure by Contract Type
plot2 <- ggplot(d.cleaned_telecom_customer_churn, aes(x = tenure, y = log(MonthlyCharges), color = Contract)) +
  geom_point(alpha = 0.4) + 
  geom_smooth(method = "loess", se = TRUE) + 
  theme_minimal() +
  labs(title = "Log Monthly Charges vs Tenure by Contract Type",
       x = "Tenure (months)", y = "Log Monthly Charges")

# Arrange plots in grid view
grid.arrange(plot1, plot2, ncol = 2)



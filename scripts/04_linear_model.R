# Step 1: Initial linear model with all relevant predictors
lm_initial <- lm(log(MonthlyCharges) ~ 
                   tenure + 
                   InternetService + 
                   Contract + 
                   StreamingTV + 
                   StreamingMovies +
                   OnlineSecurity +
                   OnlineBackup +
                   DeviceProtection +
                   TechSupport +
                   PhoneService,
                 data = d.cleaned_telecom_customer_churn)

# Step 2: Check model diagnostics
par(mfrow = c(2,2))
plot(lm_initial)

# Step 3: Get model summary
summary(lm_initial)


# Effect of Internet Service with smoother
ggplot(d.cleaned_telecom_customer_churn, 
       aes(x = tenure, y = log(MonthlyCharges), color = InternetService)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", se = TRUE) +
  theme_minimal() +
  labs(title = "Log Monthly Charges vs Tenure by Internet Service",
       x = "Tenure (months)",
       y = "Log Monthly Charges")


# Effect of Contract with smoother
ggplot(d.cleaned_telecom_customer_churn, 
       aes(x = tenure, y = log(MonthlyCharges), color = Contract)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", se = TRUE) +
  theme_minimal() +
  labs(title = "Log Monthly Charges vs Tenure by Contract",
       x = "Tenure (months)",
       y = "Log Monthly Charges")

# Enhanced model with interactions
lm_enhanced <- lm(log(MonthlyCharges) ~ 
                    tenure * InternetService +    # Add interaction with tenure
                    tenure * Contract +           # Add interaction with contract
                    StreamingTV + StreamingMovies + 
                    OnlineSecurity + OnlineBackup + 
                    DeviceProtection + TechSupport + 
                    PhoneService,
                  data = d.cleaned_telecom_customer_churn)

summary(lm_enhanced)

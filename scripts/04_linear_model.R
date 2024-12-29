
# First simple model
lm.telecom.1 <- lm(MonthlyCharges ~ tenure, data = d.cleaned_telecom_customer_churn)

# Look at summary
summary(lm.telecom.1)

# Visualize the relationship
plot(MonthlyCharges ~ tenure, data = d.cleaned_telecom_customer_churn,
     main = "Monthly Charges vs Tenure",
     xlab = "Tenure (months)",
     ylab = "Monthly Charges ($)")
abline(lm.telecom.1, col = "red")


#linear model with other predictors. 
lm.telecom.2 <- lm(MonthlyCharges ~ tenure + Contract + InternetService, data = d.cleaned_telecom_customer_churn)
summary(lm.telecom.2)


#linear model with interactions
lm.telecom.3 <- lm(MonthlyCharges ~ tenure * Contract + InternetService, data = d.cleaned_telecom_customer_churn)
summary(lm.telecom.3)

# Perform ANOVA test between models
anova(lm.telecom.1, lm.telecom.2, lm.telecom.3)


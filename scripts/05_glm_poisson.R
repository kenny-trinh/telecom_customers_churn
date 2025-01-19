# Load required libraries
library(ggplot2)

ggplot(d.cleaned_telecom_customer_churn, aes(x = tenure)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Tenure",
       x = "Tenure (Months)",
       y = "Frequency") +
  theme_minimal()

# ---- Apply poisson model ----

# Fit Poisson regression
glm_tenure <- glm(tenure ~ InternetService + Contract + PaymentMethod +
                    StreamingTV + StreamingMovies + SeniorCitizen +
                    Partner + MonthlyCharges + TotalCharges,
                  family = poisson(link = "log"), data = d.cleaned_telecom_customer_churn)

summary(glm_tenure)

# ---- Create Boxplots ----

# Boxplot for Contract
ggplot(d.cleaned_telecom_customer_churn, aes(x = Contract, y = tenure)) +
  geom_boxplot(fill = "lightblue", outlier.color = "red") +
  labs(title = "Tenure vs Contract Type", x = "Contract Type", y = "Tenure (Months)") +
  theme_minimal()

# Boxplot for Internet Service
ggplot(d.cleaned_telecom_customer_churn, aes(x = InternetService, y = tenure)) +
  geom_boxplot(fill = "lightgreen", outlier.color = "red") +
  labs(title = "Tenure vs Internet Service", x = "Internet Service Type", y = "Tenure (Months)") +
  theme_minimal()

# ---- Apply Quasi-Poisson model ----

glm_quasi <- glm(tenure ~ InternetService * Contract + PaymentMethod * Contract +
                   StreamingTV + StreamingMovies + SeniorCitizen + Partner +
                   MonthlyCharges + TotalCharges,
                 family = quasipoisson(link = "log"),
                 data = d.cleaned_telecom_customer_churn)
summary(glm_quasi)

# Fit Poisson regression with interactions
glm_tenure_interaction <- glm(
  tenure ~ InternetService * Contract +
    PaymentMethod * Contract +
    StreamingTV + StreamingMovies +
    SeniorCitizen + Partner +
    MonthlyCharges + TotalCharges,
  family = poisson(link = "log"),
  data = d.cleaned_telecom_customer_churn
)

# Summary of the updated model
summary(glm_tenure_interaction)

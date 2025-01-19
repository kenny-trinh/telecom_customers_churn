# Load required libraries if not already loaded
library(ggplot2)
library(gridExtra)
library(dplyr)
library(corrplot)

# ---- Apply log transformation ----
d.cleaned_telecom_customer_churn <- d.cleaned_telecom_customer_churn %>%
  mutate(log_MonthlyCharges = log(MonthlyCharges + 1),
         log_TotalCharges = log(TotalCharges + 1))

# ---- Create visualizations ----

# Histograms before and after log transformation
p1 <- ggplot(d.cleaned_telecom_customer_churn, aes(x = MonthlyCharges)) + 
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
  ggtitle("Before Log Transformation (MonthlyCharges)")

p2 <- ggplot(d.cleaned_telecom_customer_churn, aes(x = log_MonthlyCharges)) + 
  geom_histogram(bins = 30, fill = "darkred", alpha = 0.7) +
  ggtitle("After Log Transformation (MonthlyCharges)")

p3 <- ggplot(d.cleaned_telecom_customer_churn, aes(x = TotalCharges)) + 
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  ggtitle("Before Log Transformation (TotalCharges)")

p4 <- ggplot(d.cleaned_telecom_customer_churn, aes(x = log_TotalCharges)) + 
  geom_histogram(bins = 30, fill = "red", alpha = 0.7) +
  ggtitle("After Log Transformation (TotalCharges)")

grid.arrange(p1, p2, p3, p4, ncol = 2)


# ---- Churn distribution ----
churn_dist <- ggplot(d.cleaned_telecom_customer_churn, aes(x = Churn, fill = Churn)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), position=position_dodge(width=0.9), vjust=-0.5) +
  theme_minimal() +
  labs(title = "Distribution of Customer Churn", x = "Churn Status", y = "Number of Customers")

# Display
print(churn_dist)

# ---- Churn Rate by Contract Type ---- 
contract_churn <- ggplot(d.cleaned_telecom_customer_churn, aes(x = Contract, fill = Churn)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Churn Rate by Contract Type", x = "Contract Type", y = "Proportion")

# Display
print(contract_churn)


# ---- Monthly Charages vs Tenure ----
charges_tenure <- ggplot(d.cleaned_telecom_customer_churn, aes(x = tenure, y = MonthlyCharges, color = Churn)) +
  geom_smooth(method = "loess", se = TRUE) + 
  theme_minimal() +
  labs(title = "Monthly Charges vs Tenure (Smoothed)", x = "Tenure (months)", y = "Monthly Charges ($)")

# Display
print(charges_tenure)

# ---- Arrange all plots in a grid ----

# Bar plots on the left, Line plot on the right
grid.arrange(churn_dist, contract_churn, charges_tenure, 
             layout_matrix = rbind(c(1,2), 
                                   c(3,3)))

# ---- Correlation matrix ----

# Selecting numeric variables
numeric_vars <- d.cleaned_telecom_customer_churn %>%
  select(tenure, log_MonthlyCharges, log_TotalCharges, services_count) %>%
  na.omit()

# Create Correlation matrix
corr_matrix <- cor(numeric_vars)

# Heatmap
corrplot(corr_matrix, method = "color", type = "upper")



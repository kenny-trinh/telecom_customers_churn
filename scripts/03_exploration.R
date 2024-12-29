# Load required libraries if not already loaded
library(ggplot2)
library(gridExtra)
library(dplyr)

# 1. Target Variable Distribution (Churn)
churn_dist <- ggplot(d.cleaned_telecom_customer_churn, aes(x = Churn, fill = Churn)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), position=position_dodge(width=0.9), vjust=-0.5) +
  theme_minimal() +
  labs(title = "Distribution of Customer Churn",
       x = "Churn Status",
       y = "Number of Customers")

# 2. Count Variables Analysis (for Poisson GLM)
# Services Count by Churn
services_churn <- ggplot(d.cleaned_telecom_customer_churn, 
                         aes(x = factor(services_count), fill = Churn)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(title = "Number of Services by Churn Status",
       x = "Number of Services",
       y = "Count")

# Tenure by Churn
tenure_churn <- ggplot(d.cleaned_telecom_customer_churn, 
                       aes(x = Churn, y = tenure)) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal() +
  labs(title = "Customer Tenure by Churn Status",
       x = "Churn Status",
       y = "Tenure (months)")

# 3. Key categorical variable (Contract Type)
contract_churn <- ggplot(d.cleaned_telecom_customer_churn, 
                         aes(x = Contract, fill = Churn)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Churn Rate by Contract Type",
       x = "Contract Type",
       y = "Proportion")

# 4. Monthly Charges vs Tenure with Churn
charges_tenure <- ggplot(d.cleaned_telecom_customer_churn, 
                         aes(x = tenure, y = MonthlyCharges, color = Churn)) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Monthly Charges vs Tenure by Churn Status",
       x = "Tenure (months)",
       y = "Monthly Charges ($)")

# Arrange all plots in a grid
grid.arrange(churn_dist, services_churn,
             tenure_churn, contract_churn,
             charges_tenure, 
             layout_matrix = rbind(c(1,2),
                                   c(3,4),
                                   c(5,5)))

# Basic summary statistics for count variables
summary_stats <- d.cleaned_telecom_customer_churn %>%
  group_by(Churn) %>%
  summarise(
    avg_services = mean(services_count),
    avg_tenure = mean(tenure),
    avg_monthly = mean(MonthlyCharges)
  )

print(summary_stats)
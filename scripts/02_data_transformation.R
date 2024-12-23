# Show the cleaned data as a table
d.cleaned_telecom_customer_churn <- read_csv(
  file.path(cleaned_data_dir, "telecom_customers_churn_cleaned.csv")
)

# Check the structure of the cleaned data
str(d.cleaned_telecom_customer_churn)

# Show the first few rows of the cleaned data
head(d.cleaned_telecom_customer_churn)

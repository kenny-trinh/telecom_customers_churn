# Show the cleaned data as a table
d.cleaned_telecom_customer_churn <- read_csv(
  file.path(cleaned_data_dir, "telecom_customers_churn_cleaned.csv")
)

# Check the structure of the cleaned data
str(d.cleaned_telecom_customer_churn)

# Show the first few rows of the cleaned data
head(d.cleaned_telecom_customer_churn)

# Summary Statistics
summary(d.cleaned_telecom_customer_churn)


#classify variables types
variable_types <- sapply(d.cleaned_telecom_customer_churn, function(x) {
  if (is.numeric(x) && length(unique(x)) > 20) {
    "Continuous"
  } else if (length(unique(x)) == 2) {
    "Binomial"
  } else if (!is.numeric(x) || length(unique(x)) <= 20) {
    "Categorical"
  } else if (is.numeric(x) && all(x == as.integer(x))) {
    "Count"
  } else {
    "Other"
  }
})

print(variable_types)

View(variable_types)

#creating count datatypes:
# 1. Create service count variable (including all services)
d.cleaned_telecom_customer_churn <- d.cleaned_telecom_customer_churn %>%
  mutate(
    # Count of all services
    services_count = rowSums(
      select(., PhoneService, MultipleLines, InternetService,
             OnlineSecurity, OnlineBackup, DeviceProtection,
             TechSupport, StreamingTV, StreamingMovies) != "No")
  )

# Verify our count variables
summary(d.cleaned_telecom_customer_churn$services_count)
summary(d.cleaned_telecom_customer_churn$tenure) # tenure is already a count

# Check distribution of both count variables
par(mfrow=c(1,2))
hist(d.cleaned_telecom_customer_churn$services_count, 
     main="Distribution of Services Count",
     xlab="Number of Services")
hist(d.cleaned_telecom_customer_churn$tenure, 
     main="Distribution of Tenure (months)",
     xlab="Tenure")
## ----ImportData--------------------------------------------------------------
# Read the data from the CSV file
d.telecom_customer_churn <- read_delim(
  file.path(raw_data_dir, "telecom_customers_churn.csv"),
  delim = ",",
  na = c("", "NA")
)

## ----DataPreview-------------------------------------------------------------
# Preview the data
glimpse(d.telecom_customer_churn)

## ----CheckNAValues-----------------------------------------------------------
# Check for missing values
v.na_count <- colSums(is.na(d.telecom_customer_churn))

# Print the number of missing values
print(v.na_count)

## ----ReplaceNAWithMeanOfTotalCharges-----------------------------------------
# Replace missing values in the TotalCharges column with the mean
d.telecom_customer_churn$TotalCharges[
  is.na(d.telecom_customer_churn$TotalCharges)
  ] <- mean(d.telecom_customer_churn$TotalCharges, na.rm = TRUE)

## ----CheckNAValuesAfterCleaning----------------------------------------------
# Check if there are any missing values left
v.na_count <- colSums(is.na(d.telecom_customer_churn))

# Print the number of missing values
print(v.na_count)

## ----SaveCleanedData---------------------------------------------------------
# Save the cleaned data to a CSV file
write_csv(d.telecom_customer_churn, file.path(
  cleaned_data_dir, "telecom_customers_churn_cleaned.csv"
  )
)

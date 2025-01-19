# Telecom Customer Churn Prediction

This repository contains the analysis and code for predicting customer churn in a telecommunications company. The project aims to identify factors contributing to churn and provide actionable insights to minimize customer loss using various machine learning and statistical models.


## Folder Structure

The project is organized into the following directories:

### `/data`
Contains the datasets used in the analysis.

- **`/raw`**: Includes the original dataset provided for the project.
  - `telecom_customers_churn.csv`: The raw dataset, directly sourced without any preprocessing.
- **`/cleaned`**: Contains datasets that have undergone preprocessing.
  - `telecom_customers_churn_cleaned.csv`: The cleaned dataset, used for modeling and analysis.
- **`/output`**: Stores outputs generated during the project.
  - `segmented_test_data.csv`: Segmented test data for model validation.
  - `svm_tuned_model.rds`: Tuned Support Vector Machine (SVM) model saved as an RDS file.
  - `svm_tuned_parameters.rds`: Parameters used for tuning the SVM model.

> **Note**: The raw dataset was sourced from [source website or company], while the cleaned dataset includes preprocessing steps described in the `01_data_cleaning.R` script.

### `/scripts`
Contains all R scripts used in the data analysis pipeline. The scripts are named and organized to reflect the workflow:

1. **`00_setup.R`**: Sets up the R environment by loading necessary libraries and configurations.
2. **`01_data_cleaning.R`**: Handles missing values, data type corrections, and other cleaning steps.
3. **`02_data_transformation.R`**: Includes feature engineering and transformations required for model input.
4. **`03_exploration.R`**: Performs exploratory data analysis (EDA) to uncover trends and insights in the data.
5. **`04_linear_model.R`**: Implements linear regression models for churn prediction.
6. **`05_glm_poisson.R`**: Fits a Poisson regression model for exploratory purposes.
7. **`06_glm_binomial_and_gam.R`**: Fits binomial and Generalized Additive Models (GAM) to identify nonlinear effects.
8. **`07_neural_network.R`**: Trains and evaluates Neural Network models for churn prediction.
9. **`08_support_vector_machine.R`**: Implements SVMs for classification and parameter tuning.

### Root Files

- **`.gitignore`**: Specifies files and directories to be ignored by Git.
- **`LICENSE`**: Licensing terms for this project.
- **`README.md`**: Provides an overview of the project, structure, and instructions for reproduction.
- **`Report_Telecom_Customer_Churn.Rmd`**: The R Markdown file containing the final report and analysis.
- **`Report_Telecom_Customer_Churn.pdf`**: The final report compiled from the R Markdown file.


## Project Context

### Data Source
The raw dataset was obtained from https://www.kaggle.com/datasets/tarekmuhammed/telecom-customers/data, representing anonymized customer data for a telecommunications company.

### Objective
The primary goal of the project is to develop predictive models to identify customers likely to churn. Insights derived from the models can guide targeted retention strategies.


## Reproducibility Instructions

1. Clone the repository:
   ```bash
   git clone https://github.com/kenny-trinh/telecom_customers_churn.git
   cd telecom_customers_churn
   ```

2. Install the required R packages by running:
   ```R
   source("scripts/00_setup.R")
   ```

3. Run the pipeline step-by-step:
   - Preprocess the data:
     ```R
     source("scripts/01_data_cleaning.R")
     source("scripts/02_data_transformation.R")
     ```
   - Explore the data:
     ```R
     source("scripts/03_exploration.R")
     ```
   - Fit & Train models:
     ```R
     source("scripts/04_linear_model.R")
     source("scripts/05_glm_poisson.R")
     source("scripts/06_glm_binomial_and_gam.R")
     source("scripts/07_neural_network.R")
     source("scripts/08_support_vector_machine.R")
     ```

4. View the final report:
   - Open `Report_Telecom_Customer_Churn.pdf` for detailed results and insights.


## Contribution Information

**Team Members**:
- [Sabin Pun] - Lead on EDA, Linear models and GLM Poisson models.
- [Sarp Koc] - Lead on GLM-Binomial and GAM models.
- [Kenny Trinh] - Lead on Data cleaning and transformation, Neural Network and SVM sections.

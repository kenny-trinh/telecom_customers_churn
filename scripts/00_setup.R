## ----PackageSetup-----------------------------------------------------------------------------------

# List of required packages
required_packages <- c(
  "readr",
  'dplyr',
  'tidyverse',
  'caret',
  'nnet',
  'pROC',
  'ggplot2',
  'corrplot',
  'RSNNS',
  'e1071',
  'mgcv',
  'gridExtra',
  "tinytex",
  "stringr",
  "tidyr",
  "xfun"
)

# Function to check and install missing packages
install_if_missing <- function(packages) {
  missing_packages <- packages[!packages %in% installed.packages()[, "Package"]]
  if (length(missing_packages) > 0) {
    install.packages(missing_packages)
  }
}

# Install any missing packages
install_if_missing(required_packages)

# Load all packages
lapply(required_packages, library, character.only = TRUE)

## ----DataPaths-------------------------------------------------------------------------------------
# Define data paths
data_dir <- "data/"
raw_data_dir <- paste0(data_dir, "raw/")
cleaned_data_dir <- paste0(data_dir, "cleaned/")
output_dir <- "output/"

# Create data directories if they don't exist
dir.create(data_dir, showWarnings = FALSE)
dir.create(raw_data_dir, showWarnings = FALSE)
dir.create(cleaned_data_dir, showWarnings = FALSE)

# Create output directory if it doesn't exist
dir.create(output_dir, showWarnings = FALSE)
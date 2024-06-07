library(ggplot2)
library(jsonlite)


# set the working directory to current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Source the reusable script
source("load_data.R")

# Load data
data <- load_data()


# Step 1: Read the JSON file to understand the data structure
json_file <- 'data_analysis/data_info.json'
data_info <- fromJSON(json_file)

# print data types

# Select only the numeric columns for PCA
# Select only the numeric columns for PCA
numeric_columns <- sapply(data, is.numeric)
data_numeric <- data[, numeric_columns]

# Handle missing values if any (e.g., by removing rows with NA)
data_numeric <- na.omit(data_numeric)


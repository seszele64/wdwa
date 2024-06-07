# load_data.R

# Imports
library(readr)
library(dplyr)
library(glmnet)
library(rstudioapi)

# Set the working directory to the current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Function to load test and train data
load_data <- function(data_dir = "data", test_file = "test.csv", train_file = "train.csv") {
  # Read in the data
  test_data <- read_csv(file.path(data_dir, test_file))
  train_data <- read_csv(file.path(data_dir, train_file))
  
  # Return as a list
  return(list(test_data = test_data, train_data = train_data))
}
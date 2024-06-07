# Load necessary libraries
library(ggplot2)
library(factoextra)

# Load the data
# set the working directory to current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Source the reusable script
source("load_data.R")

# Load data
data <- load_data()

# Access the test and train datasets
test_data <- data$test_data
train_data <- data$train_data



# Assuming har_data is your data frame
# Load the data (replace with actual data loading code)
# har_data <- read.csv("path_to_your_data.csv")
har_data <- read.csv('data/test.csv')

# Remove non-numeric columns (subject and Activity)
numeric_data <- har_data[, sapply(har_data, is.numeric)]

# Standardize the data
numeric_data_scaled <- scale(numeric_data)

# Perform PCA
pca_result <- prcomp(numeric_data_scaled, center = TRUE, scale. = TRUE)

# Summary of PCA
summary(pca_result)

# Scree plot to visualize the variance explained by each principal component
fviz_eig(pca_result)

# Biplot of the first two principal components
fviz_pca_biplot(pca_result, repel = TRUE, col.var = "blue", col.ind = "red")

# Optional: Add Activity labels to the PCA plot
har_data$Activity <- as.factor(har_data$Activity)

# Plots ----

# Scree plot
fviz_eig(pca_result)

#Cumulative Variance Explained
# Calculate cumulative variance explained
var_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
cum_var_explained <- cumsum(var_explained)

# Plot cumulative variance explained
plot(cum_var_explained, xlab = "Number of Components", ylab = "Cumulative Variance Explained",
     main = "Cumulative Variance Explained by Principal Components", type = "b")

# Perform PCA
pca_result <- prcomp(numeric_data_scaled, center = TRUE, scale. = TRUE)

# Check if PCA was performed successfully
if (!is.null(pca_result)) {
  # Summary of PCA
  summary(pca_result)
  
  # Scree plot to visualize the variance explained by each principal component
  fviz_eig(pca_result)
  
  # Biplot of the first two principal components
  fviz_pca_biplot(pca_result, repel = TRUE, col.var = "blue", col.ind = "red")
  
  # Optional: Add Activity labels to the PCA plot
  har_data$Activity <- as.factor(har_data$Activity)
  fviz_pca_ind(pca_result, label = "none", habillage = har_data$Activity, addEllipses = TRUE, ellipse.type = "confidence",
               palette = "jco", repel = TRUE)
} else {
  cat("PCA failed. Please check the input data and try again.")
}



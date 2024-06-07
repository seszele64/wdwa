# set the working directory to current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# imports
library(dplyr)
library(glmnet)

# Source the reusable script
source("load_data.R")

# Load data
data <- load_data()

# Access the test and train datasets
test_data <- data$test_data
train_data <- data$train_data

## Principal Component Analysis (PCA) ----

### Preparing the Data ----

# Load the necessary library
library(dplyr)

# Remove non-numeric columns if they exist
test_data <- select(test_data, where(is.numeric))

# Normalize the data
data_scaled <- scale(test_data)

# Perform PCA
pca <- prcomp(data_scaled, center = TRUE, scale. = TRUE)

### Analyzing PCA Output ----

# Print the summary of the PCA
summary(pca)

### Save PCA Results ----

# Save the PCA object
saveRDS(pca, file = "pca_model.rds")

# Save the PCA summary to a text file
sink("pca_summary.txt")
summary(pca)
sink()

# Save the PCA loadings (rotation matrix) to a CSV file
write.csv(pca$rotation, file = "pca_loadings.csv", row.names = TRUE)

# Save the PCA scores (principal components) to a CSV file
pca_scores <- data.frame(pca$x)
write.csv(pca_scores, file = "pca_scores.csv")

### Visualization ----

#### Scree plot ----

#basic
plot(pca, type = "lines")

# advanced
library(ggplot2)

# Assuming 'pca' is your PCA object
variances <- pca$sdev^2  # Get variances from the standard deviations
explained_variances <- variances / sum(variances) * 100  # Convert to percentage

# Create a data frame for plotting
pca_data <- data.frame(
  PC = 1:length(explained_variances),
  Variances = explained_variances
)

# Filter the data to only include the first 100 PCs
pca_data <- pca_data[pca_data$PC <= 20,]

# Create the plot
ggplot(pca_data, aes(x = PC, y = Variances)) +
  geom_line(color = "blue", size = 1) +  # Blue line
  geom_point(color = "red", size = 3, shape = 22, fill = "white") +  # Red points with white fill
  labs(title = "Scree Plot of PCA (First 20 Components)",
       x = "Principal Component",
       y = "Percentage of Variance Explained") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Center the title
    text = element_text(size = 14),
    axis.text.x = element_text(color = ifelse(pca_data$PC == 3, "red", "black"))  # Red color for x-axis label at PC = 3
  ) +
  geom_vline(xintercept = 3, linetype = "dashed", color = "darkgray") +  # Add a vertical line at the 'elbow'
  annotate("text", x = 3, y = pca_data$Variances[3] + 10, label = "Elbow Point", hjust = 0) +
  annotate("text", x = 8, y = 20, label = "Chosen as the optimal cut-off point\nfor dimensionality reduction based on\nthe steep drop in variance explained.", hjust = 0.5) +
  scale_x_continuous(breaks = 1:20, labels = function(x) ifelse(x == 3, paste0(x), x)) +  # Styling the x-axis labels
# show percentage of variance explained by each PC above the point, skew by 45 degrees for better visibility
  geom_text(aes(label = paste0(round(Variances, 1), "%")), hjust = 1.2, vjust = 1, angle = 45, size = 3) +
  # add chart bottom padding to make room for the text
  theme(plot.margin = margin(b = 5))

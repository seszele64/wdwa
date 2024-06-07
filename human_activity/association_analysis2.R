# Load necessary libraries
library(arules)
library(arulesViz)

# Assuming har_data is your data frame
# Load the data (replace with actual data loading code)
# har_data <- read.csv("path_to_your_data.csv")

# Convert the data to a transaction format
# For demonstration, let's assume we are interested in the first 10 columns
# and the 'Activity' column as the target variable
trans_data <- as(har_data[, 1:10], "transactions")

# Generate association rules using the apriori algorithm
rules <- apriori(trans_data, parameter = list(supp = 0.01, conf = 0.8))

# Summary of the rules
summary(rules)

# Visualize the rules using different methods

# 1. Scatter plot of rules
plot(rules, method = "scatterplot", measure = c("support", "confidence"), shading = "lift")

# 2. Matrix-based visualization
plot(rules, method = "matrix", measure = "lift")

# 3. Graph-based visualization
plot(rules, method = "graph", control = list(type = "items"))

# 4. Parallel coordinates plot
plot(rules, method = "paracoord", control = list(reorder = TRUE))

# 5. Grouped matrix plot
plot(rules, method = "grouped")

# Optional: Interactive visualization
if (interactive()) {
  plot(rules, method = "grouped", engine = "interactive")
}
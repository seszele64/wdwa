
# Load the libraries
library(arules)
library(arulesViz)
library(tidyverse)

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


## Data preparation ----

## select relevant columns
sensor_data <- train_data %>% select(Activity, everything())

# Categorize continuous variables (e.g., sensor readings) into discrete intervals
categorize_data <- function(data) {
  for (col in colnames(data)) {
    if (is.numeric(data[[col]])) {
      data[[col]] <- discretize(data[[col]], method = "interval", categories = 3)  # 3 intervals as an example
    }
  }
  return(data)
}

sensor_data_categorical <- categorize_data(sensor_data)

# Convert to transactions
trans <- as(sensor_data_categorical, "transactions")

## Apply Apriori Algorithm ----

# Apply the Apriori algorithm to find frequent itemsets
rules <- apriori(trans, parameter = list(support = 0.01, confidence = 0.5))

# Inspect some rules
inspect(rules[1:10])

## Visualize the rules ----

# Plot the association rules
plot(rules, method = "graph", control = list(type = "items"))

# Alternatively, you can try other visualization methods:
plot(rules, method = "grouped")
plot(rules, method = "scatterplot", measure = c("support", "lift"), shading = "confidence")

## Example Network Visualization ----

# Load the igraph library for better network visualization
install.packages("igraph")
library(igraph)

# Convert to a graph and plot
rule_graph <- plot(rules, method = "graph", engine = "igraph", control = list(type = "items"))

# Adjust visualization settings
V(rule_graph)$color <- "skyblue"
E(rule_graph)$color <- "gray"
plot(rule_graph, vertex.size = 15, vertex.label.cex = 0.8, edge.arrow.size = 0.5)

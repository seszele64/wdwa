# Imports ----

# Load the libraries
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)

library(arules)
library(arulesViz)
library(tidyverse)

# Load data ----

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

# Convert the target variable `Activity` to a factor
train_data$Activity <- as.factor(train_data$Activity)
test_data$Activity <- as.factor(test_data$Activity)

## Train a Decision Tree Model ----

# Train the decision tree model using the training data
decision_tree_model <- rpart(Activity ~ ., data = train_data, method = "class")

# Print a summary of the decision tree
summary(decision_tree_model)

library(rpart)
# Example of training a decision tree model (adjust the formula as needed)
decision_tree_model <- rpart(Activity ~ ., data = train_data, method = "class")

# Adjust margins and plot directly
par(mar = c(1, 1, 1, 1))
rpart.plot(decision_tree_model, type = 2, extra = 104, fallen.leaves = TRUE, main = "Decision Tree for Activity Recognition")

## Evaluate the Model ----

# Predict the activity on the test data
predictions <- predict(decision_tree_model, test_data, type = "class")

# Evaluate the accuracy of the model
confusion_matrix <- confusionMatrix(predictions, test_data$Activity)
print(confusion_matrix)

## Plot Variable Importance ----

library(ggplot2)

# Example training of decision tree model
decision_tree_model <- rpart(Activity ~ ., data = train_data, method = "class")

# Get the variable importance
importance <- data.frame(Feature = names(decision_tree_model$variable.importance), 
                         Importance = decision_tree_model$variable.importance)

# Sort by importance
importance <- importance %>% arrange(desc(Importance))

# Limit to the top 20 important variables (or choose your limit)
top_n <- 20
importance <- importance[1:top_n, ]

# Plot using ggplot2
ggplot(importance, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Top 20 Variable Importance in Decision Tree",
       x = "Features", 
       y = "Importance") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))

## Advanced Plotting with rattle ----

# If you want to visualize the decision tree with more details, try `rattle`
library(rattle)

fancyRpartPlot(decision_tree_model)




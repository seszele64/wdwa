# Install necessary packages if they aren't already installed
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("readr")) install.packages("readr")  # For read_csv

# Load the packages
library(ggplot2)
library(dplyr)
library(readr)

# path to currently running script
current_folder <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Load your data from CSV file, relative to the current working directory
heart_data <- read_csv(file.path(current_folder, "/data/heart.csv"))

# Display the structure of the dataset
print(str(heart_data))

# EDA ----

## 1. Summary Statistics ----
# Generating summary statistics for all variables
summary_stats <- heart_data %>% summarise_all(funs(min, max, mean, median, sd, IQR))
print(summary_stats)

## 2. Visualizing Distributions ----

# Histograms for continuous variables
ggplot(heart_data, aes(x = chol)) + 
  geom_histogram(bins = 30, fill = "blue", color = "black") + 
  ggtitle("Distribution of Serum Cholesterol") + 
  xlab("Serum Cholesterol") + 
  ylab("Frequency")

# Box plot for age
ggplot(heart_data, aes(y = age, x = factor(1))) + 
  geom_boxplot(fill = "lightblue") + 
  ggtitle("Box Plot of Age") +
  xlab("") + 
  ylab("Age")


## 3. Correlation Analysis ----
# we are gonna use reshape2 package to melt the correlation matrix

# Install and load the reshape2 package if it isn't already installed
if (!require("reshape2")) install.packages("reshape2")
library(reshape2)

# Calculate correlation matrix
cor_matrix <- cor(heart_data[, sapply(heart_data, is.numeric)])  # select only numeric columns

# Use melt from reshape2 to convert the correlation matrix for ggplot2
cor_matrix_melted <- melt(cor_matrix)

# Visualize the correlation matrix using ggplot2
ggplot(cor_matrix_melted, aes(Var1, Var2, fill = value)) + 
  geom_tile() + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Correlation Matrix")

## 4. Group Comparisons ----

# Comparing cholesterol levels by sex
ggplot(heart_data, aes(x = factor(sex), y = chol, fill = factor(sex))) + 
  geom_violin() +
  labs(title = "Cholesterol Levels by Sex", x = "Sex", y = "Cholesterol")

# Heart disease presence by age groups
heart_data$age_group <- cut(heart_data$age, breaks = seq(20, 80, by = 10))
ggplot(heart_data, aes(x = age_group, fill = factor(target))) +  # Assuming 'target' is the heart disease variable
  geom_bar(position = "fill") +
  labs(title = "Heart Disease Presence by Age Group", x = "Age Group", y = "Proportion")

# Feature Engineering ----

## 1. Interaction Terms ----

### Interaction between Age and Max Heart Rate
heart_data$age_thalach_interaction <- heart_data$age * heart_data$thalach

## 2. Binning Continuous Variables ----
heart_data$age_category <- cut(heart_data$age, breaks=c(29, 40, 55, 120), labels=c("young", "middle-aged", "elderly"), include.lowest=TRUE)

## 3. Polynomial Features ----
heart_data$age_squared <- heart_data$age^2
heart_data$oldpeak_squared <- heart_data$oldpeak^2


## 4. Creating Dummy Variables ----
heart_data <- cbind(heart_data, model.matrix(~age_category - 1, data=heart_data))

# Analysis & Visualization ----

## 1. Machine Learning Models ----

## 2. Visualizations ----

### Decision Tree Model using rpart ----

# Install and load necessary packages
if (!require("rpart")) install.packages("rpart")
if (!require("rpart.plot")) install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

# Building the decision tree model
# Assume 'target' is your outcome variable and you are using all other columns as predictors
tree_model <- rpart(target ~ ., data=heart_data, method="class")

# Visualize the decision tree
rpart.plot(tree_model, extra=104)  # Provides a detailed visual of the tree

### Advanced Visualizations to explore correlations and feature impacts ----

# Install ggplot2 if not already installed
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)

# Visualization of Age and Max Heart Rate Interaction
ggplot(heart_data, aes(x=age, y=thalach, color=factor(target))) +
  geom_point(alpha=0.5) +
  geom_smooth(method="lm", se=FALSE) +
  labs(title="Interaction of Age and Max Heart Rate", x="Age", y="Max Heart Rate")

# Visualization of Polynomial Features Impact
ggplot(heart_data, aes(x=age_squared, fill=factor(target))) +
  geom_histogram(position="identity", alpha=0.5, bins=30) +
  labs(title="Distribution of Age Squared by Heart Disease Status", x="Age Squared", y="Count")

## Risk factor analysis ----

### Stacked Bar Charts for Risk Factors ----


# Assuming 'fbs' is your fasting blood sugar status variable and 'target' indicates the presence of heart disease
# Install and load necessary packages
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)

# Creating the stacked bar chart for fasting blood sugar status
ggplot(heart_data, aes(x = factor(fbs), fill = factor(target))) + 
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Proportion of Heart Disease by Fasting Blood Sugar",
       x = "Fasting Blood Sugar > 120 mg/dl",
       y = "Percentage",
       fill = "Heart Disease") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"),
                    labels = c("No Heart Disease", "Heart Disease")) +
  theme_minimal()



### Point Plots ----

# Install and load necessary packages
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)

# Grouping data by age and calculating the probability of heart disease
age_data <- heart_data %>%
  group_by(age) %>%
  summarise(HeartDisease = mean(target == 1), .groups = 'drop')

# Creating the point plot
ggplot(age_data, aes(x = age, y = HeartDisease)) + 
  geom_point() +
  geom_line(group=1, colour="red") +
  labs(title = "Probability of Heart Disease Across Age",
       x = "Age",
       y = "Probability of Heart Disease") +
  theme_minimal()


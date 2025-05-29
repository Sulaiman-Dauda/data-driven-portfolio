# Load libraries
library(ggplot2)       # Loads the ggplot2 library for enhanced plotting capabilities
library(cluster)      # Loads the cluster library for clustering algorithms
library(caret)        # Loads the caret library for feature selection
library(randomForest) # Loads the randomForest library for random forest feature importance
library(brglm)        # Loads the brglm package
library(MASS)         # Loads the MASS library for various statistical methods
library(pROC)         # Loads the pROC library for ROC analysis

# Read the dataset
alzheimer_data <- read.csv("project data.csv")  # Reads the dataset from the file "project data.csv" and assigns it to the variable 'alzheimer_data'

# Data preprocessing
alzheimer_data$M.F <- ifelse(alzheimer_data$M.F == "M", 1, 0)  # Converts the 'M.F' variable to numeric values: 'M' becomes 1 and 'F' becomes 0
alzheimer_data <- alzheimer_data[alzheimer_data$Group != "Converted", ]  # Removes rows where the 'Group' variable is "Converted"
alzheimer_data <- na.omit(alzheimer_data)  # Removes rows with missing values from the dataset

# Task 1: Descriptive statistics
dim(alzheimer_data)  # Returns the dimensions (rows and columns) of the dataset
str(alzheimer_data)  # Displays the structure and summary of the dataset
head(alzheimer_data)  # Displays the first few rows of the dataset
summary_table <- summary(alzheimer_data)  # Generates summary statistics for the dataset and stores it in 'summary_table'
print(summary_table)  # Prints the summary statistics

# Exploratory data analysis - Boxplots
par(mfrow = c(3, 3))  # Sets the layout for multiple plots in a grid
var_names <- c("Age", "EDUC", "SES", "MMSE", "CDR", "eTIV", "nWBV", "ASF")  # Defines the variables to be plotted
for (var in var_names) {
  boxplot(alzheimer_data[[var]], main = var)  # Creates a boxplot for each variable
}

boxplot(scale(alzheimer_data[,3:10]), main="Boxplots of all variables")

# Exploratory data analysis - Histograms
par(mfrow = c(3, 3))  # Sets the layout for multiple plots in a grid
for (var in var_names) {
  hist(alzheimer_data[[var]], main = var)  # Creates a histogram for each variable
}

# Exploratory data analysis - Scatterplot
ggplot(alzheimer_data, aes(x = eTIV, y = nWBV)) +
  geom_point() +
  labs(x = "eTIV", y = "nWBV") +
  ggtitle("Scatterplot: eTIV vs nWBV")  # Creates a scatterplot of 'eTIV' vs 'nWBV' using ggplot

# Task 2: Clustering
selected_vars <- c("Age", "EDUC", "SES", "MMSE", "CDR", "eTIV", "nWBV", "ASF")  # Defines the variables for clustering
selected_data <- alzheimer_data[, selected_vars]  # Creates a new dataset with only the selected variables

# Apply k-means clustering
kmeans_result <- kmeans(scale(selected_data), centers = 3, nstart = 10)  # Performs k-means clustering on the scaled selected data

# Add cluster assignment to the dataset
alzheimer_data$cluster <- as.factor(kmeans_result$cluster)  # Adds a new column 'cluster' to the original dataset with cluster assignments

# Visualize the clusters using a scatterplot
cluster_scatterplot <- ggplot(alzheimer_data, aes(x = eTIV, y = nWBV, color = cluster)) +
  geom_point() +
  labs(x = "eTIV", y = "nWBV", color = "Cluster") +
  ggtitle("K-means Clustering: eTIV vs nWBV")  # Creates a scatterplot of 'eTIV' vs 'nWBV' with clusters shown in different colors

# Task 3: Logistic Regression
# Split the dataset into training and testing sets
set.seed(123)  # Sets a seed for reproducibility
train_index <- createDataPartition(alzheimer_data$Group, p = 0.7, list = FALSE)  # Creates an index for splitting the dataset
train_data <- alzheimer_data[train_index, ]  # Creates the training dataset
test_data <- alzheimer_data[-train_index, ]  # Creates the testing dataset

# Convert "Group" variable to binary
train_data$Group <- ifelse(train_data$Group == "Demented", 1, 0)  # Converts "Group" variable to binary values

# Fit a logistic regression model
logistic_model <- brglm(Group ~ ., data = train_data, family = binomial())  # Fit logistic regression model using Firth's penalized maximum likelihood estimation
logistic_model  # Displays the logistic regression model

# Evaluate the model's performance
predicted_probs <- predict(logistic_model, newdata = test_data, type = "response")  # Predicts probabilities using the logistic regression model
predicted_classes <- ifelse(predicted_probs > 0.5, "Demented", "Nondemented")  # Assigns classes based on predicted probabilities
accuracy <- mean(predicted_classes == test_data$Group)  # Calculates accuracy by comparing predicted classes with actual classes
auc <- roc(test_data$Group, predicted_probs)$auc  # Calculates the Area Under the Curve (AUC) using the predicted probabilities

# Interpret the model coefficients and comment on their significance
summary(logistic_model)  # Displays the summary of the logistic regression model

# Task 4: Feature Selection
# Fit a logistic regression model using forward feature selection
forward_model <- stepAIC(glm(Group ~ 1, data = train_data, family = "binomial"), direction = "forward", scope = formula(train_data))  # Fits a logistic regression model using forward feature selection
forward_model  # Displays the forward-selected model

# Fit a logistic regression model using backward feature selection
backward_model <- stepAIC(glm(Group ~ ., data = train_data, family = "binomial"), direction = "backward")  # Fits a logistic regression model using backward feature selection
backward_model  # Displays the backward-selected model

# Evaluate the performance of forward-selected model
predicted_probs_forward <- predict(forward_model, newdata = test_data, type = "response")  # Predicts probabilities using the forward-selected model
predicted_classes_forward <- ifelse(predicted_probs_forward > 0.5, "Demented", "Nondemented")  # Assigns classes based on predicted probabilities
accuracy_forward <- mean(predicted_classes_forward == test_data$Group)  # Calculates accuracy by comparing predicted classes with actual classes
auc_forward <- roc(test_data$Group, predicted_probs_forward)$auc  # Calculates the AUC using the predicted probabilities

# Evaluate the performance of backward-selected model
predicted_probs_backward <- predict(backward_model, newdata = test_data, type = "response")  # Predicts probabilities using the backward-selected model
predicted_classes_backward <- ifelse(predicted_probs_backward > 0.5, "Demented", "Nondemented")  # Assigns classes based on predicted probabilities
accuracy_backward <- mean(predicted_classes_backward == test_data$Group)  # Calculates accuracy by comparing predicted classes with actual classes
auc_backward <- roc(test_data$Group, predicted_probs_backward)$auc  # Calculates the AUC using the predicted probabilities

# Interpret the forward-selected model coefficients and comment on their significance
summary(forward_model)  # Displays the summary of the forward-selected model

# Interpret the backward-selected model coefficients and comment on their significance
summary(backward_model)  # Displays the summary of the backward-selected model

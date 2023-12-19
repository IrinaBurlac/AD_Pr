library(dplyr)
library(ggplot2)
library(caret)
library(tidyr)
library(corrplot)

if (!requireNamespace("corrplot", quietly = TRUE))
  install.packages("corrplot")
  
# Reading the dataset
data <- read.csv("D:/Burlac_Irina_MI211/spotify-2023.csv")

# Viewing the first few rows and column names
head(data)
colnames(data)

#######################################
# EDA (Exploratory Data Analysis)

# Converting relevant columns to numeric types
data$streams <- as.numeric(gsub("[^0-9.-]", "", data$streams))

# Handling missing values (if any)
data <- na.omit(data)

# Identifying types of variables
numeric_cols <- sapply(data, is.numeric)
categorical_cols <- sapply(data, is.factor)

# Density plot for each numeric variable
data_long <- data[, numeric_cols] %>%
  gather(key = "Variable", value = "Value")

ggplot(data_long, aes(x = Value, fill = Variable)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ Variable, scales = "free") +
  labs(title = "Distribution of Numeric Variables", x = "Value", y = "Density") +
  theme_minimal()

#######################################
# Artist Popularity Trend Over Time
# Attempting to convert the 'streams' column to numeric
data$streams <- as.numeric(gsub("[^0-9.-]", "", data$streams))

# Removing rows where streams is NA (after conversion)
data <- data[!is.na(data$streams), ]

# Handling missing values (if any) in other columns
data <- na.omit(data)




#######################################
# Preparing Data for Linear Regression

# Splitting the data into training and testing sets
set.seed(42)
partition <- createDataPartition(data$streams, p = 0.8, list = FALSE)
training_set <- data[partition, ]
testing_set <- data[-partition, ]

# Normalizing numeric data
training_set_norm <- as.data.frame(scale(training_set[, numeric_cols]))
testing_set_norm <- as.data.frame(scale(testing_set[, numeric_cols]))

# Building the linear regression model
model <- lm(streams ~ ., data = training_set_norm)

# Summary of the model
summary(model)

cor_matrix <- cor(data[, numeric_cols])

# Visualizing the correlation matrix
corrplot(cor_matrix, method = "circle", type = "upper", order = "hclust", 
         tl.cex = 0.8, tl.col = "black")


#######################################
# Model Evaluation

# Predictions on the test set
predictions <- predict(model, newdata = testing_set_norm)

# Calculating Mean Squared Error (MSE) and Root Mean Squared Error (RMSE)
mse <- mean((predictions - testing_set_norm$streams)^2)
rmse <- sqrt(mse)

print(paste("MSE:", mse))
print(paste("RMSE:", rmse))

#######################################
# Visualizing the Results

# Comparing actual values with predicted values
results <- data.frame(Real = testing_set_norm$streams, Predicted = predictions)
ggplot(results, aes(x = Real, y = Predicted)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  ggtitle("Comparison of Actual vs Predicted Values") +
  xlab("Actual Values") +
  ylab("Predicted Values") +
  theme_minimal()



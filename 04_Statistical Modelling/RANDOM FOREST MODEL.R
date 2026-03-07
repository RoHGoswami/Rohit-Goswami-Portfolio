library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(lmtest)
library(lme4)
library(randomForest)
library(xgboost)
library(car)
library(Metrics)
library(caret)

data = read.csv("Student_data_low.csv", header = TRUE)
str(data)
summary(data)

# Step 2: Clean missing values
data[data == ""] <- NA  # Replace blanks with NA
print(colSums(is.na(data)))  # Check for missing values

# Step 3: Summarize the dataset
summary(data)

# Step 4: Impute missing values using mode
get_mode <- function(x) {
  ux <- unique(x[!is.na(x)])  # Unique non-NA values
  ux[which.max(tabulate(match(x, ux)))]  # Most frequent value
}

# Impute mode for specified columns
if ("Parental_Education_Level" %in% colnames(data)) {
  data$Parental_Education_Level[is.na(data$Parental_Education_Level)] <- get_mode(data$Parental_Education_Level)
}
if ("Distance_from_Home" %in% colnames(data)) {
  data$Distance_from_Home[is.na(data$Distance_from_Home)] <- get_mode(data$Distance_from_Home)
}
if ("Teacher_Quality" %in% colnames(data)) {
  data$Teacher_Quality[is.na(data$Teacher_Quality)] <- get_mode(data$Teacher_Quality)
}

# Step 5: Encode categorical variables
encoded_data <- data

# Ordinal encoding
ordinal_vars <- list(
  Motivation_Level = c("Low", "Medium", "High"),
  Parental_Involvement = c("Low", "Medium", "High"),
  Access_to_Resources = c("Low", "Medium", "High"),
  Family_Income = c("Low", "Medium", "High"),
  Teacher_Quality = c("Low", "Medium", "High"),
  Parental_Education_Level = c("High School", "College", "Postgraduate"),
  Distance_from_Home = c("Near", "Moderate", "Far")
)

for (var in names(ordinal_vars)) {
  if (var %in% colnames(encoded_data)) {
    encoded_data[[var]] <- as.numeric(factor(encoded_data[[var]], levels = ordinal_vars[[var]], ordered = TRUE))
  }
}

# Binary encoding
binary_vars <- list(
  Internet_Access = c("Yes", "No"),
  Learning_Disabilities = c("Yes", "No"),
  Extracurricular_Activities = c("Yes", "No"),
  School_Type = c("Public", "Private"),
  Gender = c("Female", "Male")
)

for (var in names(binary_vars)) {
  if (var %in% colnames(encoded_data)) {
    encoded_data[[var]] <- ifelse(encoded_data[[var]] == binary_vars[[var]][1], 1, 0)
  }
}

# Ensure Peer_Influence is treated as a factor (why and when do we treat variables as factors?)
if ("Peer_Influence" %in% colnames(encoded_data)) {
  encoded_data$Peer_Influence <- as.factor(encoded_data$Peer_Influence)
}
# Load necessary libraries
library(randomForest)
library(caret)

# Set seed for reproducibility
set.seed(123)

# Fit a Random Forest model
rf_model <- randomForest(Exam_Score ~ ., data = encoded_data, ntree = 500, mtry = 4, importance = TRUE)

# Print the model summary
print(rf_model)

# Plot variable importance
varImpPlot(rf_model, main = "Variable Importance in Random Forest Model")


predicted_values <- predict(rf_model, encoded_data)
actual_values <- encoded_data$Exam_Score

mse_value <- mean((actual_values - predicted_values)^2)
rmse_value <- sqrt(mse_value)

cat("MSE:", mse_value, "\n")
cat("RMSE:", rmse_value, "\n")

mae_value <- mean(abs(actual_values - predicted_values))
cat("MAE:", mae_value, "\n")

rsq <- 1 - (sum((actual_values - predicted_values)^2) / sum((actual_values - mean(actual_values))^2))
cat("R-squared:", rsq, "\n")

# Checking for overfitting
set.seed(123)
train_index <- createDataPartition(encoded_data$Exam_Score, p = 0.8, list = FALSE)
train_data <- encoded_data[train_index, ]
test_data <- encoded_data[-train_index, ]

rf_model <- randomForest(Exam_Score ~ ., data = train_data, ntree = 500, mtry = 4, importance = TRUE)

# Predict on test data
test_pred <- predict(rf_model, test_data)
test_actual <- test_data$Exam_Score

# Calculate test RMSE
test_rmse <- sqrt(mean((test_actual - test_pred)^2))
cat("Test RMSE:", test_rmse, "\n")

importance(rf_model)
varImpPlot(rf_model, main = "Feature Importance in Random Forest Model")
residuals <- actual_values - predicted_values
hist(residuals, main = "Residual Histogram", xlab = "Residuals", breaks = 20)

library(corrplot)
cor_matrix <- encoded_data %>% select(-Exam_Score) %>% corrplot()
print(cor_matrix)
tuned_model <- train(
  Exam_Score ~ ., data = train_data, method = "rf",
  trControl = trainControl(method = "cv", number = 5),
  tuneLength = 5
)
print(tuned_model)

library(ggplot2)
importance_df <- data.frame(
  Feature = rownames(importance(rf_model)),
  Importance = importance(rf_model)[, 1]
)

ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "blue", color = 'black') +
  coord_flip() +
  labs(title = "Feature Importance (Random Forest) Student data low", x = "Features", y = "Importance") +
  theme_minimal()




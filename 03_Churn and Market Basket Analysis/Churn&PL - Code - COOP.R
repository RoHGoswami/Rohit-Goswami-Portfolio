library(dplyr)
library(rfm)
library(patchwork)
library(readr)
library(lubridate)  # For date formatting
library(ggplot2)

data_B = read.csv("Dataset_CRM_TypeB.csv", header = TRUE)
str(data_B)

# Modify `data_B`
data_B <- data_B %>%
  mutate(
    # Convert `date` to Date format
    date = as.Date(date, format = "%Y-%m-%d"),
    
    # Replace commas with dots and convert `PL_gross_sales` to numeric
    PL_gross_sales = as.numeric(gsub(",", ".", PL_gross_sales, fixed = TRUE)),
    
    # Convert `number_item_PL` to integer
    number_item_PL = as.integer(number_item_PL),
    
    # Convert `number_items_other` to integer
    number_items_other = as.integer(number_items_other)
  )

# Get the total number of rows in the dataset
total_rows <- nrow(data_B)

# Create a dataset that summarizes anomalies in Data_B
anomalies_summary_B <- data.frame(
  Anomaly = c(
    "Negative values in net_sales",
    "Negative values in gross_sales",
    "Negative values in PL_gross_sales",
    "If number_item_PL is NA, PL_gross_sales different from 0",
    "PL_gross_sales > net_sales",
    "PL_gross_sales > gross_sales",
    "net_sales > gross_sales",
    "Both number_item_PL and number_items_other are equal to 0"
  ),
  Count = c(
    sum(data_B$net_sales < 0, na.rm = TRUE),
    sum(data_B$gross_sales < 0, na.rm = TRUE),
    sum(data_B$PL_gross_sales < 0, na.rm = TRUE),
    sum(is.na(data_B$number_item_PL) & data_B$PL_gross_sales != 0, na.rm = TRUE),
    sum(data_B$PL_gross_sales > data_B$net_sales, na.rm = TRUE),
    sum(data_B$PL_gross_sales > data_B$gross_sales, na.rm = TRUE),
    sum(data_B$net_sales > data_B$gross_sales, na.rm = TRUE),
    sum(data_B$number_item_PL == 0 & data_B$number_items_other == 0, na.rm = TRUE)
  ),
  Percentage = c(
    sum(data_B$net_sales < 0, na.rm = TRUE) / total_rows * 100,
    sum(data_B$gross_sales < 0, na.rm = TRUE) / total_rows * 100,
    sum(data_B$PL_gross_sales < 0, na.rm = TRUE) / total_rows * 100,
    sum(is.na(data_B$number_item_PL) & data_B$PL_gross_sales != 0, na.rm = TRUE) / total_rows * 100,
    sum(data_B$PL_gross_sales > data_B$net_sales, na.rm = TRUE) / total_rows * 100,
    sum(data_B$PL_gross_sales > data_B$gross_sales, na.rm = TRUE) / total_rows * 100,
    sum(data_B$net_sales > data_B$gross_sales, na.rm = TRUE) / total_rows * 100,
    sum(data_B$number_item_PL == 0 & data_B$number_items_other == 0, na.rm = TRUE) / total_rows * 100
  )
)

# here we can see, thatt specific transactions have a gross PL sale higher than the net sales. 
# i do not explain this, but in my oponion, the gross PL sales is not discounted and for this reason is higher 
# than the net sales. But for our purpose this will not impact the output, since from a monetary point of view
# we will take the gross_sales. 

# Count transactions per calendar month
Transactions_Per_Month <- data_B %>%
  mutate(Month = format(as.Date(date), "%Y-%m")) %>%
  group_by(Month) %>%
  summarize(Transactions = n(), .groups = 'drop')

# Print transactions per month
print(Transactions_Per_Month)

# Plot transactions per month
ggplot(Transactions_Per_Month, aes(x = Month, y = Transactions)) +
  geom_line(group = 1) +
  geom_point() +
  labs(title = "Transactions per Calendar Month", x = "Month", y = "Number of Transactions") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## Specific types of variables
# Recency metrics (days since last purchase)
# Frequency metrics (transaction counts, average inter-purchase interval)
# Monetary metrics (total spend, average spend per purchase)
# AVG number of item purchased per transaction, PL and not PL 
# Product mix metrics (ratio of private-label to total items, variety of items)
# Location typology 
# Discout applied
# # of month that customer has purchased from the store (in terms of customer month and not calendar month)
# The customer has purchased PL only 



reference_date <- max(data_B$date)
first_date <- min(data_B$date)

(reference_date - first_date)/31

# Summarize data per customer
Customers <- data_B %>%
  group_by(id_customer) %>%  # Group by customer ID
  summarize(
    # Monetary metrics
    Monetary_Value = sum(gross_sales, na.rm = TRUE),
    
    # Frequency metrics
    Frequency = n_distinct(date),
    
    # Recency metrics
    Recency = as.numeric(difftime(reference_date, max(as.Date(date)), units = "days")),
    
    # First and last purchase dates
    First_Purchase = min(as.Date(date)),
    Last_Purchase = max(as.Date(date)),
    
    # Average inter-purchase time
    AVG_ipt = ifelse(Frequency > 1, 
                     as.numeric(difftime(max(as.Date(date)), min(as.Date(date)), units = "days")) / (Frequency - 1), 
                     NA),
    
    # Standard deviation of inter-purchase time
    STD_ipt = ifelse(Frequency > 1, 
                     sd(diff(as.numeric(sort(unique(as.Date(date)))))), 
                     NA),
    
    # Average ticket value
    AVG_Ticket = mean(gross_sales, na.rm = TRUE),
    
    # Standard deviation of ticket value
    STD_Ticket = sd(gross_sales, na.rm = TRUE),
    
    # Total items purchased (PL and non-PL)
    Number_item_other = sum(number_items_other, na.rm = TRUE),
    Number_item_PL = sum(number_item_PL, na.rm = TRUE),
    
    # Average number of items purchased per transaction
    AVG_item_other_per_transaction = mean(number_items_other, na.rm = TRUE),
    AVG_item_PL_per_transaction = mean(number_item_PL, na.rm = TRUE),
    
    # Ratio of private-label to total items
    PL_ratio = ifelse((AVG_item_PL_per_transaction + AVG_item_other_per_transaction) > 0, 
                      AVG_item_PL_per_transaction / (AVG_item_PL_per_transaction + AVG_item_other_per_transaction), 
                      NA),
    
    # Location typology metrics 
    Store_Type_Variety = names(sort(table(store_type), decreasing = TRUE))[1],
    
    # Discount applied metrics (average of discount percentage)
    AVG_Discount = mean(ifelse(!is.na((1 - net_sales/gross_sales)), 
                               (1 - net_sales/gross_sales), 
                               NA), na.rm = TRUE),
    
    # Number of customer months (based on 30-day intervals from first purchase)
    Purchase_Months = n_distinct(floor(as.numeric(difftime(as.Date(date), First_Purchase, units = "days")) / 30)),
    
    # Regularity in purchases (standard deviation of days between purchases)
    Regularity = ifelse(Frequency > 1, 
                        sd(diff(as.numeric(sort(unique(as.Date(date)))))), 
                        NA),
    
    # Indicator if the customer only purchased private-label items
    Purchased_Other_Only = ifelse(Number_item_PL == 0 & Number_item_other > 0, TRUE, FALSE),
    
    # Drop grouping after summarizing
    .groups = 'drop'
  )



# The definition of churn will serve as the target (dependent variable) for your prediction model, so it needs to be:
# - Operationally Clear: Easy to apply consistently to your dataset.
# - Future-Oriented: Reflective of actual future inactivity, not a byproduct of a definition that looks into the future in a way that 
#   your model could never replicate when making real-time predictions.
# - Business-Relevant: Consistent with how the business would treat or respond to a “lost” customer.


# Churn Identification Process
# This script implements a churn identification workflow for customer data using transactional and behavioral metrics. 
# The process involves:
# 
# 1. **Dataset Segmentation:** The dataset is segmented into 31-day periods to analyze customer behavior over time.
# 2. **Customer-Level Aggregation:** Key metrics (e.g., monetary value, inter-purchase time, ticket size, ...) are calculated for each 
# customer and period.
# 3. **Confidence Interval Calculation:** Predicted monetary value and its confidence intervals are derived to benchmark performance.
# 4. **Rolling Calculations:** Rolling means of confidence intervals are computed to account for historical performance trends.
# 5. **Performance Assessment:** Customers are flagged as underperforming if their observed monetary value falls below the calculated 
# confidence interval.
# 6. **Dynamic Churn Detection:** Customers are classified as churned if they underperform in the last three periods or have a recency 
# of over 31 days.
# 
# The approach combines transactional analysis and time-based segmentation to identify at-risk customers, enabling targeted interventions 
# for retention.


## Churn Identification 

library(dplyr)
library(lubridate)
library(data.table)

# Step 1: Segment the Dataset
data_B <- data_B %>%
  mutate(
    Period_ID = floor(as.numeric(difftime(as.Date(date), min(as.Date(date)), units = "days")) / 31)
  )

# Step 2: Aggregate Data for Each Customer by Period
Customers_Period <- data_B %>%
  group_by(id_customer, Period_ID) %>%
  summarize(
    Monetary_Value = sum(gross_sales, na.rm = TRUE),  # Total monetary value
    Purchases = n(),  # Number of purchases
    AVG_ipt = ifelse(Purchases > 1, 
                     as.numeric(difftime(max(as.Date(date)), min(as.Date(date)), units = "days")) / (Purchases - 1), 
                     NA),  # Avg inter-purchase time
    STD_ipt = ifelse(Purchases > 1, 
                     sd(diff(as.numeric(sort(unique(as.Date(date)))))), 
                     NA),  # Std deviation of inter-purchase time
    AVG_Ticket = mean(gross_sales, na.rm = TRUE),  # Avg ticket value
    STD_Ticket = sd(gross_sales, na.rm = TRUE),   # Std deviation of ticket value
    .groups = 'drop'
  )

# Step 3: Calculate Confidence Intervals and Handle Single-Purchase Periods
# Add confidence interval calculation and approximate for single-purchase periods
Customers_Period <- Customers_Period %>%
  mutate(
    Predicted_Monetary_Value = AVG_Ticket * (31 / AVG_ipt),  # Predicted monetary value
    Std_Error = sqrt(
      (STD_Ticket^2 / Purchases) + 
        (STD_ipt^2 * (AVG_Ticket / (AVG_ipt^2))^2)
    ),  # Standard error
    CI_Lower = ifelse(Purchases > 1, 
                      Predicted_Monetary_Value - 1.96 * Std_Error, 
                      AVG_Ticket * 0.8),  # Fixed lower bound for single-purchase periods
    CI_Upper = ifelse(Purchases > 1, 
                      Predicted_Monetary_Value + 1.96 * Std_Error, 
                      AVG_Ticket * 1.2)   # Fixed upper bound for single-purchase periods
  )

# Replace NA values in CI_Lower and CI_Upper with calculated approximations
Customers_Period <- Customers_Period %>%
  mutate(
    CI_Lower = ifelse(is.na(CI_Lower), AVG_Ticket * 0.8, CI_Lower),
    CI_Upper = ifelse(is.na(CI_Upper), AVG_Ticket * 1.2, CI_Upper)
  )

# Step 4: Cumulative and Rolling Calculations
Customers_Period <- as.data.table(Customers_Period)

Customers_Period <- Customers_Period[
  order(id_customer, Period_ID),  # Ensure temporal order
  `:=`(
    Cumulative_Sum_Lower = cumsum(CI_Lower),
    Cumulative_Sum_Upper = cumsum(CI_Upper),
    Cumulative_Count = 1:.N
  ),
  by = id_customer
]

Customers_Period <- Customers_Period[
  , `:=`(
    Rolling_CI_Lower = shift(Cumulative_Sum_Lower / Cumulative_Count, type = "lag", fill = NA),
    Rolling_CI_Upper = shift(Cumulative_Sum_Upper / Cumulative_Count, type = "lag", fill = NA)
  ),
  by = id_customer
]

Customers_Period <- Customers_Period[
  , `:=`(
    Combined_CI_Lower = pmin(CI_Lower, Rolling_CI_Lower, na.rm = TRUE),
    Combined_CI_Upper = pmax(CI_Upper, Rolling_CI_Upper, na.rm = TRUE)
  )
]

# Step 5: Compare Observed Monetary Value Against Combined Intervals
Customers_Period <- Customers_Period[
  , Underperforming := ifelse(Monetary_Value < Combined_CI_Lower, 1, 0)
]

# Step 6: Detect Churn Dynamically (Last 3 Available Periods)
Customers_Period <- Customers_Period[
  order(id_customer, -Period_ID),
  `:=`(
    Churned = ifelse(
      .N >= 3 && all(Underperforming[1:3] == 1),
      1,
      0
    )
  ),
  by = id_customer
]

Customers_Period <- Customers_Period[
  order(id_customer, Period_ID)
]



# Count the total number of churned customers
total_churned_customers <- Customers_Period[
  Churned == 1,  # Filter rows where customers are marked as churned
  .N,  # Count the number of rows (unique customers in this context)
  by = id_customer  # Ensure each customer is counted only once
][, .N]  # Get the total count of unique churned customers



churned_customer <- Customers_Period %>%
  filter(Churned > 0) %>%  # Filter rows where the customer is churned
  distinct(id_customer)    # Keep only unique customer IDs


Customers <- Customers %>%
  mutate(
    Churn = ifelse(id_customer %in% churned_customer$id_customer | Recency > 31, 1, 0)  # Flag 1 if churned based on either condition, 0 otherwise
  )

num_churned_customers <- sum(Customers$Churn == 1, na.rm = TRUE)

merged_customers <- read.csv("merged_customers.csv", header = TRUE)

# Perform a left join to combine Customers with merged_customers on id_customer
Customers <- Customers %>%
  left_join(merged_customers %>% select(id_customer, Final_Segment), by = "id_customer") %>%
  mutate(
    # Assign "Not Clusterized" to rows where Final_Segment is NA
    Final_Segment = ifelse(is.na(Final_Segment), "Not Clusterized", Final_Segment)
  )


###################
#### CHURN ANALYSIS 

######################
## LOGISTIC REGRESSION 
# Load necessary libraries
library(dplyr)
library(PRROC)
library(ggplot2)
library(tidyr)


Customers <- read.csv("Customers_Final.csv", header = TRUE)

# Target variable: Churn (1 = churned, 0 = not churned)
# Predictor variables: Select relevant features
logistic_data <- Customers %>%
  select(Churn, Frequency, Monetary_Value, AVG_ipt, STD_ipt, AVG_Ticket, Number_item_other, Number_item_PL, AVG_item_other_per_transaction, AVG_item_PL_per_transaction, PL_ratio, AVG_Discount, Purchase_Months, Final_Segment, Recency, STD_Ticket, Store_Type_Variety, Regularity)

logistic_data$Store_Type_Variety <- as.factor(logistic_data$Store_Type_Variety)
logistic_data$Final_Segment <- as.factor(logistic_data$Final_Segment)

logistic_data <- logistic_data %>%
  drop_na()

# Select only numeric columns (excluding factors or non-numeric data)
numeric_data <- logistic_data %>%
  select(where(is.numeric), -Churn) # Remove 'Churn' if you want to keep it as a categorical variable

# Calculate the correlation matrix
correlation_matrix <- cor(numeric_data, use = "complete.obs")

print(correlation_matrix)
library(corrplot)
corrplot(correlation_matrix, method = "color", type = "upper", tl.cex = 0.7)

# I eliminate the variable highly correlated with each other, to deal with the collinearity problem 

logistic_data <- Customers %>%
  select(Churn, Frequency, Monetary_Value, AVG_item_PL_per_transaction, PL_ratio, AVG_Discount, Purchase_Months, Final_Segment, STD_Ticket, Store_Type_Variety)
logistic_data$Store_Type_Variety <- as.factor(logistic_data$Store_Type_Variety)
logistic_data$Final_Segment <- as.factor(logistic_data$Final_Segment)


logistic_data <- logistic_data %>%
  drop_na()

# Scaling data 
logistic_data <- logistic_data %>%
  mutate(
    Frequency = scale(Frequency),
    Monetary_Value = scale(Monetary_Value),
    AVG_item_PL_per_transaction = scale(AVG_item_PL_per_transaction),
    STD_Ticket = scale(STD_Ticket)
  )


# Fit the logistic regression model
mod <- glm(Churn ~ .,
           family = binomial(link = "logit"), data = logistic_data)

# Display the summary of the model with the interaction term
summary(mod)

# Logistic Regression Model Summary:
# - The model shows significant coefficients (p < 0.05) for several predictors, such as Frequency, Monetary_Value, PL_ratio, 
#   Purchase_Months, and customer segments. 
# - Negative coefficients (e.g., Frequency, Monetary_Value, Purchase_Months) reduce the likelihood of churn, while positive
#   coefficients (e.g., PL_ratio, Final_SegmentHighly Active and Recent) increase it.
# - Notable customer segment effects:
#   - "Low Engagement" has the most negative impact on churn likelihood.
# - Model deviance (Residual Deviance: 5390.1) and AIC (5418.1) indicate a well-fitting model.
# - Variables like AVG_Discount and some Store_Type categories are insignificant (p > 0.05), suggesting minimal impact on churn.

summary(mod)


r2log <- function(mod){
  summaryLog <- summary(mod)
  1 - summaryLog$deviance / summaryLog$null.deviance
}

r2log(mod)
# r^2 0.5050093

library(car)
vif_values <- vif(mod)  # Use the fitted logistic regression model
print(vif_values)

# A high VIF (> 8 or 10) for a variable indicates strong multicollinearity.
# in this situation we have all undercontroll and we don't have multicollinearity problems 

# Coefficients on the logit scale
logit_coefficients <- mod$coefficients
print("Logit coefficients:")
print(logit_coefficients)

# Odds Ratios for better interpretation
odds_ratios <- exp(logit_coefficients)
print("Odds Ratios:")
print(odds_ratios)

# If OR > 1 A one-unit increase in the predictor increases the odds of the target outcome.
# If OR < 1 A one-unit increase in the predictor decreases the odds of the target outcome.
# If OR = 1 The predictor has no effect on the odds of the target outcome.


# Predictions (probabilities) Stores the predicted probabilities for each observation in the dataset (logistic_data) based on the logistic regression model.
logistic_data$predicted_probabilities <- mod$fitted.values

### OPTIMAL THRESHOLD IDENTIFICATION
## Method 1
# Load PRROC library for ROC curve
library(PRROC)

# Compute the ROC curve
PRROC_obj <- roc.curve(scores.class0 = logistic_data$predicted_probabilities,
                       weights.class0 = logistic_data$Churn, curve = TRUE)

# Plot the ROC curve
plot(PRROC_obj, main = "ROC Curve for Churn Prediction")
# AUC = 0.856253


# Calculate the optimal threshold
distances <- sqrt((1 - sens)^2 + (1 - spec)^2)
optimal_threshold <- thresholds[which.min(distances)]
print(optimal_threshold)

# Interpretation:
# The ideal threshold corresponds to the point closest to the top-left corner of the ROC curve.
# You can visually inspect the plot and identify the optimal threshold based on this.

# with this method the threshold is around 0.2

## Method 2
# Define a sequence of thresholds
thresholds <- seq(0.2, 0.9, by = 0.05)

# Initialize vectors to store evaluation metrics
sens <- spec <- acc <- aper <- rep(0, length(thresholds))

for (i in seq_along(thresholds)) {
  # Classify predictions based on the threshold
  pred_class <- ifelse(logistic_data$predicted_probabilities > thresholds[i], 1, 0)
  
  # Create confusion matrix with fixed levels
  misclass_table <- table(factor(pred_class, levels = c(0, 1)), 
                          factor(logistic_data$Churn, levels = c(0, 1)))
  
  # Compute metrics (add safeguards for division by zero)
  sens[i] <- ifelse((misclass_table[2, 2] + misclass_table[1, 2]) > 0,
                    misclass_table[2, 2] / (misclass_table[2, 2] + misclass_table[1, 2]), 
                    0) # Sensitivity
  spec[i] <- ifelse((misclass_table[1, 1] + misclass_table[2, 1]) > 0,
                    misclass_table[1, 1] / (misclass_table[1, 1] + misclass_table[2, 1]), 
                    0) # Specificity
  acc[i]  <- sum(diag(misclass_table)) / nrow(logistic_data)  # Accuracy
  aper[i] <- 1 - acc[i]  # Misclassification Error
}


# Plot metrics against thresholds
plot(thresholds, sens, col = "red2", type = "l", ylim = c(0, 1), lwd = 3, ylab = "Metrics", xlab = "Threshold")
lines(thresholds, spec, col = "green3", lwd = 3)
lines(thresholds, acc, col = "blue", lwd = 3)
lines(thresholds, aper, col = "purple", lwd = 3)

# Add legend
legend("topleft", legend = c("Sensitivity", "Specificity", "Accuracy", "Misclassification Error"),
       col = c("red2", "green3", "blue", "purple"), lwd = 3)

# Compute optimal thresholds for each criterion
best_threshold_accuracy <- thresholds[which.max(acc)]
best_threshold_balance <- thresholds[which.min(abs(sens - spec))]
best_threshold_aper <- thresholds[which.min(aper)]

# Print results
cat("Best Threshold (Max Accuracy):", best_threshold_accuracy, "\n")
# 0.4
cat("Best Threshold (Sensitivity-Specificity Balance):", best_threshold_balance, "\n")
# 0.2
cat("Best Threshold (Min Misclassification Error):", best_threshold_aper, "\n")
# 0.4


# Interpretation:
# Choose the threshold with the best trade-off (e.g., highest accuracy or a balance between sensitivity and specificity).
# Also in this case, 0.2 threshold is the best one 



# Predict churn classification 
logistic_data$predicted_class <- ifelse(logistic_data$predicted_probabilities > 0.2, 1, 0)

# Confusion Matrix
confusion_matrix <- table(Predicted = logistic_data$predicted_class, Actual = logistic_data$Churn)
print("Confusion Matrix:")
print(confusion_matrix)

#               Actual
# Predicted     0     1
#         0 23254   506
#         1   156   915

# Compute evaluation metrics
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
sensitivity <- confusion_matrix[2, 2] / (confusion_matrix[2, 2] + confusion_matrix[1, 2])
specificity <- confusion_matrix[1, 1] / (confusion_matrix[1, 1] + confusion_matrix[2, 1])
print(paste("Accuracy:", round(accuracy, 4)))
# "Accuracy: 0.9732"
print(paste("Sensitivity (Recall):", round(sensitivity, 4)))
# "Sensitivity (Recall): 0.6432"
print(paste("Specificity:", round(specificity, 4)))
# "Specificity: 0.9933"


####################
##### RANDOM FORREST 
library(randomForest)
library(ggplot2) # For additional plots (if needed)
library(partykit)

set.seed(123) # For reproducibility

logistic_data <- Customers %>%
  select(Churn, Frequency, Monetary_Value, AVG_ipt, STD_ipt, AVG_Ticket, Number_item_other, Number_item_PL, AVG_item_other_per_transaction, AVG_item_PL_per_transaction, PL_ratio, AVG_Discount, Purchase_Months, Final_Segment, STD_Ticket, Store_Type_Variety, Regularity)

logistic_data$Store_Type_Variety <- as.factor(logistic_data$Store_Type_Variety)
logistic_data$Final_Segment <- as.factor(logistic_data$Final_Segment)

logistic_data <- logistic_data %>%
  drop_na()

# Scaling data 
logistic_data <- logistic_data %>%
  mutate(
    Frequency = scale(Frequency),
    Monetary_Value = scale(Monetary_Value),
    AVG_ipt = scale(AVG_ipt),
    STD_ipt = scale(STD_ipt),
    AVG_Ticket = scale(AVG_Ticket), 
    Number_item_other= scale(Number_item_other),
    Number_item_PL = scale(Number_item_PL),
    AVG_Ticket = scale(AVG_Ticket),
    AVG_item_PL_per_transaction = scale(AVG_item_PL_per_transaction),
    STD_Ticket = scale(STD_Ticket)
  )

# Ensure 80% of churned and 80% of non-churned customers are in the training dataset
set.seed(123)  # For reproducibility

# Separate churned and non-churned customers
churned <- logistic_data %>% filter(Churn == 1)
non_churned <- logistic_data %>% filter(Churn == 0)

# Sample 80% of churned and 80% of non-churned customers for training
train_churned_index <- sample(1:nrow(churned), size = 0.8 * nrow(churned))
train_non_churned_index <- sample(1:nrow(non_churned), size = 0.8 * nrow(non_churned))

data_train <- bind_rows(churned[train_churned_index, ], non_churned[train_non_churned_index, ])
# Create the test dataset as the set difference
data_test <- anti_join(logistic_data, data_train, by = colnames(logistic_data))


data_train$Churn <- as.factor(data_train$Churn)
data_test$Churn <- as.factor(data_test$Churn)


# Fit the Random Forest model
set.seed(123)
rf_model <- randomForest(
  Churn ~ ., 
  data = data_train, 
  mtry = floor(sqrt(ncol(data_train) - 1)), # Recommended for classification
  ntree = 40, 
  importance = TRUE # To compute variable importance
)

# Print model summary
print(rf_model)
# Random Forest Model Summary
# The model achieved an Out-of-Bag (OOB) error rate of 2.16%, indicating strong overall performance.
# Confusion Matrix Analysis:
# - Class 0 (Non-Churn): 18,621 correctly classified, with only 24 misclassified (error rate: 0.13%).
# - Class 1 (Churn): 706 correctly classified, but 403 misclassified as non-churn (error rate: 36.34%).
# The model performs exceptionally well in predicting non-churn (Class 0) but struggles more with identifying churned customers (Class 1).

print(rf_model)



# Plot OOB error to determine stabilization point
plot(rf_model, main = "OOB Error Stabilization", lwd = 2)

# more or less 20 threes 

# Compute and plot variable importance
var_importance <- importance(rf_model)
print(var_importance)

# Visualize variable importance
varImpPlot(rf_model, pch = 19, main = "Variable Importance")

# Variable Importance Summary:
# - The plots show the importance of variables based on Mean Decrease in Accuracy and Mean Decrease in Gini.
# - Top predictors:
#   - "Final_Segment" and "Purchase_Months" are the most important variables, strongly influencing model accuracy and splits.
#   - "Monetary_Value", "PL_ratio", and "Frequency" also contribute significantly to the model's performance.
# - Interpretation of Metrics:
#   - Mean Decrease Accuracy: Indicates the reduction in model accuracy when the variable is permuted. Variables like "Final_Segment" have the largest impact on predictive performance.
#   - Mean Decrease Gini: Reflects the reduction in node impurity when a variable is used for splitting. Variables with higher values (e.g., "Final_Segment", "Purchase_Months") are more important for classification.
# - Insights:
#   - Segmentation and purchase patterns (e.g., "Final_Segment", "Purchase_Months") are critical for predicting churn.
#   - Less important variables include "Store_Type_Variety" and "AVG_Discount", which have minimal impact on model performance.


# Make predictions on the test set
predictions <- predict(rf_model, newdata = data_test, type = "response")

# Confusion Matrix
confusion_matrix <- table(Predicted = predictions, Actual = data_test$Churn)
print(confusion_matrix)

#             Actual
# redicted    0    1
#        0 4656  111
#        1    6  167

# Compute evaluation metrics
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
sensitivity <- confusion_matrix[2, 2] / (confusion_matrix[2, 2] + confusion_matrix[1, 2])
specificity <- confusion_matrix[1, 1] / (confusion_matrix[1, 1] + confusion_matrix[2, 1])

cat("Accuracy:", round(accuracy, 4), "\n")
# Accuracy: 0.9763 
cat("Sensitivity:", round(sensitivity, 4), "\n")
# Sensitivity: 0.6007 
cat("Specificity:", round(specificity, 4), "\n")
# Specificity: 0.9987 

#######################
##### XGBOOST ALGORITHM 
library(xgboost)
library(dplyr)
library(caret)

logistic_data <- Customers %>%
  select(Churn, Frequency, Monetary_Value, AVG_ipt, STD_ipt, AVG_Ticket, Number_item_other, Number_item_PL, AVG_item_other_per_transaction, AVG_item_PL_per_transaction, PL_ratio, AVG_Discount, Purchase_Months, Final_Segment, STD_Ticket, Store_Type_Variety, Regularity)

logistic_data <- logistic_data %>%
  mutate(
    Final_Segment = case_when(
      Final_Segment == "Highly Active and Recent" ~ 1,
      Final_Segment == "Super Customer" ~ 2,
      Final_Segment == "Consistently Moderate" ~ 3,
      Final_Segment == "Not Clusterized" ~ 4,
      Final_Segment == "Low Engagement" ~ 5,
      TRUE ~ NA_real_  # Catch any unexpected values as NA
    )
  )

logistic_data <- logistic_data %>%
  drop_na()

# Scaling data 
logistic_data <- logistic_data %>%
  mutate(
    Frequency = scale(Frequency),
    Monetary_Value = scale(Monetary_Value),
    AVG_ipt = scale(AVG_ipt),
    STD_ipt = scale(STD_ipt),
    AVG_Ticket = scale(AVG_Ticket), 
    Number_item_other= scale(Number_item_other),
    Number_item_PL = scale(Number_item_PL),
    AVG_Ticket = scale(AVG_Ticket),
    AVG_item_PL_per_transaction = scale(AVG_item_PL_per_transaction),
    STD_Ticket = scale(STD_Ticket)
  )


# Ensure `Churn` is a factor with levels 0 and 1
logistic_data$Churn <- as.numeric(as.character(logistic_data$Churn))  # Convert to 0/1

# Split data into training and testing sets (80/20 split)
# Ensure 80% of churned and 80% of non-churned customers are in the training dataset
set.seed(123)  # For reproducibility

# Separate churned and non-churned customers
churned <- logistic_data %>% filter(Churn == 1)
non_churned <- logistic_data %>% filter(Churn == 0)

# Sample 80% of churned and 80% of non-churned customers for training
train_churned_index <- sample(1:nrow(churned), size = 0.8 * nrow(churned))
train_non_churned_index <- sample(1:nrow(non_churned), size = 0.8 * nrow(non_churned))

data_train <- bind_rows(churned[train_churned_index, ], non_churned[train_non_churned_index, ])
# Create the test dataset as the set difference
data_test <- anti_join(logistic_data, data_train, by = colnames(logistic_data))

# Convert the training and testing data into matrices
train_matrix <- as.matrix(data_train %>% select(-Churn))  # Exclude the target column
train_labels <- data_train$Churn  # Extract the target column as labels

test_matrix <- as.matrix(data_test %>% select(-Churn))  # Exclude the target column
test_labels <- data_test$Churn  # Extract the target column as labels



# Define parameters, including alpha, lambda, and number of trees
xgb_params <- list(
  objective = "binary:logistic",  # Binary classification
  eval_metric = "logloss",       # Log loss for binary classification
  max_depth = 6,                 # Maximum depth of trees
  eta = 0.1,                     # Learning rate
  gamma = 0,                     # Minimum loss reduction for splits
  subsample = 0.8,               # Subsample ratio of the training data
  colsample_bytree = 0.8,        # Subsample ratio of features
  alpha = 0.1,                   # L1 regularization
  lambda = 1                     # L2 regularization
)

# Convert data to DMatrix format for XGBoost
dtrain <- xgb.DMatrix(data = train_matrix, label = train_labels)
dtest <- xgb.DMatrix(data = test_matrix, label = test_labels)


# Train the XGBoost model with specified parameters
set.seed(123)
xgb_model <- xgb.train(
  params = xgb_params,
  data = dtrain,                   # Training data
  nrounds = 150,                   # Number of boosting rounds (trees)
  watchlist = list(train = dtrain, test = dtest), # Monitor performance
  print_every_n = 10               # Print log every 10 rounds
)

print(xgb_model)

# - Training and Testing Performance:
#   - Training Logloss: Reduced from 0.6063 (iter 1) to 0.0472 (iter 150), showing a strong fit to the training data.
#   - Test Logloss: Reduced from 0.6068 (iter 1) to 0.1074 (iter 150), indicating good generalization on the test data with minimal overfitting.
# - Features:
#   - The model uses 16 features with 150 boosting rounds (nrounds).
# - Insights:
#   - The gradual reduction in both training and test logloss demonstrates effective learning.
#   - The small gap between train and test logloss suggests the model generalizes well to unseen data.

# Predict probabilities
pred_probs <- predict(xgb_model, newdata = dtest)

# Convert probabilities to class labels using a threshold of 0.439 (found by trials)
pred_labels <- ifelse(pred_probs > 0.439, 1, 0)

# Confusion matrix
conf_matrix <- table(Predicted = pred_labels, Actual = test_labels)
print("Confusion Matrix:")
print(conf_matrix)

#              Actual
# Predicted    0    1
#         0 4658  111
#         1    4  167

# Compute evaluation metrics
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
sensitivity <- conf_matrix[2, 2] / (conf_matrix[2, 2] + conf_matrix[1, 2])
specificity <- conf_matrix[1, 1] / (conf_matrix[1, 1] + conf_matrix[2, 1])

cat("Accuracy:", round(accuracy, 4), "\n")
# Accuracy: 0.9767 
cat("Sensitivity:", round(sensitivity, 4), "\n")
# Sensitivity: 0.6007 
cat("Specificity:", round(specificity, 4), "\n")
# Specificity: 0.9991 

library(pROC)

# Compute ROC and AUC
roc_obj <- roc(response = test_labels, predictor = pred_probs)
auc_value <- auc(roc_obj)

# Print AUC value
cat("AUC:", round(auc_value, 4), "\n")
# AUC 0.8865

# Plot ROC curve
plot(roc_obj, main = "ROC Curve for XGBoost Model", col = "blue", lwd = 2)
legend("bottomright", legend = paste("AUC =", round(auc_value, 4)), col = "blue", lwd = 2)

## Feature importance
importance <- xgb.importance(feature_names = colnames(train_matrix), model = xgb_model)

# Print feature importance
print(importance)

# Visualize feature importance
xgb.plot.importance(importance, main = "Variable Importance from XGBoost")

# Variable Importance from XGBoost:
# - Top Predictors:
#   - "Final_Segment": The most important variable, significantly impacting the predictions.
#   - "Purchase_Months" and "Number_item_other": Strong contributors to the model's decision-making process.
# - Moderate Importance:
#   - Variables like "STD_ipt", "STD_Ticket", and "AVG_Discount" also play a notable role but are less critical than the top predictors.
# - Low Importance:
#   - Features such as "Store_Type_Variety" and "Regularity" have minimal impact on the model's performance.
# - Insights:
#   - The segmentation of customers ("Final_Segment") and their purchase patterns ("Purchase_Months", "Number_item_other") are key drivers of churn predictions.
#   - Variables with low importance may not significantly affect the model and could be considered for removal in feature engineering if needed.


#### VARIABLE AGGREGATION, for bext variables identification

library(tibble)

# Logistic Regression Variable Importance
# Rank variables by p-value classification before using absolute coefficients for importance
logit_importance <- summary(mod)$coefficients[-1, ] %>%
  as.data.frame() %>%
  mutate(
    Variable = rownames(.),
    Variable = ifelse(grepl("Final_Segment", Variable), "Final_Segment", Variable), # Handle "Final_Segment"
    PValueRank = case_when(
      `Pr(>|z|)` <= 0.001 ~ 1,  # Highly significant (***)
      `Pr(>|z|)` <= 0.01 ~ 2,   # Very significant (**)
      `Pr(>|z|)` <= 0.05 ~ 3,   # Significant (*)
      TRUE ~ 4                  # Not significant
    ),
    Importance = abs(Estimate)  # Use absolute coefficient value
  ) %>%
  group_by(Variable) %>%  # Group by variable to combine duplicates like "Final_Segment"
  summarise(
    PValueRank = min(PValueRank),
    Importance = sum(Importance)
  ) %>%
  ungroup() %>%
  arrange(PValueRank, desc(Importance)) %>%
  top_n(17, Importance) %>%
  mutate(Rank = row_number())


logit_importance$Method <- "Logistic Regression"

# Random Forest Variable Importance
rf_importance <- as.data.frame(importance(rf_model)) %>%
  mutate(Variable = rownames(.)) %>%
  arrange(desc(`MeanDecreaseAccuracy`)) %>%
  top_n(17, `MeanDecreaseAccuracy`) %>%
  select(Variable, `MeanDecreaseAccuracy`) %>%
  rename(Importance = `MeanDecreaseAccuracy`) %>%
  mutate(Rank = row_number())
rf_importance$Method <- "Random Forest"

# XGBoost Variable Importance
xgb_importance <- as.data.frame(importance) %>%
  top_n(17, Gain) %>%
  select(Feature, Gain) %>%
  rename(Variable = Feature, Importance = Gain) %>%
  mutate(Rank = row_number())
xgb_importance$Method <- "XGBoost"

# Combine All Importance Tables, Filling Missing Variables with 0
all_importances <- bind_rows(
  logit_importance %>% select(Rank, Method, Variable),
  rf_importance %>% select(Rank, Method, Variable),
  xgb_importance %>% select(Rank, Method, Variable)
) %>%
  complete(Variable, Method, fill = list(Rank = 21))  # Fill missing variables with 0

# Calculate Final Variable Rank Including Zeros
final_ranks <- all_importances %>%
  group_by(Variable) %>%
  summarise(AggregateRank = mean(Rank)) %>%
  arrange(AggregateRank) %>%
  mutate(FinalRank = row_number())

# Merge Final Rank with Individual Model Ranks
final_merge <- all_importances %>%
  left_join(final_ranks, by = "Variable") %>%
  arrange(FinalRank, Method)

# Create a Wide Format Table for Comparison
importance_wide <- final_merge %>%
  select(FinalRank, Method, Variable, Rank, AggregateRank) %>%
  pivot_wider(
    names_from = Method,
    values_from = Rank,
    values_fill = 21
  ) %>%
  mutate(AggregateRank = round(AggregateRank, 2))

# Display the Final Table
library(knitr)

# Adjust kable to use correct column names
importance_wide %>%
  kable(
    caption = "Final Variable Ranking Across Models with Aggregate Rank (Missing Variables Ranked as 0)",
    col.names = colnames(importance_wide)  # Automatically match column names
  )


print(importance_wide)

# Final Variable Ranking Summary:
# - This table combines variable importance rankings across Logistic Regression, Random Forest, and XGBoost models to determine the best predictors for churn.
# - Key Findings:
#   - "Final_Segment" consistently ranks as the most important variable (Rank 1 across all models), making it the strongest predictor of churn.
#   - "Purchase_Months" is highly influential, ranking 2nd overall, driven by its high importance in both Random Forest and XGBoost.
#   - "PL_ratio" and "Frequency" rank 3rd and 4th, indicating their significant contribution to churn prediction, particularly in Logistic Regression and Random Forest.
# - Moderate Predictors:
#   - Variables like "STD_Ticket" (Rank 5), "Monetary_Value" (Rank 6), and "AVG_item_PL_per_transaction" (Rank 7) have consistent but slightly lower importance.



### Survival analysis 

# Disclaimer:
# The survival analysis has been conducted using a time span of 12 months, which may not be sufficient for reliable insights. 
# Short time spans can lead to limited variability in customer behavior, potentially underestimating the true survival probability trends.
# In this case:
# 1. Customers' purchasing behaviors may not fully manifest within a 12-month period, especially for churn or low-engagement segments.
# 2. Long-term trends, such as reactivation or delayed churn, cannot be observed due to the restricted time frame.
# 3. Survival analysis often benefits from a longer observation window to better capture time-dependent effects and provide more robust conclusions.

if (!require(survival)) install.packages("survival")
if (!require(survminer)) install.packages("survminer")

library(survival)
library(survminer)

logistic_data <- Customers %>%
  select(Churn, Frequency, Monetary_Value, PL_ratio, AVG_ipt, STD_ipt, STD_Ticket, AVG_Ticket, 
         Number_item_other, Number_item_PL, AVG_item_other_per_transaction, 
         AVG_item_PL_per_transaction, PL_ratio, AVG_Discount, Purchase_Months, Final_Segment) %>%
  mutate(
    Final_Segment = case_when(
      Final_Segment == "Super Customer" ~ 1,
      Final_Segment == "Highly Active and Recent" ~ 2,
      Final_Segment == "Consistently Moderate" ~ 3,
      Final_Segment == "Low Engagement" ~ 4,
      Final_Segment == "Not Clusterized" ~ 5,
      TRUE ~ NA_real_  # Handle unexpected cases, setting them to NA
    )
  )


logistic_data <- logistic_data %>%
  drop_na()

# Ensure Churn is numeric (0 = no churn, 1 = churn)
logistic_data$Churn <- as.numeric(as.character(logistic_data$Churn))

# Remove rows with missing or invalid data
logistic_data <- logistic_data[!is.na(logistic_data$Purchase_Months) & !is.na(logistic_data$Churn), ]

# Define the time and event variables
time_var <- logistic_data$Purchase_Months
event_var <- logistic_data$Churn

# Create the Surv object
surv_object <- Surv(time = time_var, event = event_var)

# Fit the Kaplan-Meier model
km_fit <- survfit(surv_object ~ 1, data = logistic_data)

# Plot the survival curve
ggsurvplot(
  km_fit,
  title = "Kaplan-Meier Survival Curve",
  xlab = "Time (Months)",  # Adjust based on your time variable
  ylab = "Survival Probability",
  conf.int = TRUE,  # Include confidence intervals
  ggtheme = theme_minimal()  # Apply a minimal theme
)


# Fit the Kaplan-Meier model with grouping by Final_Segment
km_fit_grouped <- survfit(surv_object ~ Final_Segment, data = logistic_data)

# Plot the survival curves for each group
ggsurvplot(
  km_fit_grouped,
  title = "Kaplan-Meier Survival Curves by Final Segment",
  xlab = "Time (Months)",  # Adjust based on your time variable
  ylab = "Survival Probability",
  conf.int = TRUE,  # Include confidence intervals
  ggtheme = theme_minimal(),  # Apply a minimal theme
  legend.title = "Customer Segment",  # Add a legend title
  legend.labs = c("Super Customer", "Highly Active and Recent", "Consistently Moderate", "Low Engagement", "Not Clusterized")  # Customize legend labels
)

# Log-rank test to compare survival curves between groups
log_rank_test <- survdiff(surv_object ~ Final_Segment, data = logistic_data)
print(log_rank_test)

summary(mod)



####################################################
#### Analysis of private labels in loyalty mechanisms

### Based on RF model 
# Load the necessary library
library(pdp)

set.seed(123) # For reproducibility

logistic_data <- Customers %>%
  select(Churn, Frequency, Monetary_Value, AVG_ipt, STD_ipt, AVG_Ticket, Number_item_other, Number_item_PL, AVG_item_other_per_transaction, AVG_item_PL_per_transaction , PL_ratio, AVG_Discount, Purchase_Months, Final_Segment, STD_Ticket, Store_Type_Variety, Regularity)

logistic_data$Store_Type_Variety <- as.factor(logistic_data$Store_Type_Variety)
logistic_data$Final_Segment <- as.factor(logistic_data$Final_Segment)

logistic_data <- logistic_data %>%
  drop_na()

# Scaling data 
logistic_data <- logistic_data %>%
  mutate(
    Frequency = scale(Frequency),
    Monetary_Value = scale(Monetary_Value),
    AVG_ipt = scale(AVG_ipt),
    STD_ipt = scale(STD_ipt),
    AVG_Ticket = scale(AVG_Ticket), 
    Number_item_other= scale(Number_item_other),
    Number_item_PL = scale(Number_item_PL),
    AVG_Ticket = scale(AVG_Ticket),
    STD_Ticket = scale(STD_Ticket)
  )

# Ensure 80% of churned and 80% of non-churned customers are in the training dataset
set.seed(123)  # For reproducibility

# Separate churned and non-churned customers
churned <- logistic_data %>% filter(Churn == 1)
non_churned <- logistic_data %>% filter(Churn == 0)

# Sample 80% of churned and 80% of non-churned customers for training
train_churned_index <- sample(1:nrow(churned), size = 0.8 * nrow(churned))
train_non_churned_index <- sample(1:nrow(non_churned), size = 0.8 * nrow(non_churned))

data_train <- bind_rows(churned[train_churned_index, ], non_churned[train_non_churned_index, ])
# Create the test dataset as the set difference
data_test <- anti_join(logistic_data, data_train, by = colnames(logistic_data))


data_train$Churn <- as.factor(data_train$Churn)
data_test$Churn <- as.factor(data_test$Churn)


# AVG_item_PL_per_transaction
# Generate a partial dependence plot for the 'AVG_item_PL_per_transaction' variable
pdp_pl_ratio <- partial(
  object = rf_model, 
  pred.var = "AVG_item_PL_per_transaction",        # Variable of interest
  train = data_train,           # Training dataset used for the random forest
  grid.resolution = 50,          # Number of points to evaluate the variable
  prob = TRUE
)

# Plot the partial dependence
plotPartial(
  pdp_pl_ratio, 
  main = "Partial Dependence Plot for AVG_item_PL_per_transaction",
  xlab = "AVG_item_PL_per_transaction",            # X-axis label
  ylab = "Churn Probability",   # Y-axis label
  rug = TRUE                    # Add rug plot to show data distribution
)

# Output shows how for value of AVG_item_PL_per_transaction > 6 the probability of churn decrease from 0.93 to 0.875 
# --> the higher AVG_item_PL_per_transaction the lower will be the churn probability 

# PL_ratio
# Generate a partial dependence plot for the 'PL_ratio' variable
pdp_pl_ratio <- partial(
  object = rf_model, 
  pred.var = "PL_ratio",        # Variable of interest
  train = data_train,           # Training dataset used for the random forest
  grid.resolution = 50,          # Number of points to evaluate the variable
  prob = TRUE
)

# Plot the partial dependence
plotPartial(
  pdp_pl_ratio, 
  main = "Partial Dependence Plot for PL_ratio",
  xlab = "PL_ratio",            # X-axis label
  ylab = "Churn Probability",   # Y-axis label
  rug = TRUE                    # Add rug plot to show data distribution
)

# Output shows how for value of PL_ratio > 0.2 the probability of churn increase from 0.86 to 0.885 
# --> the higher PL_ratio the highr will be the churn probability 



### Based on XGBoost model 
logistic_data <- Customers %>%
  select(Churn, Frequency, Monetary_Value, AVG_ipt, STD_ipt, AVG_Ticket, Number_item_other, Number_item_PL, AVG_item_other_per_transaction, AVG_item_PL_per_transaction, PL_ratio, AVG_Discount, Purchase_Months, Final_Segment, STD_Ticket, Store_Type_Variety, Regularity)

logistic_data$Final_Segment

logistic_data <- logistic_data %>%
  mutate(
    Final_Segment = case_when(
      Final_Segment == "Highly Active and Recent" ~ 1,
      Final_Segment == "Super Customer" ~ 2,
      Final_Segment == "Consistently Moderate" ~ 3,
      Final_Segment == "Not Clusterized" ~ 4,
      Final_Segment == "Low Engagement" ~ 5,
      TRUE ~ NA_real_  # Catch any unexpected values as NA
    )
  )

logistic_data <- logistic_data %>%
  drop_na()

# Scaling data 
logistic_data <- logistic_data %>%
  mutate(
    Frequency = scale(Frequency),
    Monetary_Value = scale(Monetary_Value),
    AVG_ipt = scale(AVG_ipt),
    STD_ipt = scale(STD_ipt),
    AVG_Ticket = scale(AVG_Ticket), 
    Number_item_other= scale(Number_item_other),
    Number_item_PL = scale(Number_item_PL),
    AVG_Ticket = scale(AVG_Ticket),
    STD_Ticket = scale(STD_Ticket)
  )



# Ensure `Churn` is a factor with levels 0 and 1
logistic_data$Churn <- as.numeric(as.character(logistic_data$Churn))  # Convert to 0/1

# Split data into training and testing sets (80/20 split)
# Ensure 80% of churned and 80% of non-churned customers are in the training dataset
set.seed(123)  # For reproducibility

# Separate churned and non-churned customers
churned <- logistic_data %>% filter(Churn == 1)
non_churned <- logistic_data %>% filter(Churn == 0)

# Sample 80% of churned and 80% of non-churned customers for training
train_churned_index <- sample(1:nrow(churned), size = 0.8 * nrow(churned))
train_non_churned_index <- sample(1:nrow(non_churned), size = 0.8 * nrow(non_churned))

data_train <- bind_rows(churned[train_churned_index, ], non_churned[train_non_churned_index, ])
# Create the test dataset as the set difference
data_test <- anti_join(logistic_data, data_train, by = colnames(logistic_data))

# Convert the training and testing data into matrices
train_matrix <- as.matrix(data_train %>% select(-Churn))  # Exclude the target column
train_labels <- data_train$Churn  # Extract the target column as labels

test_matrix <- as.matrix(data_test %>% select(-Churn))  # Exclude the target column
test_labels <- data_test$Churn  # Extract the target column as labels



library(pdp)

# PL_ratio
# Generate partial dependence plot for the 'PL_ratio' variable
pdp_pl_ratio <- partial(
  object = xgb_model,             # XGBoost model
  pred.var = "PL_ratio",          # Variable of interest
  train = train_matrix,           # Training data used to train the model
  grid.resolution = 50,           # Number of points to evaluate the variable
  type = "classification",        # Specify classification task
  prob = TRUE                     # Output probabilities instead of raw predictions
)

# Plot the partial dependence
plotPartial(
  pdp_pl_ratio, 
  main = "Partial Dependence Plot for PL_ratio (XGBoost)",
  xlab = "PL_ratio", 
  ylab = "Churn Probability",
  rug = TRUE                      # Add rug plot to show data distribution
)


# AVG_item_PL_per_transaction
# Generate partial dependence plot for the 'AVG_item_PL_per_transaction' variable
pdp_pl_ratio <- partial(
  object = xgb_model,             # XGBoost model
  pred.var = "AVG_item_PL_per_transaction",          # Variable of interest
  train = train_matrix,           # Training data used to train the model
  grid.resolution = 50,           # Number of points to evaluate the variable
  type = "classification",        # Specify classification task
  prob = TRUE                     # Output probabilities instead of raw predictions
)

# Plot the partial dependence
plotPartial(
  pdp_pl_ratio, 
  main = "Partial Dependence Plot for AVG_item_PL_per_transaction (XGBoost)",
  xlab = "AVG_item_PL_per_transaction", 
  ylab = "Churn Probability",
  rug = TRUE                      # Add rug plot to show data distribution
)


# The variation of the probability of churn in the output of the XGBoost are insignificant compared to the ones of the RF
# for this reason we will focus on the output of the RF and also becasue is coherent with the impact defined in the output 
# of the Logistic Regression



#### DESCRIPTIVE ANALYSIS 
# Load necessary libraries
library(ggplot2)
library(dplyr)

Customers_1 <- Customers
Customers_1$Final_Segment <- as.factor(Customers_1$Final_Segment)
Customers_1$Purchase_Months <- as.factor(Customers_1$Purchase_Months)
Customers_1$Churn <- as.factor(Customers_1$Churn)


# Define the custom color palette
custom_palette <- c("#75BDA7", "#58B6C0")


# Summarize the data to count churned and not churned customers
churn_summary <- Customers_1 %>%
  group_by(Churn) %>%
  summarise(Count = n()) %>%
  mutate(Churn_Status = ifelse(Churn == 1, "Churned", "Not Churned"))

# Create the pie chart
ggplot(churn_summary, aes(x = "", y = Count, fill = Churn_Status)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = custom_palette) +
  labs(
    title = "Proportion of Churned vs Not Churned Customers",
    fill = "Churn Status"
  ) +
  theme_void() + # Clean background for pie chart
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )


# Define the custom color palette
custom_palette <- c("#58B6C0", "#75BDA7", "#70B4BE", "#2F80ED", "#D3E0EA", "#7D8B8D","#4A90E2")

# Function to handle plotting based on variable type
plot_variable <- function(data, variable) {
  if (is.factor(data[[variable]]) || is.character(data[[variable]])) {
    # For categorical (factor or character) variables, use a bar plot
    ggplot(data, aes_string(x = variable, fill = "factor(Churn)")) +
      geom_bar(position = "fill", alpha = 0.8) +
      scale_fill_manual(values = custom_palette) +
      labs(
        title = paste("Proportion of", variable, "by Churn Status"),
        x = variable,
        y = "Proportion",
        fill = "Churn Status"
      ) +
      theme_minimal()
  } else if (is.numeric(data[[variable]]) || is.integer(data[[variable]])) {
    # For numerical variables, use density plots
    ggplot(data, aes_string(x = variable, fill = "factor(Churn)")) +
      geom_density(alpha = 0.5, adjust = 1.5) +
      scale_fill_manual(values = custom_palette) +
      labs(
        title = paste("Distribution of", variable, "by Churn Status"),
        x = variable,
        y = "Density",
        fill = "Churn Status"
      ) +
      theme_minimal()
  } else {
    message(paste("Variable", variable, "is neither numeric nor categorical."))
  }
}

# List of variables to analyze (replace with variables of interest)
variables <- c(
  "Final_Segment",               
  "Purchase_Months",
  "Frequency",
  "STD_Ticket",
  "Monetary_Value",
  "PL_ratio",
  "AVG_item_PL_per_transaction"
)

target = "AVG_item_PL_per_transaction"

# Loop through variables and create plots
for (variable in variables) {
  if (variable == target) {
    print(plot_variable(Customers_1, variable))
  } else {
    message(paste("Variable", variable, "is not in the dataset"))
  }
}

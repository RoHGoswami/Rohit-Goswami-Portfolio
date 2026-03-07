# Load necessary libraries
library(tidyverse)
library(lme4)
library(lmtest)
library(performance)
library(ggplot2)
library(nortest)
library(car)

data = read.csv("Student_data_high.csv", header = TRUE)
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

# Ensure Peer_Influence is treated as a factor
if ("Peer_Influence" %in% colnames(encoded_data)) {
  encoded_data$Peer_Influence <- as.factor(encoded_data$Peer_Influence)
}


# Step 2: Log transformation of Exam Scores
encoded_data$Log_Exam_Score <- log(encoded_data$Exam_Score + 1)

# Load necessary libraries
library(lme4)

# Reduced Model: Fixed Effects Only (lm)
Reduced_model <- lm(Log_Exam_Score ~ Attendance + Hours_Studied + Motivation_Level 
                    + Internet_Access + Access_to_Resources +
                      + Sleep_Hours + Tutoring_Sessions 
                    + Previous_Scores 
                    + Gender + Parental_Involvement  
                    + Extracurricular_Activities + Peer_Influence +
                      + Teacher_Quality*School_Type 
                    + Teacher_Quality*Learning_Disabilities
                    + Learning_Disabilities*Distance_from_Home
                    + Physical_Activity*Learning_Disabilities,
                    data = encoded_data)

# Full Model: Fixed + Random Effects (lmer)
log_model <- lmer(Log_Exam_Score ~ Attendance + Hours_Studied + Motivation_Level 
                  + Internet_Access + Access_to_Resources +
                    + Sleep_Hours + Tutoring_Sessions 
                  + Previous_Scores 
                  + Gender + Parental_Involvement  
                  + Extracurricular_Activities + Peer_Influence +
                    + Teacher_Quality*School_Type 
                  + Teacher_Quality*Learning_Disabilities
                  + Learning_Disabilities*Distance_from_Home
                  + Physical_Activity*Learning_Disabilities
                  + (1 | Parental_Education_Level), 
                  data = encoded_data, REML = FALSE)

# Likelihood Ratio Test
lrt_result <- anova(log_model, Reduced_model, test = "Chisq")
print(lrt_result)

# Step 5: Residual Analysis
residuals_full <- residuals(log_model)
fitted_values <- fitted(log_model)

# Q-Q Plot
qq_plot <- ggplot(data = data.frame(residuals = residuals_full), aes(sample = residuals)) +
  stat_qq() + stat_qq_line(color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(title = "Q-Q Plot of Residuals", x = "Theoretical Quantiles", y = "Sample Quantiles")
print(qq_plot)


# Breusch-Pagan Test
bp_test <- bptest(residuals_full ~ fitted_values)
print(bp_test)

# Step 6: Scale-Location Plot
scale_loc_plot <- ggplot(data = data.frame(Fitted = fitted_values, Std_Residuals = sqrt(abs(residuals_full))),
                         aes(x = Fitted, y = Std_Residuals)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "loess", color = "red", linewidth = 1, se = FALSE) +
  theme_minimal() +
  labs(title = "Scale-Location Plot", x = "Fitted Values", y = "√|Standardized Residuals|")
print(scale_loc_plot)

# Step 7: Random Effects Validation
random_effects_var <- r2_nakagawa(log_model)
print(random_effects_var)

library(lme4)

# Step 8: Backward Feature Selection
while (TRUE) {
  step_result <- drop1(log_model, test = "Chisq")
  print(step_result)
  
  # Find the maximum p-value
  max_p <- max(step_result$`Pr(>Chi)`, na.rm = TRUE)
  
  # Stop if all remaining variables are significant
  if (max_p < 0.05) break
  
  # Get the variable with the highest p-value
  remove_var <- rownames(step_result)[which.max(step_result$`Pr(>Chi)`)]
  
  # Prevent removal of intercept or invalid variable names
  if (remove_var == "(Intercept)") break
  
  # Update formula
  formula_new <- as.formula(paste(deparse(formula(log_model)), "- ", remove_var))
  
  # Refit model
  log_model <- lmer(formula_new, data = data, REML = FALSE)
}



# Load necessary library
library(ggplot2)

# Extract fitted values and residuals
fitted_values <- fitted(log_model)
residuals_values <- residuals(log_model)

# Create a dataframe for plotting
residuals_data <- data.frame(Fitted = fitted_values, Residuals = residuals_values)

# Generate the scatter plot without the red horizontal line
ggplot(residuals_data, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 3, color = "blue") +  # Scatter points
  geom_smooth(method = "loess", color = "black", linewidth = 1, se = FALSE) +  # Trend line
  theme_minimal() +
  labs(title = "Fitted vs Residuals Plot", x = "Fitted Values", y = "Residuals")

summary(log_model)

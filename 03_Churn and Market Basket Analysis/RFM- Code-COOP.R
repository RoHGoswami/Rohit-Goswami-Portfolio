library(dplyr)
library(rfm)
library(patchwork)
library(readr)
library(lubridate)  # For date formatting

data_B = read.csv("Dataset_CRM_TypeB.csv", header = TRUE)
str(data_B)

# PL_gross_sales --> value of Private label sale 
# net_sales --> (gross sales) - (advertisement)
# gross_sales --> gross value of the sale 




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

# Check the updated data
head(data_B)


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

# Display the anomalies summary table
print(anomalies_summary_B)

## There is a problem in this situation: "PL_gross_sales > net_sales"
## but for the RFM we will take into account the gross sales in total, so no prob :) 

netsales_anomaly <- data_B %>%
  filter(net_sales > gross_sales)

data_B <- data_B %>%
  filter(net_sales < gross_sales)

# Eliminate the net_sales > gross_sales 

reference_date <- max(data_B$date)
First_purchase <- min(data_B$date)


# Perform RFM analysis
Customers <- data_B %>%
  group_by(id_customer) %>%  # Group by customer ID
  summarize(
    # Calculate the total monetary value of purchases made by the customer
    Monetary_Value = sum(gross_sales, na.rm = TRUE),
    
    # Calculate frequency as the number of distinct purchase dates
    Frequency = n_distinct(date),
    
    # Calculate recency as the number of days since the customer's last purchase to the reference date
    Recency = as.numeric(difftime(reference_date, max(date), units = "days")),
    
    # Record the date of the first purchase by the customer
    First_Purchase = min(as.Date(date)),
    
    # Record the date of the last purchase by the customer
    Last_Purchase = max(as.Date(date)),
    
    # Calculate the average inter-purchase time if the customer made more than one purchase
    AVG_ipt = ifelse(Frequency > 1, 
                     as.numeric(difftime(max(as.Date(date)), min(as.Date(date)), units = "days")) / (Frequency - 1), 
                     NA),
    
    # Calculate the standard deviation of inter-purchase time if the customer made more than one purchase
    STD_ipt = ifelse(Frequency > 1, 
                     sd(diff(as.numeric(sort(unique(as.Date(date)))))), 
                     NA),
    
    # Calculate the average ticket value (monetary value per invoice)
    AVG_Ticket = mean(gross_sales, na.rm = TRUE),
    
    # Calculate the standard deviation of ticket value
    STD_Ticket = sd(gross_sales, na.rm = TRUE),
    
    # Sum up the total number of items bought by the customer
    Number_item_other = sum(number_items_other, na.rm = TRUE),
    Number_item_PL = sum(number_item_PL, na.rm = TRUE),
    
    # Drop grouping after summarizing
    .groups = 'drop'
  )

# Display the first few rows of the summarized RFM dataset
head(Customers)


# Step 1: Analyze Recency to Determine Customer Status

# Summarize the Recency variable to get an overview of its distribution
summary(Customers$Recency)

# Calculate the standard deviation of the Recency variable
sd(Customers$Recency)

# Calculate the 25th and 80th percentiles of the Recency variable
recency_quantiles <- quantile(Customers$Recency, probs = c(0.25, 0.8), na.rm = TRUE)

# Step 2: Visualize the Recency Distribution

# Load ggplot2 for data visualization
library(ggplot2)

# Plot a histogram of the Recency variable with vertical lines for the 25th and 80th percentiles
ggplot(Customers, aes(x = Recency)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +  # Histogram of Recency
  geom_vline(xintercept = recency_quantiles[1], color = "red", linetype = "dashed", size = 1) + # Add a line for the 25th percentile
  geom_vline(xintercept = recency_quantiles[2], color = "green", linetype = "dashed", size = 1) + # Add a line for the 75th percentile
  labs(
    title = "Distribution of Recency",  # Add a title to the plot
    x = "Recency (days)",               # X-axis label
    y = "Frequency"                     # Y-axis label
  ) +
  # Annotate the percentiles with labels
  annotate("text", x = recency_quantiles[1] + 1, y = 30, label = "25th %tile", color = "red", angle = 90, hjust = -0.1) +
  annotate("text", x = recency_quantiles[2] + 1, y = 30, label = "80th %tile", color = "green", angle = 90, hjust = -0.1) +
  # Use a minimal theme for better readability
  theme_minimal()


# Calculate the IQR (Interquartile Range)
IQR_recency <- recency_quantiles[2] - recency_quantiles[1]

# Function to calculate outliers for different multipliers
identify_outliers <- function(multiplier) {
  upper_bound <- recency_quantiles[2] + multiplier * IQR_recency
  outliers <- Customers %>%
    filter(Recency > upper_bound)
  return(nrow(outliers))
}

# Test different multipliers
multipliers <- seq(1, 3, by = 0.5)
outliers_count <- sapply(multipliers, identify_outliers)

# Create a dataframe to compare results
outlier_analysis <- data.frame(
  Multiplier = multipliers,
  Outliers_Count = outliers_count
)

# Calculate the percentage of outliers for different multipliers
outlier_analysis <- data.frame(
  Multiplier = multipliers,
  Outliers_Count = outliers_count,
  Outlier_Percentage = (outliers_count / nrow(Customers)) * 100
)

# Plot the Elbow Method for identifying the optimal multiplier
ggplot(outlier_analysis, aes(x = Multiplier, y = Outliers_Count)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(
    title = "Elbow Method to Identify Optimal Multiplier",
    x = "Multiplier",
    y = "Number of Outliers"
  ) +
  theme_minimal()

# let's keep the multiplier to 2 


# Display the outlier analysis table
print(outlier_analysis)

# Step 2: Visualize the Recency Distribution

# Load ggplot2 for data visualization
library(ggplot2)

# Plot a histogram of the Recency variable with vertical lines for the 25th and 75th percentiles
# and lines for the IQR outlier bounds (for multiplier = 1.5)
upper_bound <- recency_quantiles[2] + 2 * IQR_recency

ggplot(Customers, aes(x = Recency)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +  # Histogram of Recency
  geom_vline(xintercept = recency_quantiles[1], color = "red", linetype = "dashed", size = 1) + # Add a line for the 25th percentile
  geom_vline(xintercept = recency_quantiles[2], color = "green", linetype = "dashed", size = 1) + # Add a line for the 75th percentile
  geom_vline(xintercept = upper_bound, color = "purple", linetype = "dotted", size = 1) + # Add a line for the upper bound of IQR (multiplier = 1.5)
  labs(
    title = "Distribution of Recency",  # Add a title to the plot
    x = "Recency (days)",               # X-axis label
    y = "Frequency"                     # Y-axis label
  ) +
  # Annotate the percentiles with labels
  annotate("text", x = recency_quantiles[1] + 1, y = 30, label = "25th %tile", color = "red", angle = 90, hjust = -0.1) +
  annotate("text", x = recency_quantiles[2] + 1, y = 30, label = "75th %tile", color = "green", angle = 90, hjust = -0.1) +
  annotate("text", x = upper_bound + 1 , y = 30, label = "Upper Bound (2x IQR)", color = "purple", angle = 90, hjust = -0.1) +
  # Use a minimal theme for better readability
  theme_minimal()


# Step 3: Save Customers exceeding 2 multiplier threshold in a new dataset
upper_bound_2 <- recency_quantiles[2] + 2 * IQR_recency
Customers_outliers_2 <- Customers %>%
  filter(Recency > upper_bound_2)

## For now i will cut these customer, but later on we can check if we can save some high value lost customer 

# Step 4: Remove outliers from Customers dataset
Customers <- Customers %>%
  filter(Recency <= upper_bound_2)


# Step 5: Idenfication of super customers 

summary(Customers$Frequency)
summary(Customers$Monetary_Value)


# Calculate the IQR (Interquartile Range) for Frequency and Monetary Value
freq_IQR <- IQR(Customers$Frequency, na.rm = TRUE)
monetary_IQR <- IQR(Customers$Monetary_Value, na.rm = TRUE)

# Define a wide range of percentiles to analyze
percentiles <- seq(0.7, 0.99, by = 0.01)

# Calculate quantiles for Frequency and Monetary Value
dynamic_freq_quantiles <- quantile(Customers$Frequency, probs = percentiles, na.rm = TRUE)
dynamic_monetary_quantiles <- quantile(Customers$Monetary_Value, probs = percentiles, na.rm = TRUE)

# Create a data frame to analyze number of outliers at each percentile
percentile_analysis <- data.frame(Percentile = percentiles)

percentile_analysis <- percentile_analysis %>%
  mutate(
    Freq_Outliers = sapply(dynamic_freq_quantiles, function(thresh) nrow(Customers %>% filter(Frequency > thresh))),
    Monetary_Outliers = sapply(dynamic_monetary_quantiles, function(thresh) nrow(Customers %>% filter(Monetary_Value > thresh)))
  )

# Visualize the Elbow Method for defining the optimal percentile
library(ggplot2)

ggplot(percentile_analysis, aes(x = Percentile)) +
  geom_line(aes(y = Freq_Outliers, color = "Frequency Outliers"), size = 1) +
  geom_line(aes(y = Monetary_Outliers, color = "Monetary Outliers"), size = 1) +
  labs(
    title = "Elbow Method for Optimal Percentile",
    x = "Percentile",
    y = "Number of Outliers",
    color = "Outlier Type"
  ) +
  theme_minimal()


# Inspect the distributions of Frequency and Monetary Value
# Plot boxplots
boxplot_freq <- ggplot(Customers, aes(y = Frequency)) +
  geom_boxplot(fill = "blue", color = "black", alpha = 0.7) +
  labs(
    title = "Boxplot of Frequency",
    y = "Frequency"
  ) +
  theme_minimal()

boxplot_monetary <- ggplot(Customers, aes(y = Monetary_Value)) +
  geom_boxplot(fill = "green", color = "black", alpha = 0.7) +
  labs(
    title = "Boxplot of Monetary Value",
    y = "Monetary Value"
  ) +
  theme_minimal()

print(boxplot_freq)
print(boxplot_monetary)


# Use the plot to determine the elbow point where the increase in outliers flattens
# Select the optimal percentile dynamically based on the plot
selected_percentile <- 0.75

# After several triales, we selected the percentile 0.75

# Calculate thresholds for the selected percentile
freq_upper_bound <- quantile(Customers$Frequency, probs = selected_percentile, na.rm = TRUE)
monetary_upper_bound <- quantile(Customers$Monetary_Value, probs = selected_percentile, na.rm = TRUE)

# Compare different multipliers for outlier detection
multipliers <- seq(1, 3, by = 0.5)
outlier_analysis <- data.frame(Multiplier = multipliers)

# Calculate upper bounds and number of outliers for each multiplier
outlier_analysis <- outlier_analysis %>%
  mutate(
    Freq_Upper_Bound = freq_upper_bound + Multiplier * freq_IQR,
    Monetary_Upper_Bound = monetary_upper_bound + Multiplier * monetary_IQR,
    Freq_Outliers = sapply(Freq_Upper_Bound, function(ub) nrow(Customers %>% filter(Frequency > ub))),
    Monetary_Outliers = sapply(Monetary_Upper_Bound, function(ub) nrow(Customers %>% filter(Monetary_Value > ub)))
  )

# Output the results
print(outlier_analysis)

# Visualize the results using ggplot2

ggplot(outlier_analysis, aes(x = Multiplier)) +
  geom_line(aes(y = Freq_Outliers, color = "Frequency Outliers"), size = 1) +
  geom_line(aes(y = Monetary_Outliers, color = "Monetary Outliers"), size = 1) +
  geom_point(aes(y = Freq_Outliers, color = "Frequency Outliers"), size = 2) +
  geom_point(aes(y = Monetary_Outliers, color = "Monetary Outliers"), size = 2) +
  labs(
    title = "Optimal Multiplier for Outlier Detection",
    x = "Multiplier",
    y = "Number of Outliers",
    color = "Outlier Type"
  ) +
  theme_minimal()

# Select the multiplier to use based on the plot
selected_multiplier <- 1.5  
freq_upper_bound <- freq_upper_bound + selected_multiplier * freq_IQR
monetary_upper_bound <- monetary_upper_bound + selected_multiplier * monetary_IQR

# Identify potential outliers for Frequency and Monetary Value using the selected multiplier
outliers_frequency <- Customers %>% filter(Frequency > freq_upper_bound)
outliers_monetary <- Customers %>% filter(Monetary_Value > monetary_upper_bound)


# Combine outliers for further analysis
combined_outliers <- bind_rows(outliers_frequency, outliers_monetary) %>% distinct()

# Display combined outliers
print(combined_outliers)



# Step 6: Identify Super Customers Based on Frequency and Monetary Value

# Identify super customers based on the thresholds for Frequency and Monetary Value
# Customers are classified as super customers if they have either high frequency or high monetary value
super_customer <- Customers %>%
  filter(
    (Frequency > freq_upper_bound & !is.na(freq_upper_bound)) |  # Check for customers with high frequency
      (Monetary_Value > monetary_upper_bound & !is.na(monetary_upper_bound))  # Check for customers with high monetary value
  ) %>%
  # Add a new column to indicate the type of anomaly ("frequency" or "monetary")
  mutate(
    anomaly = case_when(
      Frequency > freq_upper_bound ~ "frequency",
      Monetary_Value > monetary_upper_bound ~ "monetary"
    )
  )


# Step 4: Remove Super Customers from Final Data

# Filter out the super customers from the Final_Data dataframe
# These super customers will be dealt with separately in a later analysis step
Customers <- Customers %>%
  filter(
    (Frequency <= freq_upper_bound | is.na(freq_upper_bound)) &  # Keep customers whose frequency is below or equal to the threshold
      (Monetary_Value <= monetary_upper_bound | is.na(monetary_upper_bound))  # Keep customers whose monetary value is below or equal to the threshold
  )



## Start the ANALYSIS

freq_quartiles <- quantile(Customers$Frequency, probs = c(0.2, 0.5, 0.8), na.rm = TRUE)
monetary_quartiles <- quantile(Customers$Monetary_Value, probs = c(0.2, 0.5, 0.8), na.rm = TRUE)
recency_quartiles <- quantile(Customers$Recency, probs = c(0.2, 0.5, 0.8), na.rm = TRUE)

Customers <- Customers %>%
  mutate(
    
    # Etichetta per Monetary Value
    Monetary_Label = case_when(
      Monetary_Value <= monetary_quartiles[1] ~ "L",  # Low (<= 20th percentile)
      Monetary_Value <= monetary_quartiles[2] ~ "M",  # Medium (>20th - <=50th percentile)
      Monetary_Value <= monetary_quartiles[3] ~ "H",  # High (>50th - <=80th percentile)
      TRUE ~ "H"  # Very High (>80th percentile)
    ),
    
    # Etichetta per Frequency
    Frequency_Label = case_when(
      Frequency <= freq_quartiles[1] ~ "L",  # Low (<= 20th percentile)
      Frequency <= freq_quartiles[2] ~ "M",  # Medium (>20th - <=50th percentile)
      Frequency <= freq_quartiles[3] ~ "H",  # High (>50th - <=80th percentile)
      TRUE ~ "H"  # Very High (>80th percentile)
    ),
    
    # Etichetta per Recency
    Recency_Label = case_when(
      Recency <= recency_quartiles[1] ~ "H",  # High (recent purchases, <= 20th percentile)
      Recency <= recency_quartiles[2] ~ "M",  # Medium (>20th - <=50th percentile)
      Recency <= recency_quartiles[3] ~ "L",  # Low (>50th - <=80th percentile)
      TRUE ~ "L"  # Very Low (least recent, >80th percentile)
    )
  )

# Segmentare i clienti in base al punteggio RFM
Customers <- Customers %>%
  mutate(
    Customer_Segment = case_when(
      # Champions
      Recency_Label == "H" & Frequency_Label == "H" & Monetary_Label == "H" ~ "Champions",
      Recency_Label == "H" & Frequency_Label == "M" & Monetary_Label == "H" ~ "Champions",
      Recency_Label == "H" & Frequency_Label == "H" & Monetary_Label == "M" ~ "Champions",
      
      # Loyal
      Recency_Label == "M" & Frequency_Label == "H" & Monetary_Label == "H" ~ "Loyal",
      Recency_Label == "M" & Frequency_Label == "H" & Monetary_Label == "M" ~ "Loyal",
      
      # Cannot Lose
      Recency_Label == "L" & Frequency_Label == "M" & Monetary_Label == "H" ~ "Cannot Lose",
      Recency_Label == "L" & Frequency_Label == "H" & Monetary_Label == "H" ~ "Cannot Lose",
      Recency_Label == "L" & Frequency_Label == "H" & Monetary_Label == "M" ~ "Cannot Lose",
      
      # Potential Loyalist
      Recency_Label == "H" & Frequency_Label == "M" & Monetary_Label == "M" ~ "Potential Loyalist",
      Recency_Label == "H" & Frequency_Label == "M" & Monetary_Label == "L" ~ "Potential Loyalist",
      Recency_Label == "H" & Frequency_Label == "L" & Monetary_Label == "M" ~ "Potential Loyalist",
      Recency_Label == "H" & Frequency_Label == "H" & Monetary_Label == "L" ~ "Potential Loyalist",
      
      # As New
      Recency_Label == "H" & Frequency_Label == "L" & Monetary_Label == "L" ~ "As New",
      Recency_Label == "H" & Frequency_Label == "L" & Monetary_Label == "H" ~ "As New",
      Recency_Label == "M" & Frequency_Label == "H" & Monetary_Label == "L" ~ "As New",
      
      # To Reactivate
      Recency_Label == "M" & Frequency_Label == "L" & Monetary_Label == "H" ~ "To Reactivate",
      Recency_Label == "M" & Frequency_Label == "L" & Monetary_Label == "M" ~ "To Reactivate",
      
      # Searching for Attention
      Recency_Label == "M" & Frequency_Label == "M" & Monetary_Label == "M" ~ "Searching for Attention",
      Recency_Label == "M" & Frequency_Label == "M" & Monetary_Label == "L" ~ "Searching for Attention",
      
      # About to Sleep
      Recency_Label == "L" & Frequency_Label == "L" & Monetary_Label == "M" ~ "About to Sleep",
      Recency_Label == "L" & Frequency_Label == "M" & Monetary_Label == "M" ~ "About to Sleep",
      Recency_Label == "L" & Frequency_Label == "L" & Monetary_Label == "H" ~ "About to Sleep",
      
      # Bad
      Recency_Label == "L" & Frequency_Label == "L" & Monetary_Label == "M" ~ "Bad",
      Recency_Label == "M" & Frequency_Label == "L" & Monetary_Label == "L" ~ "Bad",
      Recency_Label == "L" & Frequency_Label == "M" & Monetary_Label == "L" ~ "Bad",
      Recency_Label == "L" & Frequency_Label == "L" & Monetary_Label == "L" ~ "Bad",
      
      # Default: Unclassified for unforeseen combinations
      TRUE ~ "Unclassified"
    )
  )

# Customer Segmentation Based on Recency, Frequency, and Monetary Value:
# - Champions: Customers with low recency, high frequency, and high monetary value. These are the best customers.
# - Loyal: Customers with moderate recency, high frequency, and moderate monetary value.
# - Potential Loyalist: Customers with low recency but either not the highest frequency or monetary value; they have growth potential.
# - As New: Customers who recently joined or have low recency and low frequency.
# - To Reactivate: Customers with high recency who need incentives to become active again.
# - Searching for Attention: Customers with moderate recency, moderate frequency, and value but may become inactive without attention.
# - About to Sleep: Customers with increasing recency, low frequency, and are at risk of abandoning the relationship.
# - Cannot Lose: Customers with high monetary value but higher recency; they need attention to prevent them from being lost.
# - Bad: Customers with high recency, low frequency, and low monetary value; they offer little value to the company.


# Calculate frequency distribution of Custo---X column
customer_type_freq <- table(Customers$Customer_Segment)

# Calculate percentage occurrence
customer_type_percentage <- prop.table(customer_type_freq) * 100

# Display frequency and percentage
percentage_table <- data.frame(
  Customer_Type = names(customer_type_percentage),
  Frequency = as.vector(customer_type_freq),
  Percentage = round(as.vector(customer_type_percentage), 2) # Rounded to 2 decimals
)

percentage_table <- percentage_table[order(-percentage_table$Percentage), ]


## Also in this situation, whe have too many clusters, which make the analysis too complex and also too costly 

# Convert Recency, Frequency, and Monetary labels into numeric values for clustering
Customers <- Customers %>%
  mutate(
    Recency_Score = case_when(
      Recency_Label == "H" ~ 3,  # High Recency gets a score of 3
      Recency_Label == "M" ~ 2,  # Medium Recency gets a score of 2
      Recency_Label == "L" ~ 1   # Low Recency gets a score of 1
    ),
    Frequency_Score = case_when(
      Frequency_Label == "H" ~ 3,  # High frequency gets a score of 3
      Frequency_Label == "M" ~ 2,  # Medium frequency gets a score of 2
      Frequency_Label == "L" ~ 1   # Low frequency gets a score of 1
    ),
    Monetary_Score = case_when(
      Monetary_Label == "H" ~ 3,  # High monetary value gets a score of 3
      Monetary_Label == "M" ~ 2,  # Medium monetary value gets a score of 2
      Monetary_Label == "L" ~ 1   # Low monetary value gets a score of 1
    )
  )

# Select variables for clustering
clustering_data <- Customers %>%
  select(Recency_Score, Frequency_Score, Monetary_Score)


# Apply k-means to reduce the number of clusters and determine the optimal value of k
# Calculate the total within-cluster sum of squares for different values of k (1 to 10)
wss <- sapply(1:10, function(k){
  kmeans(clustering_data, centers = k, nstart = 20)$tot.withinss
})


# Plot the Elbow Method to Find the Optimal Value of k
plot(1:10, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters K",  # X-axis label
     ylab = "Total Within-Cluster Sum of Squares",  # Y-axis label
     main = "Elbow Method to Determine the Optimal Number of K")  # Plot title

# Set the Desired Number of Clusters Based on the Elbow Method
set.seed(123)  # Set seed for reproducibility
k <- 3 # Desired number of clusters (chosen based on the elbow method)
kmeans_result <- kmeans(clustering_data, centers = k, nstart = 20)


# Install required package if not already installed
if (!require(cluster)) install.packages("cluster")

# Calculate silhouette width for k-means clustering
library(cluster)
silhouette_scores <- silhouette(kmeans_result$cluster, dist(clustering_data))

# Plot the silhouette scores
plot(silhouette_scores, main = "Silhouette Plot for k-means Clustering",
     col = 1:k, border = NA)

# Calculate the average silhouette width
avg_silhouette_width <- mean(silhouette_scores[, 3])
cat("Average Silhouette Width:", avg_silhouette_width, "\n")
if (avg_silhouette_width > 0.5) {
  cat("The clustering quality is good with well-defined clusters.\n")
} else if (avg_silhouette_width > 0.3) {
  cat("The clustering quality is moderate, with some overlap between clusters.\n")
} else {
  cat("The clustering quality is poor; consider revising the number of clusters or preprocessing steps.\n")
}


# Add the Assigned Cluster to the Original Data
Customers <- Customers %>%
  mutate(Cluster = as.factor(kmeans_result$cluster))  # Add the cluster assignment as a new column

# Examine the Distribution of Clusters
table(Customers$Cluster)  # Display the number of customers in each cluster

# Enhanced Scatter Plot to Visualize Clusters Using PCA (Principal Component Analysis)
to_plot <- prcomp(clustering_data, center = TRUE, scale. = TRUE)  # Perform PCA on clustering data
plot_data <- as.data.frame(to_plot$x)  # Convert PCA scores into a dataframe
plot_data$Cluster <- Customers$Cluster  # Add cluster information to the PCA dataframe

# Analyze the Loadings of Principal Components
loadings <- as.data.frame(to_plot$rotation)  # Extract loadings of the principal components
print(loadings)  # Print the loadings to understand the contributions of each variable


# PC1 (Overall Engagement Score) is a combination of all three variables and represents an overall measure of customer engagement. 
# Lower PC1 values indicate higher engagement.
# PC 2 (Recency vs. Value Dynamics) represents customer recency, with higher values indicating more recent activity and lower values 
# indicating a longer lapse since the last activity.


# Enhanced Scatter Plot to Visualize Clusters Using PCA with Meaningful Labels
ggplot(plot_data, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(alpha = 0.7) +  # Plot points with transparency to better visualize overlap
  theme_minimal() +  # Use a minimal theme for better aesthetics
  labs(
    title = "Customer Clustering (PCA)",  # Plot title
    x = "Overall Engagement Score (PC1)",  # X-axis label reflecting customer engagement
    y = "Recency vs. Value Dynamics (PC2)",  # Y-axis label reflecting irregular spending behavior
    color = "Cluster"  # Legend label for clusters
  ) +
  theme(legend.position = "bottom")  # Position the legend at the bottom for better layout

# Assign Meaningful Names to Clusters for Simplified Analysis
Customers <- Customers %>%
  mutate(Simplified_Cluster = case_when(
    Cluster == 2 ~ "Low Engagement",  
    Cluster == 3 ~ "Consistently Moderate",  
    Cluster == 1 ~ "Highly Active and Recent",  
    TRUE ~ "Unclassified"  # Default value for unclassified clusters
  ))


# Cluster 1 (Red) - "Low Engagement":
# Characteristics: Customers in this group exhibit low monetary value (51,084.36) and low visit frequency (16.23 visits). 
# Their purchases occur less frequently (19.72 days recency), and their IPT is 29.42 days, reflecting longer gaps between purchases. 
# They purchase the fewest items, with 122.41 other items and 48.36 PL items on average.
# Description: These customers are minimally engaged, contributing the least to overall revenue. They may be new or inactive customers 
# who require significant reactivation efforts. They present opportunities for targeted campaigns to reignite their interest and increase 
# their engagement.

# Cluster 2 (Green) - "Consistently Moderate":
# Characteristics: Customers in this cluster have an average monetary value of 24,743.41, with moderate visit frequency (60.48 visits) 
# and relatively low recency (9.43 days). Their average inter-purchase time (IPT) is 7.43 days, and they purchase a balanced number of 
# items, with 590.69 other items and 271.93 private-label (PL) items on average.
# Description: These customers are consistent buyers, showing steady purchasing behavior over time. They are reliable contributors to 
# the revenue and represent a stable customer base. The focus should be on maintaining their loyalty and gradually increasing their 
# spending through personalized offers or loyalty programs.

# Cluster 3 (Blue) - "Highly Active and Recent":
# Characteristics: These customers have the highest average monetary value (338,117.78) and visit the most frequently (95.98 visits). 
# Their purchases are very recent (0.57 days), and their average IPT is 4.69 days, showing consistent engagement. They purchase 851.50 
# other items and 404.78 PL items on average.
# Description: These are the most valuable and active customers. Their frequent visits and high spending demonstrate exceptional loyalty 
# and engagement. This segment contributes significantly to overall revenue and should be treated as a high-priority group.


# Visualize the Distribution of New Customer Segments
ggplot(Customers, aes(x = Simplified_Cluster)) +
  geom_bar(fill = "skyblue") +  # Create a bar plot to show the count of customers in each segment
  theme_minimal() +  # Use a minimal theme for better readability
  labs(
    title = "Distribution of New Customer Segments",  # Title of the plot
    x = "Segment",  # X-axis label representing customer segments
    y = "Customer Count"  # Y-axis label representing the count of customers
  ) +
  # Rotate the x-axis labels for better visibility
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Clusters 

# Split the dataset into three clusters
Cluster_1 <- Customers %>% filter(Simplified_Cluster == "Low Engagement")
Cluster_2 <- Customers %>% filter(Simplified_Cluster == "Consistently Moderate")
Cluster_3 <- Customers %>% filter(Simplified_Cluster == "Highly Active and Recent")


super_customer
summary(super_customer)
sc_quartiles_mon <- quantile(super_customer$Monetary_Value, probs = c(0.25, 0.8), na.rm = TRUE)
sc_quartiles_freq <- quantile(super_customer$Frequency, probs = c(0.25, 0.8), na.rm = TRUE)

super_customer <- super_customer %>%
  filter(Monetary_Value < sc_quartiles_mon[2] & 
           Frequency < sc_quartiles_freq[2])


merged_customers <- bind_rows(
  Cluster_1 %>% mutate(Final_Segment = "Low Engagement"),
  Cluster_2 %>% mutate(Final_Segment = "Consistently Moderate"),
  Cluster_3 %>% mutate(Final_Segment = "Highly Active and Recent"),
  super_customer %>% mutate(Final_Segment = "Super Customer")
)

# Save the merged_customers data frame to a CSV file
write.csv(merged_customers, "merged_customers.csv", row.names = FALSE)



# Step 6: Study the habits of each cluster and summarize them in a table
cluster_summary <- merged_customers %>%
  group_by(Final_Segment) %>%
  summarize(
    Avg_Monetary_Value = mean(Monetary_Value, na.rm = TRUE),
    AVG_Ticket = mean(AVG_Ticket, na.rm = TRUE),
    Avg_Visits = mean(Frequency, na.rm = TRUE),
    Avg_Recency = mean(Recency, na.rm = TRUE),
    AVG_IPT = mean(AVG_ipt, na.rm = TRUE),
    Total_Customers = n(),
    Avg_Number_Item_Other = mean(Number_item_other, na.rm = TRUE),
    Avg_Number_Item_PL = mean(Number_item_PL, na.rm = TRUE),
    PL_on_Tot = Avg_Number_Item_PL / sum(Avg_Number_Item_PL, Avg_Number_Item_Other),
    .groups = 'drop'
  )

library(patchwork)

# Step 7: Plot VARIABLE distribution for each of the 3 clusters
# Plot VARIABLE distribution for each cluster in a single plot using facets

combined_plot <- ggplot(merged_customers, aes(x = Final_Segment, y = Number_item_PL, fill = Final_Segment)) +
  geom_boxplot() +
  labs(
    title = "Number_item_PL Distribution Across Clusters",
    x = "Cluster",
    y = "Number_item_PL"
  ) +
  theme_minimal()

print(combined_plot)






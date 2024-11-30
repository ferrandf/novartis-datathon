setwd("Downloads")
library(dplyr)
library(tidyr)
library(lubridate)

# Load data
data <- read.csv("train_data.csv")

# Data cols
cols <- colnames(data)

# Transform date cols to Date
data$date <- as.Date(data$date)
data$launch_date <- as.Date(data$launch_date)

# Transform array cols to array
# data$indication <- as.list(data$indication)

################################################################################################

# Average of price_unit
average_price <- data %>%
  group_by(country) %>%
  summarize(avg_price = mean(price_unit, na.rm = TRUE))

# View the result
print(average_price)

#------------------------------------------------------ it is not modifying the data output
# Average of price_unit
average_target <- data %>%
  group_by(country) %>%
  summarize(avg_target = mean(target, na.rm = TRUE))

# View the result
print(average_target)

average_data <- merge(average_price, average_target, by = "country")

#------------------------------------------------------

# Set the number of clusters
set.seed(123) # For reproducibility
num_clusters <- 4 # Choose the number of clusters

# Perform k-means clustering
kmeans_result <- kmeans(average_price$avg_price, centers = num_clusters)

# Add cluster labels to the original data
average_price$Country_Group <- kmeans_result$cluster

# Assuming `clustered_data` contains unique countries with their clusters
data <- data %>%
  left_join(average_price %>% select(country, Country_Group), by = "country")


###### Same for price

# Set the number of clusters
set.seed(123) # For reproducibility
num_clusters <- 4 # Choose the number of clusters

# Perform k-means clustering
kmeans_result <- kmeans(data$price_unit, centers = num_clusters)

# Add cluster labels to the original data
data$Price_Group <- kmeans_result$cluster

### Same for therapeutical area
# Step 1: Aggregate target data by therapeutic_area
library(dplyr)
agg_data <- data %>%
  group_by(therapeutic_area) %>%
  summarise(
    mean_target = mean(target, na.rm = TRUE),
    sd_target = sd(target, na.rm = TRUE),
    median_target = median(target, na.rm = TRUE)
  )

# Step 2: Standardize the aggregated features
agg_data_scaled <- as.data.frame(scale(agg_data[,-1])) # Scale numeric columns (mean, sd, median)

# Step 3: Perform clustering (e.g., hierarchical clustering or k-means)
set.seed(123)
num_clusters <- 5 # Choose the desired number of clusters

# K-means clustering
kmeans_result <- kmeans(agg_data_scaled, centers = num_clusters)

# Add cluster labels to the aggregated data
agg_data$cluster <- kmeans_result$cluster

# Step 4: Visualize therapeutic areas similarity (optional: use hierarchical clustering for a dendrogram)
library(ggplot2)
ggplot(agg_data, aes(x = mean_target, y = sd_target, color = as.factor(cluster), label = therapeutic_area)) +
  geom_point(size = 3) +
  geom_text(hjust = 1.1, vjust = 1.1) +
  labs(title = "Clusters of Therapeutic Areas", color = "Cluster") +
  theme_minimal()

# Step 5: (Optional) Map clusters back to original data
data <- merge(data, agg_data[, c("therapeutic_area", "cluster")], by = "therapeutic_area")

data <- data |> rename(therapeutical_cluster = cluster)

############################################### ARRAY COUNT
# Assuming the column with arrays is named "array_column"
INDICATIONS <- lapply(data$indication, function(x) {
  # Remove brackets and split the string into elements
  gsub("[\\[\\]']", "", x) |> # Remove brackets and single quotes
    strsplit(", ") |>        # Split by ', ' to create a list
    unlist()                 # Flatten to a vector
})

# Assuming the column with arrays is named "array_column"
data$indication_number <- sapply(INDICATIONS, function(x) length(x))

#----------------------------------------------------------------------------- not modifying data output
# Unnest the 'indication' column so each row has a single indication
data_expanded <- data %>%
  mutate(indication = as.list(indication)) %>% # Ensure it's a list
  unnest(indication)

# Group by indication and calculate average price_unit
average_price_per_indication <- data_expanded %>%
  group_by(indication) %>%
  summarize(avg_price_unit = mean(price_unit, na.rm = TRUE))

# View the result
print(average_price_per_indication)
#-------------------------------------------------------------------------------------------

#### create column avg price per year
data <- data %>%
  group_by(drug_id) %>%
  mutate(avg_price_per_year = mean(price_unit, na.rm = TRUE)) %>%
  ungroup() # Ungroup after the operation to avoid unintended grouping

# Split the data by therapeutical_area
split_data <- split(data, data$therapeutical_cluster)

# Example: Accessing one subset (e.g., for the first therapeutic area)
subset_22ED <- split_data[[1]]
subset_4BA5_645F_8E53_980E <- split_data[[5]]
subset_96D7 <- split_data[[3]]
subset_032C_051D_644A_66C5_6CEE <- split_data[[4]]
subset_CD59 <- split_data[[2]]

# Step 1: Filter the rows for the specific countries
subset_CD59_country_0309_1007_D8B0_89F9_6C16_53A5_4253_A67D_907E_445D <- subset(subset_CD59, country %in% c("COUNTRY_1007", "COUNTRY_0309", "COUNTRY_D8B0", "COUNTRY_89F9", 
                                                                                   "COUNTRY_6C16", "COUNTRY_53A5", "COUNTRY_4253", "COUNTRY_A67D", "COUNTRY_907E",
                                                                                   "COUNTRY_445D"))

# Step 2: Remove these rows from the original dataset
subset_CD59 <- subset(subset_CD59, !country %in% c("COUNTRY_1007", "COUNTRY_0309", "COUNTRY_D8B0", "COUNTRY_89F9", "COUNTRY_6C16", 
                                                   "COUNTRY_4253", "COUNTRY_A67D", "COUNTRY_907E", "COUNTRY_445D"))

# subset_645F <- split_data[[6]]
# subset_66C5 <- split_data[[7]]
# subset_6CEE <- split_data[[8]]
# subset_8E53 <- split_data[[9]]
# subset_96D7 <- split_data[[10]]
# subset_980E <- split_data[[11]]
# subset_CD59 <- split_data[[12]]

# Assuming these variables (subset_032C, subset_051D, ...) exist in your environment
subsets <- list(
  subset_22ED = subset_22ED,
  subset_4BA5_645F_8E53_980E = subset_22ED_4BA5_645F_8E53_980E, 
  subset_CD59 = subset_CD59, 
  subset_96D7 = subset_96D7, 
  subset_032C_051D_644A_66C5_6CEE = subset_032C_051D_644A_66C5_6CEE,
  subset_CD59_country_0309_1007_D8B0_89F9_6C16_53A5_4253_A67D_907E_445D = subset_CD59_country_0309_1007_D8B0_89F9_6C16_53A5_4253_A67D_907E_445D
  # subset_644A = subset_644A, 
  # subset_645F = subset_645F,
  # subset_66C5 = subset_66C5, 
  # subset_6CEE = subset_6CEE, 
  # subset_8E53 = subset_8E53, 
  # subset_96D7 = subset_96D7, 
  # subset_980E = subset_980E, 
  # subset_CD59 = subset_CD59
)

#### logtransform in target per subset
subset_22ED$target <- log(subset_22ED$target) 
subset_4BA5_645F_8E53_980E$target <- log(subset_4BA5_645F_8E53_980E$target) 
subset_CD59$target <- log(subset_CD59$target) 
subset_96D7$target <- log(subset_96D7$target)
subset_032C_051D_644A_66C5_6CEE$target <- log(subset_032C_051D_644A_66C5_6CEE$target)
subset_CD59_country_0309_1007_D8B0_89F9_6C16_53A5_4253_A67D_907E_445D$target <- log(subset_CD59_country_0309_1007_D8B0_89F9_6C16_53A5_4253_A67D_907E_445D$target)

# subset_032C$target <- log(subset_032C$target)
# subset_051D$target <- log(subset_051D$target)
# subset_22ED$target <- log(subset_22ED$target)
# subset_4BA5$target <- log(subset_4BA5$target)
# subset_644A$target <- log(subset_644A$target)
# subset_645F$target <- log(subset_645F$target)
# subset_66C5$target <- log(subset_66C5$target)
# subset_6CEE$target <- log(subset_6CEE$target)
# subset_8E53$target <- log(subset_8E53$target)
# subset_96D7$target <- log(subset_96D7$target)
# subset_980E$target <- log(subset_980E$target)
# subset_CD59$target <- log(subset_CD59$target)

### We want to replace outliers with the "limit" value

for (name in names(subsets)) {
  dataset <- subsets[[name]] # Access the dataset
  for (col in names(dataset)) { # Iterate over each column
    if (is.numeric(dataset[[col]])) { # Check if the column is numeric
      # Calculate the interquartile range (IQR)
      Q1 <- quantile(dataset[[col]], 0.25, na.rm = TRUE)
      Q3 <- quantile(dataset[[col]], 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      
      # Define lower and upper limits
      lower_limit <- Q1 - 1.5 * IQR
      upper_limit <- Q3 + 1.5 * IQR
      
      # Transform outliers to the limit values
      dataset[[col]][dataset[[col]] < lower_limit] <- lower_limit
      dataset[[col]][dataset[[col]] > upper_limit] <- upper_limit
    }
  }
  subsets[[name]] <- dataset # Update the dataset in the list
}

## Create a column that assign a number per each month
for (name in names(subsets)) {
  dataset <- subsets[[name]]  # Access the dataset
  
  if ("date" %in% names(dataset)) {  # Check if 'date' column exists
    dataset$month_number <- month(dataset$date)  # Extract month as number (1-12)
  }
  
  # Update the original variable outside of the subsets list
  assign(name, dataset)
}


# Write the subsets
write.csv(subset_22ED, "22ED.csv")
write.csv(subset_4BA5_645F_8E53_980E, "4BA5_645F_8E53_980E.csv")
write.csv(subset_CD59, "CD59.csv")
write.csv(subset_96D7, "96D7.csv")
write.csv(subset_032C_051D_644A_66C5_6CEE, "032C_051D_644A_66C5_6CEE.csv")
write.csv(subset_CD59_country_0309_1007_D8B0_89F9_6C16_53A5_4253_A67D_907E_445D, "CD59_country_0309_1007_D8B0_89F9_6C16_53A5_4253_A67D_907E_445D.csv")

# write.csv(subset_032C, "subset_032C.csv")
# write.csv(subset_051D, "subset_051D.csv")
# write.csv(subset_22ED, "subset_22ED.csv")
# write.csv(subset_4BA5, "subset_4BA5.csv")
# write.csv(subset_644A, "subset_644A.csv")
# write.csv(subset_645F, "subset_645F.csv")
# write.csv(subset_66C5, "subset_66C5.csv")
# write.csv(subset_6CEE, "subset_6CEE.csv")
# write.csv(subset_8E53, "subset_8E53.csv")
# write.csv(subset_96D7, "subset_96D7.csv")
# write.csv(subset_980E, "subset_980E.csv")
# write.csv(subset_CD59, "subset_CD59.csv")

# Write the data to a CSV file
#write.csv(data, "train_data_TRY2.csv")

################################################################################################

# Calculate the average target for each country
avg_target_by_country <- subset_CD59 %>%
  group_by(country) %>%
  summarise(avg_target = mean(target, na.rm = TRUE))

# View the result
print(sort(avg_target_by_country$avg_target))

avg_prev_by_country <- subset_CD59 %>%
  group_by(country) %>%
  summarise(avg_prev = mean(prev_perc, na.rm = TRUE))

# View the result
avg_haha <- merge(avg_target_by_country, avg_prev_by_country, by ="country")
print(avg_haha)

hist(subset_032C$target)
hist(subset_051D$target)
hist(subset_22ED$target)
hist(subset_4BA5$target)
hist(subset_644A$target)
hist(subset_645F$target)
hist(subset_66C5$target)
hist(subset_6CEE$target)
hist(subset_8E53$target)
hist(subset_96D7$target)
hist(subset_980E$target)
hist(subset_CD59$target)



# Initialize an empty list to store the results
averages <- list()

# Loop through each subset and calculate the mean of "target"
for (name in names(subsets)) {
  avg_target <- mean(subsets[[name]]$target, na.rm = TRUE) # Use na.rm = TRUE to handle missing values
  averages[[name]] <- avg_target
}

# Convert the results to a data frame for better readability
averages_df <- data.frame(
  Subset = names(averages),
  Average_Target = unlist(averages)
)

# Print the results
print(averages_df)

# remove ind_launch_date because it has a lot  of missing data and is not informative
# data <- data[ , -c(10)]

# There are no NAs 
na_cols <- colMeans(is.na(data))

# scale data, I think it is not necessary as the values are all pretty close
#data <- scale(data)

# Search for outliers

# Initialize a list to store outliers
outliers_list <- list()

# Loop through numerical columns
for (col_name in names(data)) {
  if (is.numeric(data[[col_name]])) {
    # Calculate IQR
    Q1 <- quantile(data[[col_name]], 0.25, na.rm = TRUE)
    Q3 <- quantile(data[[col_name]], 0.75, na.rm = TRUE)
    IQR_value <- Q3 - Q1
    
    # Define lower and upper bounds
    lower_bound <- Q1 - 1.5 * IQR_value
    upper_bound <- Q3 + 1.5 * IQR_value
    
    # Identify outliers
    outliers <- data[[col_name]][data[[col_name]] < lower_bound | data[[col_name]] > upper_bound]
    
    # Store the results in the list
    outliers_list[[col_name]] <- outliers
  }
}

# Print the number of outlier rows for each column
for (col_name in names(outliers_list)) {
  cat(sprintf(
    "Column: %s | Outliers: %d | Total Rows in Data: %d\n", 
    col_name, 
    length(outliers_list[[col_name]]), 
    nrow(data)
  ))
}

# Do the same but removing the outliers from the data
# Initialize an empty list to store outliers
outliers_list <- list()

# Loop through all columns in the data
for (col_name in names(data)) {
  if (is.numeric(data[[col_name]])) {
    # Calculate IQR
    Q1 <- quantile(data[[col_name]], 0.25, na.rm = TRUE)
    Q3 <- quantile(data[[col_name]], 0.75, na.rm = TRUE)
    IQR_value <- Q3 - Q1
    
    # Define lower and upper bounds
    lower_bound <- Q1 - 1.5 * IQR_value
    upper_bound <- Q3 + 1.5 * IQR_value
    
    # Identify outliers
    outliers <- data[[col_name]][data[[col_name]] < lower_bound | data[[col_name]] > upper_bound]
    
    # Store the results in the list
    outliers_list[[col_name]] <- outliers
    
    # Remove outliers if the column is "target"
    if (col_name == "target" || col_name == "price_month") {
      # Filter the dataset to keep only rows within bounds for the "target" column
      data <- data[data[[col_name]] >= lower_bound & data[[col_name]] <= upper_bound, ]
      
      # Break the loop to avoid further filtering on a modified dataset
      break
    }
  }
}


## SCALE
# Identify numeric columns
numeric_columns <- sapply(data, is.numeric)

# Scale only the numeric columns
data[numeric_columns] <- scale(data[numeric_columns])


# Descriptive statistics for numerical data
library(e1071) # For skewness calculation

for (col_name in names(data)) {
  if (is.numeric(data[[col_name]])) {
    cat(sprintf("Column: %s\n", col_name))
    cat(sprintf("Mean: %.2f | Median: %.2f | Std Dev: %.2f | Skewness: %.2f\n\n",
                mean(data[[col_name]], na.rm = TRUE),
                median(data[[col_name]], na.rm = TRUE),
                sd(data[[col_name]], na.rm = TRUE),
                skewness(data[[col_name]], na.rm = TRUE)))
  }
}

# Skewness > 0: Positive bias (right-skewed)
# Skewness < 0: Negative bias (left-skewed)

hist(data$price_month)

categorical_cols <- c()  # Initialize an empty vector

# Check for dominance of a category
for (col_name in names(data)) {
  if (is.character(data[[col_name]])) {
    categorical_cols <- c(categorical_cols, col_name)  # Append column name
  }
}

categorical_cols <- categorical_cols[-c(5, 7)]

# check if there is a dominance of a category
for (col_name in categorical_cols) {
  if (is.character(data[[col_name]])) {
    if (is.factor(data[[col_name]]) || is.character(data[[col_name]])) {
      cat(sprintf("Column: %s\n", col_name))
      print(prop.table(table(data[[col_name]])))
      cat("\n")
    }
  }
}

# Loop through each column to find the most dominant class
for (col_name in categorical_cols) {
  if (is.factor(data[[col_name]]) || is.character(data[[col_name]])) {
    # Get the frequency table for the column
    freq_table <- table(data[[col_name]])
    
    # Find the most dominant class and its count
    dominant_class <- names(freq_table)[which.max(freq_table)]
    dominant_count <- max(freq_table)
    
    # Calculate the total number of rows and the proportion
    total_rows <- nrow(data)
    proportion <- dominant_count / total_rows * 100
    
    # Print the results
    cat(sprintf("Column: %s\n", col_name))
    cat(sprintf("Most Dominant Class: %s | Count: %d | Proportion: %.2f%% of total rows (%d)\n\n", 
                dominant_class, dominant_count, proportion, total_rows))
  }
}


## Correlation of numerical variables with target
numerical_data_sub_032C <- subset_032C %>% select(where(is.numeric))

# library(psych)
# # S3 method for panels
# pairs(numerical_data)
      
cor(numerical_data_sub_032C)

bptest(numerical_data_sub_032C$avg_price_per_year, numerical_data_sub_032C$target)

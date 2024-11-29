setwd("Downloads")
library(dplyr)
library(tidyr)

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

# Remove rows where Drug_ID is "DRUG_ID_4C4E"
data <- data %>%
  filter(drug_id != "DRUG_ID_4C4E")
#--------------------------------------------------------------------------

# Average of price_unit
average_price <- data %>%
  group_by(country) %>%
  summarize(avg_price = mean(price_unit, na.rm = TRUE))

# View the result
print(average_price)

# Average of price_unit
average_target <- data %>%
  group_by(country) %>%
  summarize(avg_target = mean(target, na.rm = TRUE))

# View the result
print(average_target)

average_data <- merge(average_price, average_target, by = "country")

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

#-------------------------------------------------------------------------------------------
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


# Write the data to a CSV file
write.csv(data, "train_data_TRY1.csv")

################################################################################################

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



# remove indication
data <- data[ , -c(10)]

write.csv(data, "First_Clean_train_data.csv")

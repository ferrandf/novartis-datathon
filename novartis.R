# Load data
data <- read.csv("train_data.csv")

# Data cols
cols <- colnames(data)

# Transform date cols to Date
data$date <- as.Date(data$date)
data$launch_date <- as.Date(data$launch_date)

# Transform array cols to array
data$indication <- as.array(data$indication)

# remove ind_launch_date because it has a lot  of missing data and is not informative
data <- data[ , -c(10)]

# There are no NAs 
na_cols <- colMeans(is.na(data))

# scale data, I think it is not necessary as the values are all pretty close
# data <- scale(data)

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

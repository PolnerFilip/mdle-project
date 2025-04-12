source("./shared.R")
load_required_packages()

df <- read.csv("./data/merged/merged_data.csv")
head(df)

# Inspect missing values
na_summary <- sapply(df, function(x) {
  na_count <- sum(is.na(x))
  total_count <- length(x)
  
  # Percentage of missing values
  na_percentage <- (na_count / total_count) * 100
  
  c(na_count = na_count, na_percentage = na_percentage)
})

na_summary_df <- t(na_summary)
na_summary_df <- as.data.frame(na_summary_df)
print(na_summary_df)

# Drop columns with high % of missing values (>20%)
columns_to_drop <- rownames(na_summary_df)[na_summary_df$na_percentage > 20]
df <- df[, !names(df) %in% columns_to_drop]

# Replace NaN with Mean value
df[] <- lapply(df, function(x) {
  if (is.numeric(x)) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
  }
  return(x)
})

# Show distribution plots
numeric_cols <- names(df)[sapply(df, is.numeric)]

for (col in numeric_cols) {
  print(
    ggplot(df, aes_string(x = col)) +
      geom_histogram(fill = "skyblue", color = "black", bins = 30) +
      labs(title = paste("Distribution of", col), x = col, y = "Frequency") +
      theme_minimal()
  )
}

# Check skewness programatically
skew_values <- sapply(df[sapply(df, is.numeric)], skewness, na.rm = TRUE)

df_skewness <- data.frame(
  Column = names(skew_values),
  Skewness = as.numeric(skew_values)
)

print(df_skewness)

# Standardization and normalization
cols_to_standardize <- df_skewness$Column[df_skewness$Skewness >= -1 & df_skewness$Skewness <= 1]
cols_to_normalize   <- df_skewness$Column[df_skewness$Skewness < -1 | df_skewness$Skewness > 1]

# Standardization: (x - mean) / sd
df[cols_to_standardize] <- lapply(df[cols_to_standardize], function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
})

# Normalization: (x - min) / (max - min)
df[cols_to_normalize] <- lapply(df[cols_to_normalize], function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
})

# Export to csv
if (!dir.exists("data/processed")) {
  dir.create("data/processed", recursive = TRUE)
}
write.table(df, "data/processed/scaled_data.csv", sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)
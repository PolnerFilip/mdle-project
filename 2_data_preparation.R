required_packages <- c("dplyr", "tidyr", "tools", "lubridate")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

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

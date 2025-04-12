required_packages <- c("dplyr", "tidyr", "tools", "lubridate")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

# Check and import the datasets
file_paths <- list.files("data/raw", pattern = "\\.csv$", full.names = TRUE)

# Load datasets
dataset_list <- list()

for (file in file_paths) {
  file_name <- basename(file)
  
  data <- read.csv(file)
  print(head(data))
  
  dataset_list[[file_name]] <- data
  
  # Check for missing values in Date and Region (critical merging columns)
  missing_date <- sum(is.na(data$Date))
  missing_region <- sum(is.na(data$Region))
  
  cat("\n---", file_name, "---\n")
  cat("Summary: \n")
  print(summary(data))
  cat("Rows with missing Date:", missing_date, "\n")
  cat("Rows with missing Region:", missing_region, "\n")
  print(head(data))
}

wildfires_df = dataset_list[["Historical_Wildfires.csv"]]
weather_df = dataset_list[["HistoricalWeather.csv"]]
weather_forecast_df = dataset_list[["HistoricalWeatherForecasts.csv"]]
vegetation_df = dataset_list[["VegetationIndex.csv"]]

# Process and pivot weather datasets
colnames(weather_df) <- c(
  "Date", "Region", "Parameter", "Count_km2", "Min", "Max", "Mean", "Variance"
)

colnames(weather_forecast_df) <- c(
  "Date", "Region", "Parameter", "Lead_time", "Count_km2", "Min", "Max", "Mean", "Variance"
)

weather_forecast_df <- weather_forecast_df %>% select(-Lead_time)

weather_df <- weather_df %>%
  pivot_wider(names_from = Parameter, values_from = c(Count_km2, Min, Max, Mean, Variance), names_glue = "Weather_{Parameter}_{.value}")

weather_forecast_df <- weather_forecast_df %>%
  pivot_wider(names_from = Parameter, values_from = c(Count_km2, Min, Max, Mean, Variance), names_glue = "Weather_forecast_{Parameter}_{.value}", values_fn = mean)

# Prepare datasets for merging
wildfires_df$Date <- mdy(wildfires_df$Date)
weather_df$Date <- ymd(weather_df$Date)
weather_forecast_df$Date <- ymd(weather_forecast_df$Date)
vegetation_df$Date <- dmy(vegetation_df$Date)

head(wildfires_df)
head(weather_df)
head(weather_forecast_df)
head(vegetation_df)

# Merge datasets
merged_df <- wildfires_df %>%
  left_join(weather_df, by = c("Region", "Date")) %>%
  left_join(weather_forecast_df, by = c("Region", "Date")) %>%
  left_join(vegetation_df, by = c("Region", "Date"))

# Export csv
if (!dir.exists("data/merged")) {
  dir.create("data/merged", recursive = TRUE)
}
write.table(merged_df, "data/merged/merged_data.csv", sep = ",", row.names = FALSE, col.names = TRUE, quote = FALSE)

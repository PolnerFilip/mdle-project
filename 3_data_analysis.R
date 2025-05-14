source("./shared.R")
load_required_packages()

df <- read.csv("data/merged/merged_data.csv")

numeric_cols <- names(df)[sapply(df, is.numeric)]

for (col in numeric_cols) {
  print(
    ggplot(df, aes_string(x = col)) +
      geom_histogram(fill = "skyblue", color = "black", bins = 30) +
      labs(title = paste("Distribution of", col), x = col, y = "Frequency") +
      theme_minimal()
  )
}
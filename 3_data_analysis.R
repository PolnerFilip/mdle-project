source("./shared.R")
load_required_packages()

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

numeric_cols <- names(df)[sapply(df_clean, is.numeric)]

for (col in numeric_cols) {
  print(
    ggplot(df, aes_string(x = col)) +
      geom_histogram(fill = "skyblue", color = "black", bins = 30) +
      labs(title = paste("Distribution of", col), x = col, y = "Frequency") +
      theme_minimal()
  )
}
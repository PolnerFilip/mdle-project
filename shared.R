load_required_packages <- function() {
  required_packages <- c("dplyr", "tidyr", "tools", "lubridate", "ggplot2", "e1071", "randomForest", "tidyverse", "corrplot", "caret")
  
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}
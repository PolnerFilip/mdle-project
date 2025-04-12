load_required_packages <- function() {
  required_packages <- c("dplyr", "tidyr", "tools", "lubridate", "ggplot2", "e1071")
  
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}
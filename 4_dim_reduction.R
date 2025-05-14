# Install necessary packages
source("./shared.R")
load_required_packages()

# Load the data
df <- read.csv("data/processed/scaled_data.csv")

# Select only numeric columns, excluding non-numeric ones
df_numeric <- df %>%
  select(-Region, -Date)
df_numeric <- df[, sapply(df, is.numeric)]

# Calculate correlation matrix for numeric columns
cor_matrix <- cor(df_numeric, use = "complete.obs")

# Get the correlation of all features with the target variable (Estimated_fire_area)
cor_target <- cor_matrix[, "Estimated_fire_area"]

# Sort the correlations in descending order and display the top 10
cor_target_sorted <- sort(cor_target, decreasing = TRUE)
head(cor_target_sorted, 10)

# Get the top 10 features most correlated with the target
top_features <- names(sort(abs(cor_target), decreasing = TRUE))[2:11]

# Create a submatrix of the correlation matrix with the top features
selected_features <- c("Estimated_fire_area", top_features)
cor_submatrix <- cor_matrix[selected_features, selected_features]

# Plot the correlation heatmap of selected features
corrplot(cor_submatrix, method = "color", type = "upper", tl.cex = 0.8,
         tl.col = "black", addCoef.col = "black", number.cex = 0.7)

# Visualize the distribution of the target variable (Estimated_fire_area)
ggplot(df, aes(x = Estimated_fire_area)) +
  geom_histogram(bins = 50, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Estimated Fire Area", x = "Estimated Fire Area")


# Create a long format data for the top 10 correlated features for boxplot
df_long <- df %>% 
  select(all_of(top_features)) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

# Plot boxplots for the top 10 correlated features
ggplot(df_long, aes(x = Variable, y = Value)) +
  geom_boxplot(fill = "lightgreen") +
  coord_flip() +
  labs(title = "Boxplots of Top 10 Correlated Features")

# Train a Random Forest model to identify feature importance
set.seed(123)
rf_model <- randomForest(Estimated_fire_area ~ ., data = df_numeric, importance = TRUE, ntree = 500)

# Extract and display the importance of features based on Random Forest
importance_df <- as.data.frame(importance(rf_model))
importance_df <- importance_df %>% arrange(desc(IncNodePurity))

print("Top important features according to Random Forest:")
print(head(importance_df, 10))

# --- Principal Component Analysis (PCA) ---
# Select the top 10 features based on correlation
top_10_features <- names(sort(abs(cor_target), decreasing = TRUE))[2:11]

# Perform PCA on the top 10 features
pca_result <- prcomp(df_numeric[, top_10_features], scale. = TRUE)

# Create a new data frame to hold the first two principal components (PC1, PC2) for plotting
pca_df <- as.data.frame(pca_result$x[, 1:2])
pca_df$Estimated_fire_area <- df$Estimated_fire_area

# Plot PCA results to show how the data is spread along the first two principal components
ggplot(pca_df, aes(x = PC1, y = PC2, color = Estimated_fire_area)) +
  geom_point(alpha = 0.7) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "PCA of Top 10 Features",
       x = "Principal Component 1",
       y = "Principal Component 2",
       color = "Fire Area") +
  theme_minimal()

# --- Save PCA results ---
# Save the PCA results (first 3 principal components) to a CSV file
pca_data <- as.data.frame(pca_result$x[, 1:3])
write.csv(pca_data, "data/processed/pca_results.csv", row.names = FALSE)

# Save the explained variance for each principal component
pca_variance <- summary(pca_result)$importance
print(pca_variance)
pca_data <- read.csv("data/processed/pca_results.csv")

# combine target varibale + PCA
pca_data$Estimated_fire_area <- df$Estimated_fire_area
head(pca_data)

# save PCA + target variable
write.csv(pca_data, "data/processed/pca_with_target.csv", row.names = FALSE)

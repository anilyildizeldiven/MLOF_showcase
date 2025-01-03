# Load necessary libraries
library(isotree)
library(ggplot2)
library(caret)
library(FNN)  # For k-NN
library(dplyr)
library(missForest)  # For Random Forest Imputation

# Generate a 2D dataset
set.seed(42)
n <- 1000
p <- 2  # 2D for visualization
normal_data <- matrix(rnorm(n * p, mean = 0, sd = 1), ncol = p)
anomaly_data <- matrix(rnorm(50 * p, mean = 5, sd = 1), ncol = p)
data <- rbind(normal_data, anomaly_data)
labels <- c(rep(0, n), rep(1, 50))  # 0 = normal, 1 = anomaly
colnames(data) <- c("f1", "f2")

# 1. Isolation Forest
iso_forest <- isolation.forest(data, ndim = 1)
iso_scores <- predict(iso_forest, data)
iso_threshold <- quantile(iso_scores, 0.95)
iso_predictions <- ifelse(iso_scores > iso_threshold, 1, 0)

# 2. k-Nearest Neighbors
k <- 10
knn_distances <- knn.dist(data, k = k)  # Compute distances
knn_scores <- apply(knn_distances, 1, mean)
knn_threshold <- quantile(knn_scores, 0.95)
knn_predictions <- ifelse(knn_scores > knn_threshold, 1, 0)

# Combine outliers detected by both methods
combined_outliers <- which(iso_predictions == 1 | knn_predictions == 1)

# 3. Median Imputation
median_corrected_data <- data
feature_medians <- apply(data, 2, median)
for (i in combined_outliers) {
  median_corrected_data[i, ] <- feature_medians
}

# 4. Random Forest Imputation
# Replace outliers with NA
data_with_na <- data
data_with_na[combined_outliers, ] <- NA

# Perform Random Forest Imputation
# Load library
library(mice)

# Perform Random Forest Imputation
mice_result <- mice(data_with_na, method = "rf", m = 1)  # `m = 1` specifies a single imputation
rf_corrected_data <- complete(mice_result)

# Metrics Before, After Median, and After Random Forest Imputation
before_metrics <- data.frame(
  Feature = c("f1", "f2"),
  Mean = colMeans(data),
  Variance = apply(data, 2, var)
)
median_metrics <- data.frame(
  Feature = c("f1", "f2"),
  Mean = colMeans(median_corrected_data),
  Variance = apply(median_corrected_data, 2, var)
)
rf_metrics <- data.frame(
  Feature = c("f1", "f2"),
  Mean = colMeans(rf_corrected_data),
  Variance = apply(rf_corrected_data, 2, var)
)

# Combine metrics for visualization
metrics_comparison <- before_metrics %>%
  rename(Before_Mean = Mean, Before_Variance = Variance) %>%
  left_join(median_metrics, by = "Feature") %>%
  rename(Median_Mean = Mean, Median_Variance = Variance) %>%
  left_join(rf_metrics, by = "Feature") %>%
  rename(RF_Mean = Mean, RF_Variance = Variance)

# Print metrics comparison
print(metrics_comparison)

# Visualize Corrected Data (Random Forest)
corrected_with_labels_rf <- data.frame(rf_corrected_data, 
                                       Label = ifelse(seq_len(nrow(data)) %in% combined_outliers, "Corrected (RF)", "Normal"))

ggplot(corrected_with_labels_rf, aes(x = f1, y = f2, color = Label)) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Normal" = "gray", "Corrected (RF)" = "blue")) +
  ggtitle("Corrected Data Distribution (Random Forest Imputation)") +
  theme_minimal()

# Save the plot
ggsave("corrected_data_rf_plot.png", height = 5, width = 7)

# Visualize Corrected Data (Median)
corrected_with_labels_median <- data.frame(median_corrected_data, 
                                           Label = ifelse(seq_len(nrow(data)) %in% combined_outliers, "Corrected (Median)", "Normal"))

ggplot(corrected_with_labels_median, aes(x = f1, y = f2, color = Label)) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Normal" = "gray", "Corrected (Median)" = "red")) +
  ggtitle("Corrected Data Distribution (Median Imputation)") +
  theme_minimal()

# Save the plot
ggsave("corrected_data_median_plot.png", height = 5, width = 7)

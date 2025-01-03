library(isotree)
library(ggplot2)
library(caret)
library(FNN)  # For k-NN
library(dplyr)

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
overlap_outliers <- which(iso_predictions == 1 & knn_predictions == 1)

# Highlight overlap in the visualization
data_with_scores <- data.frame(data, 
                               IsoScore = iso_scores, 
                               KnnScore = knn_scores, 
                               Label = labels,
                               Combined = ifelse(seq_len(nrow(data)) %in% overlap_outliers, "Overlap", 
                                                 ifelse(iso_predictions == 1, "IsoForest", 
                                                        ifelse(knn_predictions == 1, "kNN", "Normal"))))

# Visualizing overlap between methods
ggplot(data_with_scores, aes(x = f1, y = f2, color = Combined)) +
  geom_point(size = 2) +
  scale_color_manual(values = c("IsoForest" = "blue", "kNN" = "green", "Overlap" = "purple", "Normal" = "gray")) +
  ggtitle("Overlap of Detected Outliers") +
  theme_minimal()

# Step 2: Correct outliers using Median Imputation
feature_medians <- apply(data, 2, median)

corrected_data <- data
for (i in combined_outliers) {
  corrected_data[i, ] <- feature_medians
}

# Evaluate before/after correction
before_metrics <- data.frame(
  Feature = c("f1", "f2"),
  Mean = colMeans(data),
  Variance = apply(data, 2, var)
)
after_metrics <- data.frame(
  Feature = c("f1", "f2"),
  Mean = colMeans(corrected_data),
  Variance = apply(corrected_data, 2, var)
)

# Combine metrics for visualization
metrics_comparison <- before_metrics %>%
  rename(Before_Mean = Mean, Before_Variance = Variance) %>%
  left_join(after_metrics, by = "Feature") %>%
  rename(After_Mean = Mean, After_Variance = Variance)

# Metrics visualization
print(metrics_comparison)



# Convert `Combined` to a factor for proper discrete coloring
data_with_scores$Combined <- as.factor(data_with_scores$Combined)

# Visualizing overlap between methods
ggplot(data_with_scores, aes(x = f1, y = f2, color = Combined)) +
  geom_point(size = 2) +
  scale_color_manual(values = c("IsoForest" = "blue", 
                                "kNN" = "green", 
                                "Overlap" = "purple", 
                                "Normal" = "gray")) +
  ggtitle("Overlap of Detected Outliers") +
  theme_minimal()





# Corrected Data
corrected_labels <- ifelse(seq_len(nrow(corrected_data)) %in% combined_outliers, "Corrected", "Normal")
corrected_with_scores <- data.frame(corrected_data, Label = corrected_labels)

ggplot(corrected_with_scores, aes(x = f1, y = f2, color = Label)) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Normal" = "gray", "Corrected" = "blue")) +
  ggtitle("Corrected Data Distribution") +
  theme_minimal()

# Save the results
ggsave("overlap_outliers_plot.png", height = 5, width = 7)
ggsave("corrected_data_plot.png", height = 5, width = 7)


# Load required library
library(caret)

# Assuming these variables are defined:
# iso_predictions: Predictions from Isolation Forest (0 for normal, 1 for anomaly)
# knn_predictions: Predictions from k-NN (0 for normal, 1 for anomaly)
# labels: True labels (0 for normal, 1 for anomaly)

# Function to calculate metrics
calculate_metrics <- function(predictions, true_labels) {
  # Confusion matrix
  cm <- confusionMatrix(as.factor(predictions), as.factor(true_labels))
  
  # Extract precision, recall, and F1-score
  precision <- cm$byClass["Precision"]
  recall <- cm$byClass["Recall"]
  f1_score <- cm$byClass["F1"]
  
  return(c(Precision = precision, Recall = recall, F1 = f1_score))
}

# Calculate metrics for Isolation Forest
iso_metrics <- calculate_metrics(iso_predictions, labels)

# Calculate metrics for k-NN
knn_metrics <- calculate_metrics(knn_predictions, labels)

# Print results
cat("Isolation Forest Metrics:\n")
print(iso_metrics)

cat("\nk-NN Metrics:\n")
print(knn_metrics)


library(pROC)

print(iso_roc )
# Isolation Forest
iso_roc <- roc(labels, iso_scores)
#iso_auc <- auc(iso_roc)

# k-NN
knn_roc <- roc(labels, knn_scores)
#knn_auc <- auc(knn_roc)

# Plot ROC curves
plot(iso_roc, col = "blue", main = "ROC Curves: Isolation Forest vs. k-NN")
lines(knn_roc, col = "green")
legend("bottomright", legend = c("Isolation Forest", "k-NN"), col = c("blue", "green"), lwd = 2)




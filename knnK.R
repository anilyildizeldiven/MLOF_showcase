library(FNN)
library(dplyr)
library(ggplot2)

set.seed(42)
n <- 1000
p <- 2  # 2D für Visualisierung
normal_data <- matrix(rnorm(n * p, mean = 0, sd = 1), ncol = p)
anomaly_data <- matrix(rnorm(50 * p, mean = 5, sd = 1), ncol = p)
data <- rbind(normal_data, anomaly_data)
labels <- c(rep(0, n), rep(1, 50))  # 0 = normal, 1 = anomaly
colnames(data) <- c("f1", "f2")

# Wir probieren verschiedene k-Werte
k_values <- c(1, 5, 10, 20)

# Wir erstellen eine leere Liste für spätere Ergebnisse
results_list <- list()

for(k in k_values) {
  # k-NN-Distanzen berechnen
  knn_distances <- knn.dist(data, k = k)
  knn_scores <- rowMeans(knn_distances)
  
  # Schwellenwert (z.B. 95%-Quantil)
  threshold <- quantile(knn_scores, 0.95)
  
  # Outlier-Prediction
  knn_pred <- ifelse(knn_scores > threshold, 1, 0)
  
  # Ergebnis als Data Frame speichern
  df_k <- data.frame(
    f1 = data[,1],
    f2 = data[,2],
    k = factor(k),               # k als Faktor, damit wir facetten können
    knnScore = knn_scores,
    isOutlier = factor(knn_pred, labels = c("Normal", "Outlier"))
  )
  
  # Dem results_list hinzufügen
  results_list[[paste0("k=", k)]] <- df_k
}

# Alle Data Frames zusammenfügen
combined_df <- do.call(rbind, results_list)

# Facet-Plot: Jede Facette entspricht einem k-Wert
ggplot(combined_df, aes(x = f1, y = f2, color = isOutlier)) +
  geom_point(size = 2) +
  facet_wrap(~ k) +
  scale_color_manual(values = c("Normal" = "gray60", "Outlier" = "red")) +
  labs(
    title = "Outlier Detection bei Variation von k (k-NN)",
    subtitle = "95%-Quantil als Schwellenwert",
    x = "Feature 1",
    y = "Feature 2",
    color = "Klasse"
  ) +
  theme_minimal()

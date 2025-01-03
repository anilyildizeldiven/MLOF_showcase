library(isotree)
library(dplyr)
library(ggplot2)

trees_values <- c(1, 5, 50)
iso_list <- list()

for(nt in trees_values) {
  iso_forest <- isolation.forest(data, ntrees = nt, ndim = 1)
  iso_scores <- predict(iso_forest, data)
  
  # Schwellenwert (z.B. 95%-Quantil)
  iso_threshold <- quantile(iso_scores, 0.95)
  
  # Outlier-Prediction
  iso_pred <- ifelse(iso_scores > iso_threshold, 1, 0)
  
  df_iso <- data.frame(
    f1 = data[,1],
    f2 = data[,2],
    n_trees = factor(nt),
    isoScore = iso_scores,
    isOutlier = factor(iso_pred, labels = c("Normal", "Outlier"))
  )
  
  iso_list[[paste0("ntrees=", nt)]] <- df_iso
}

combined_iso <- do.call(rbind, iso_list)

ggplot(combined_iso, aes(x = f1, y = f2, color = isOutlier)) +
  geom_point(size = 2) +
  facet_wrap(~ n_trees) +
  scale_color_manual(values = c("Normal" = "gray60", "Outlier" = "red")) +
  labs(
    title = "Outlier Detection bei Variation der Baumanzahl (Isolation Forest)",
    subtitle = "95%-Quantil als Schwellenwert",
    x = "Feature 1",
    y = "Feature 2",
    color = "Klasse"
  ) +
  theme_minimal()

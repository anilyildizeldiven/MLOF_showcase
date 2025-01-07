########
# context


# Install necessary library if not already installed
if (!require("DiagrammeR")) install.packages("DiagrammeR")

# Load the library
library(DiagrammeR)

# Create a graph for logical sequence
graph <- grViz("
digraph NSI_Editing {
  graph [layout = dot, rankdir = TB]

  node [shape = box, style = filled, color = lightblue, fontsize = 12]
  A [label = 'NSIs rely on surveys & admin data']
  B [label = 'Common Issues:\\n- Measurement Errors\\n- Missing Responses\\n- Contradictions']
  C [label = 'Goal:\\nFocus on relevant errors\\nReduce invalid data efficiently']
  D [label = 'Challenge:\\nAvoid over-editing\\nOptimize resource use']
  E [label = 'Outcome:\\nBetter data quality\\nAccurate results']

  # Define edges
  A -> B
  B -> C
  C -> D
  D -> E
}
")

# Render the graph
graph




############
# editing


# Install necessary library if not already installed
if (!require("DiagrammeR")) install.packages("DiagrammeR")

# Load the library
library(DiagrammeR)

# Create the graph for data processing value chain
graph <- grViz("
digraph Data_Processing {
  graph [layout = dot, rankdir = TB]

  # Node styles
  node [shape = box, style = filled, color = lightblue, fontsize = 12]
  
  # Nodes
  A [label = 'Modern Approaches']
  B1 [label = 'Interactive Editing\\nManual review by experts']
  B2 [label = 'Selective Editing\\nTargeted checks for high-error-risk records']
  B3 [label = 'Macro Editing\\nAggregated data checks for anomalies']
  B4 [label = 'Automatic Editing\\nAlgorithms detect and correct issues']
  C [label = 'ML Integration\\nPredictive models streamline error detection and correction']

  # Edges
  A -> {B1 B2 B3 B4}
  {B1 B2 B3 B4} -> C
}
")

# Render the graph
graph


### imputation


# Installiere und lade das DiagrammeR-Paket, falls noch nicht installiert
if (!require(DiagrammeR)) {
  install.packages("DiagrammeR")
  library(DiagrammeR)
}

# Definiere das Diagramm mit Graphviz Syntax
diagram_code <- "
digraph Imputation {
    # Einstellungen für das Layout
    graph [layout = dot, rankdir = TB, fontsize=12, fontname=Helvetica]
    
    # Knoten-Stil
    node [shape = rectangle, style = filled, color = LightBlue, fontname=Helvetica]
    
    # Hauptknoten
    Imputation [label = 'Imputation', shape = ellipse, color = LightGoldenrod]
    
    # Unterknoten
    ManualImputation [label = 'Manual Imputation:\nExpert knowledge or follow-up with respondents', shape = rectangle, color = LightBlue]
    
    AutomatedMethods [label = 'Automated Methods', shape = rectangle, color = LightBlue]
    
    # Unterknoten für Automated Methods
    RegressionModels [label = 'Regression Models\n(e.g., linear or logistic regression)', shape = rectangle, color = LightBlue]
    DonorMethods [label = 'Donor Methods\n(nearest neighbor, hot-deck)', shape = rectangle, color = LightBlue]
    MLBasedMethods [label = 'ML-Based Methods\n(e.g., Random Forest, Gradient Boosting)', shape = rectangle, color = LightBlue]
    
    # Verbindungen
    Imputation -> ManualImputation
    Imputation -> AutomatedMethods
    
    AutomatedMethods -> RegressionModels
    AutomatedMethods -> DonorMethods
    AutomatedMethods -> MLBasedMethods
}
"

# Erstelle das Diagramm
grViz(diagram_code)

# Optional: Speichern des Diagramms als PDF oder PNG
# Installiere und lade die notwendigen Pakete, falls noch nicht installiert
if (!require(DiagrammeRsvg)) {
  install.packages("DiagrammeRsvg")
  library(DiagrammeRsvg)
}

if (!require(rsvg)) {
  install.packages("rsvg")
  library(rsvg)
}

# Konvertiere das Diagramm in SVG
svg_code <- export_svg(grViz(diagram_code))

# Speichere das SVG als PDF
rsvg_pdf(charToRaw(svg_code), file = "Imputation.pdf")

# Oder speichere als PNG (erfordert ggf. zusätzliche Tools)
rsvg_png(charToRaw(svg_code), file = "Imputation.png")










#### beneifts and challnages

# Install necessary packages if not already installed
install.packages(c("ggplot2", "cowplot"))

# Load the packages
library(ggplot2)
library(cowplot)

# Data for Benefits and Challenges
benefits <- c(
  "• Automates and speeds up data processing",
  "• High precision in detecting errors and patterns",
  "• Highly adaptable to large, complex datasets"
)

challenges <- c(
  "• Requires sufficiently clean and representative training data",
  "• Model interpretability (the \"black-box\" problem)",
  "• High computational requirements for large datasets"
)

# Create data frames
df_benefits <- data.frame(Items = benefits)
df_challenges <- data.frame(Items = challenges)

# Plot for Benefits
plot_benefits <- ggplot(df_benefits, aes(x = 1, y = Items)) +
  geom_text(aes(label = Items), hjust = 0, size = 5, color = "#2E8B57") +  # SeaGreen color for text
  theme_void() +  # Remove background, grid, and axes
  labs(title = "Benefits") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "#2E8B57"),
    plot.background = element_rect(fill = "#F0FFF0", color = NA)  # Honeydew background
  )

# Plot for Challenges
plot_challenges <- ggplot(df_challenges, aes(x = 1, y = Items)) +
  geom_text(aes(label = Items), hjust = 0, size = 5, color = "#8B0000") +  # DarkRed color for text
  theme_void() +  # Remove background, grid, and axes
  labs(title = "Challenges") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold", color = "#8B0000"),
    plot.background = element_rect(fill = "#FFF0F5", color = NA)  # LavenderBlush background
  )

# Combine the two plots side by side
combined_plot <- plot_grid(
  plot_benefits, plot_challenges,
  labels = NULL,  # No labels
  ncol = 2,       # Two columns
  align = "h"     # Align horizontally
)

# Display the combined plot
print(combined_plot)

# Save as PNG
ggsave("ML_Benefits_Challenges.png", plot = combined_plot, width = 14, height = 8, dpi = 300)

# Save as PDF
ggsave("ML_Benefits_Challenges.pdf", plot = combined_plot, width = 14, height = 8)




######## optimizing imutaton process
library(DiagrammeR)

graph_code <- "
digraph ImputationOptimization {
  # Layout-Einstellungen
  graph [layout = dot, rankdir = LR]

  # Verbesserungen (Improvements) - Links
  node [shape = rectangle, style = filled, fillcolor = lightblue, fontname = Helvetica, fontsize = 10]
  SequentialImputation [label = <<b>Sequential Imputation</b><br/>Feature-by-feature imputation>]
  AdvancedModels [label = <<b>Advanced Models</b><br/>Boosting or Neural Networks>]
  CrossValidation [label = <<b>Cross-Validation</b><br/>Improve robustness>]
  FeatureEngineering [label = <<b>Feature Engineering</b><br/>Better feature relationships>]
  EnsembleMethods [label = <<b>Ensemble Methods</b><br/>Combine techniques>]

  # Ziele (Goals) - Rechts
  node [shape = rectangle, style = filled, fillcolor = green, fontname = Helvetica, fontsize = 10]
  IncreaseAccuracy [label = <<b>Increase Accuracy</b>>]
  PreserveVariance [label = <<b>Preserve Variance</b>>]
  EnhancePerformance [label = <<b>Enhance Performance</b>>]

  # Platzierung der Knoten
  {rank = same; SequentialImputation; AdvancedModels; CrossValidation; FeatureEngineering; EnsembleMethods}
  {rank = same; IncreaseAccuracy; PreserveVariance; EnhancePerformance}

  # Entfernen von individuellen Pfeilen und Hinzufügen eines großen Pfeils
  node [shape = point, width=0]
  invisible1 [label = '']
  invisible2 [label = '']

  invisible1 -> invisible2 [arrowhead = none]
  invisible1 -> invisible2 [label = '', penwidth = 3, arrowhead = normal]

  invisible1 -> {SequentialImputation AdvancedModels CrossValidation FeatureEngineering EnsembleMethods} [arrowhead = none]
  invisible2 -> {IncreaseAccuracy PreserveVariance EnhancePerformance} [arrowhead = none]
}
"

# Render das Diagramm
DiagrammeR::grViz(graph_code)

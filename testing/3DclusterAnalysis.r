# Define required packages
required_packages <- c("readxl", "tidyverse", "cluster", "factoextra", "ggplot2", "ggrepel", "FactoMineR", "plotly")

# Install and load required packages
for (package in required_packages) {
    if (!require(package, character.only = TRUE)) {
        install.packages(package, dependencies = TRUE)
    }
    library(package, character.only = TRUE)
}

# Define the results directory and sheet name
results_dir <- paste0("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/SIS_Analysis/PCA/pcaFollowedByCluster/", sheet_name)
sheet_name <- "DLSsingleSlim_noBatchMaleRef"

# Read in the data
data <- read_excel("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/SIS_Analysis/E9_Behavior_Data.xlsx", sheet = sheet_name)
susAnimals <- c(readLines(paste0("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/sus_animals.csv")))
conAnimals <- c(readLines(paste0("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/con_animals.csv")))
sheet_name <- "DLSsingleSlim_noBatchMaleRef"

# Create the results directory if it doesn't exist
if (!dir.exists(results_dir)) {
  dir.create(results_dir)
}

# Define if sex is included as a factor
sex_as_factor <- FALSE

# Rename the group of animals that have the Group "SIS" and are in the susAnimals list to "SUS". 
# If they are SIS but not in the susAnimals list, rename to "RES"
data$Group <- ifelse(data$Group == "SIS" & data$ID %in% susAnimals, "SUS", 
                     ifelse(data$Group == "SIS" & !data$ID %in% susAnimals, "RES", data$Group))

# Function to perform clustering and plotting
perform_clustering <- function(data, sex = NULL, sex_as_factor = TRUE, results_dir) {
  
  # Filter data for the specified sex if sex_as_factor is TRUE
  if (sex_as_factor) {
    cat("Processing for sex:", sex, "\n")
    data_sex <- data %>% filter(Sex == sex)
  } else {
    cat("Processing for all sexes combined\n")
    data_sex <- data
    print(data_sex)
  }
  head(data_sex)
  
  # Preserve the metadata (ID, Group, and Sex)
  metadata <- data_sex[, 1:4]  # Assuming the first 4 columns are ID, Group, Sex, and some other metadata

  # Extract columns for PCA (excluding metadata columns like ID, Group, etc.)
  data_for_pca <- data_sex[, -(1:4)]

  # Handle missing values
  data_for_pca <- data_for_pca %>% mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

  # Perform PCA
  pca_result <- PCA(data_for_pca, scale.unit = TRUE, ncp = 5)

  # Extract principal components
  pca_data <- as.data.frame(pca_result$ind$coord)

  # Combine PCA results with the original metadata
  data_sex <- cbind(metadata, pca_data)
  
  # Add interaction column for shape mapping
  data_sex$Sex_Group <- interaction(data_sex$Sex, data_sex$Group)
  print(data_sex)
  
  # Save Variable Contribution Plots for each of the top 5 dimensions
  for (dim in 1:5) {
    contrib_plot <- fviz_contrib(pca_result, choice = "var", axes = dim, top = 10) + 
      labs(title = paste("Variable Contribution - PC", dim, ifelse(sex_as_factor, paste("for sex", sex), "for all sexes combined")))
    
    # Print the contribution plot
    print(contrib_plot)
    
    # Save the plot
    ggsave(filename = file.path(results_dir, paste0("variable_contrib_PC", dim, ifelse(sex_as_factor, paste("_", sex), "_combined"), ".svg")), plot = contrib_plot)
  }

  # Determine the optimal number of clusters using the Elbow method
  set.seed(42)
  elbow_plot <- fviz_nbclust(pca_data, kmeans, method = "wss") +
    geom_vline(xintercept = 3, linetype = 2) +
    labs(subtitle = ifelse(sex_as_factor, paste("Elbow method for sex", sex), "Elbow method for all sexes combined"))

  print(elbow_plot)

  # Save the elbow plot
  ggsave(filename = file.path(results_dir, paste0("elbow_plot_", ifelse(sex_as_factor, sex, "combined"), ".svg")))

  # Perform K-Means clustering with the optimal number of clusters
  optimal_clusters <- 4  # Adjust based on elbow plot
  set.seed(42)
  kmeans_result <- kmeans(pca_data, centers = optimal_clusters, nstart = 25)

  # Add the cluster labels to the original data
  data_sex$Cluster <- as.factor(kmeans_result$cluster)

  # Save clustering results to a CSV file
  write_csv(data_sex[, c("ID", "Group", "Sex", "Cluster")], file.path(results_dir, paste0("clustering_results_", ifelse(sex_as_factor, sex, "combined"), ".csv")))

  # Calculate percentages of animals in each group for each cluster
  cluster_stats <- data_sex %>%
    group_by(Group, Cluster) %>%
    summarise(Percentage = n() / nrow(data_sex) * 100, .groups = "drop")

  # Calculate silhouette widths
  sil_width <- silhouette(kmeans_result$cluster, dist(pca_data))

  # Plot silhouette plot
  silhouette_plot <- fviz_silhouette(sil_width) +
    labs(title = ifelse(sex_as_factor, paste("Silhouette plot for sex", sex), "Silhouette plot for all sexes combined"))

  print(silhouette_plot)

  # Save the silhouette plot
  ggsave(filename = file.path(results_dir, paste0("silhouette_plot_", ifelse(sex_as_factor, sex, "combined"), ".svg")))

  # Plot PCA for visualization
  pca_data$Cluster <- data_sex$Cluster
  pca_data$Group <- data_sex$Group  # Include Group information
  pca_data$Sex_Group <- data_sex$Sex_Group  # Include Sex_Group information for shapes

  # Perform PCA (assuming pca_result has already been calculated)
  variance_explained <- pca_result$eig[, 2]  # Extract the percentage of variance explained

  # Define shapes
  shapes <- c("m.RES" = 16,   # Closed circle for males in RES group
              "m.SUS" = 17,   # Closed triangle for males in SUS group
              "m.CON" = 15,   # Closed square for males in Con group
              "f.RES" = 1,    # Open circle for females in RES group
              "f.SUS" = 2,    # Open triangle for females in SUS group
              "f.CON" = 0)    # Open square for females in Con group

  # Map shapes to symbols for plotly
  plotly_shapes <- c("m.RES" = 16,   # Closed circle for males in RES group
                     "m.SUS" = 17,   # Closed triangle for males in SUS group
                     "m.CON" = 15,   # Closed square for males in Con group
                     "f.RES" = 1,    # Open circle for females in RES group
                     "f.SUS" = 2,    # Open triangle for females in SUS group
                     "f.CON" = 0)    # Open square for females in Con group

  # Create the 3D PCA plot using plotly
  pca_plot_3d <- plot_ly(data = pca_data, 
                         x = ~Dim.1, 
                         y = ~Dim.2, 
                         z = ~Dim.3, 
                         color = ~Cluster, 
                         symbol = ~Sex_Group, 
                         symbols = plotly_shapes, 
                         marker = list(
                           size = ifelse(pca_data$Sex_Group %in% c("m.RES", "m.SUS", "m.CON"), 10, 8),  # Larger size for male groups
                           line = list(width = ifelse(pca_data$Sex_Group %in% c("f.RES", "f.SUS", "f.CON"), 4, 2))  # Thicker outline for female groups
                         ),
                         colors = c("#FF7E47", "#8ACE00", "#FFBB49", "#6BCAEB")) %>%
    add_markers() %>%
    layout(scene = list(
      xaxis = list(title = paste0("PC1 (", round(variance_explained[1], 1), "%)")),
      yaxis = list(title = paste0("PC2 (", round(variance_explained[2], 1), "%)")),
      zaxis = list(title = paste0("PC3 (", round(variance_explained[3], 1), "%)"))
    ),
    title = ifelse(sex_as_factor, 
                   paste("3D PCA Visualization with Clusters for sex", sex), 
                   "3D PCA Visualization with Clusters for all sexes combined"))

  # Print the 3D plot
  print(pca_plot_3d)

  # Save the PCA plot
  ggsave(filename = file.path(results_dir, paste0("pca_visualization_", ifelse(sex_as_factor, sex, "combined"), ".svg")))
}

# Perform clustering based on whether sex is a factor
if (sex_as_factor) {
  unique_sex <- unique(data$Sex)
  for (sex in unique_sex) {
    perform_clustering(data, sex, sex_as_factor, results_dir)
  }
} else {
  # Perform clustering for all data combined (without sex as a factor)
  perform_clustering(data, sex_as_factor = FALSE, results_dir = results_dir)
}
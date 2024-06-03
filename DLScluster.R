# Install necessary packages
required_packages <- c("ggplot2", "cowplot", "colorspace", "dplyr", "factoextra", "FactoMineR", "kernlab", "readxl", "tidyr", "GGally", "topicmodels", "missMDA", "lda", "MASS", "broom", "missForest", "readxl")

for (package in required_packages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)
  } else {
    library(package, character.only = TRUE)
  }
}

# Load necessary packages
library(ggplot2)
library(cowplot)
library(colorspace)
library(dplyr)
library(factoextra)
library(FactoMineR)
library(kernlab)
library(readxl)
library(tidyr)
library(topicmodels)
library(missMDA)
library(lda)

# define output folder
results_dir <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/SIS_Analysis/PCA/pcaFollowedByCluster/"
# Read in data
sheet_name <- "DLSsingleSlim"
data <- read_excel("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/SIS_Analysis/E9_Behavior_Data.xlsx", sheet = sheet_name)
susAnimals <- c(readLines(paste0("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/sus_animals.csv")))
conAnimals <- c(readLines(paste0("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/con_animals.csv")))

# rename the group of animals that have the Group "SIS" and are in the susAnimals list to "SUS". if they are SIS but not in the susAnimals list, rename to "RES"
data$Group <- ifelse(data$Group == "SIS" & data$ID %in% susAnimals, "SUS", ifelse(data$Group == "SIS" & !data$ID %in% susAnimals, "RES", data$Group))

# Define a function to extract optimal number of PCs
get_optimal_pcs <- function(pca_obj) {
  # Get the percentage of variance explained by each PC
  var_prop <- pca_obj$eig / sum(pca_obj$eig)
  
  # Calculate cumulative proportion of variance explained
  cum_var_prop <- cumsum(var_prop)
  
  # Find the number of PCs needed to explain 95% of the variance
  num_pcs <- min(which(cum_var_prop >= 0.95))
  
  # Return the optimal number of PCs
  return(num_pcs)
}

# Run PCA and extract optimal number of PCs for all data
pca_data <- PCA(data[, -c(1:4, which(names(data) == "CombZ"))], graph = FALSE)
optimal_pcs <- get_optimal_pcs(pca_data)

# Define color palette
group_cols <- c("#1e3791","#00ac8c","#F79719","#1e3791","#00ac8c","#F79719")

# Plot PCA with optimal number of PCs for all data
pca_plot <- fviz_pca_ind(pca_data, 
             label = "none", 
             habillage = interaction(data$Group, data$Sex), 
             palette = group_cols, 
             addEllipses = TRUE, 
             axes = c(1, 2), # only plot PC1 and PC2 on x and y axis
             choose.var = optimal_pcs # plot optimal number of PCs
)

# Save the PCA plot for all data
ggsave(file.path(results_dir, paste0("pca_plot_", sheet_name, ".svg")), pca_plot, dpi = 300, height = 5, width = 6)

# Perform separate PCA for males (m)
male_data <- data[data$Sex == "m", ]
male_pca_data <- PCA(male_data[, -c(1:4, which(names(male_data) == "CombZ"))], graph = FALSE)
male_optimal_pcs <- get_optimal_pcs(male_pca_data)

# Define color palette for males
male_group_cols <- group_cols[1:3]

# Plot PCA with optimal number of PCs for males (m)
male_pca_plot <- fviz_pca_ind(male_pca_data, 
  label = "none", 
  habillage = interaction(male_data$Group, male_data$Sex), 
  palette = male_group_cols, 
  addEllipses = TRUE, 
  axes = c(1, 2), # only plot PC1 and PC2 on x and y axis
  choose.var = male_optimal_pcs # plot optimal number of PCs
)

# Save the PCA plot for males (m)
ggsave(file.path(results_dir, paste0("pca_plot_males_", sheet_name, ".svg")), male_pca_plot, dpi = 300, height = 5, width = 6)

# Perform separate PCA for females (f)
female_data <- data[data$Sex == "f", ]
female_pca_data <- PCA(female_data[, -c(1:4, which(names(female_data) == "CombZ"))], graph = FALSE)
female_optimal_pcs <- get_optimal_pcs(female_pca_data)

# Define color palette for males
female_group_cols <- group_cols[1:3]

# Plot PCA with optimal number of PCs for females (f)
female_pca_plot <- fviz_pca_ind(female_pca_data, 
                label = "none", 
                habillage = interaction(female_data$Group, female_data$Sex), 
                palette = group_cols[4:6], 
                addEllipses = TRUE, 
                axes = c(1, 2), # only plot PC1 and PC2 on x and y axis
                choose.var = female_optimal_pcs # plot optimal number of PCs
)

# Save the PCA plot for females (f)
ggsave(file.path(results_dir, paste0("pca_plot_females_", sheet_name, ".svg")), female_pca_plot, dpi = 300, height = 5, width = 6)

# Generate variable contribution plots for each sex and the first 5 principal components
for (sex in unique(data$Sex)) {
  sex_data <- data[data$Sex == sex, ]
  sex_pca_data <- PCA(sex_data[, -c(1:4, which(names(sex_data) == "CombZ"))], graph = FALSE)
  
  for (i in 1:5) {
    var_contrib <- fviz_contrib(sex_pca_data, choice = "var", axes = i, top = 10)
    
    # Modify the plot appearance 
    var_contrib <- var_contrib +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        axis.title.x = element_blank(),  # Hide x-axis title
        axis.title.y = element_text(size = 12),
        axis.text = element_text(size = 10, face = "bold"),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        plot.margin = margin(1, 1, 1, 1, "cm")
      )
    
    # Save the variable contribution plot
    ggsave(file.path(results_dir, paste0("var_contrib_", data$sheet_name, "_", sex, "_", i, ".svg")), var_contrib, dpi = 300, height = 5, width = 6)
  }
}

# Plot the PCA plot
print(pca_plot)


###############################################################################
# Perform k-means clustering on the PCA scores
install.packages("NbClust")
library(NbClust)

fviz_nbclust(pca_data, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")


k <- 3  # number of clusters
set.seed(123)  # for reproducibility
kmeans_result <- kmeans(pca_data$ind$coord[,1:2], centers = k)

# Create an empty plot
library(factoextra)
library(ggplot2)
library(GGally)
library(dplyr)
library(GGally)

# Create parallel coordinates plot
parallel_coord <- ggparcoord(pca_data$ind$coord, columns = 1:4, groupColumn = "cluster",
             showPoints = TRUE, alphaLines = 0.5, alphaPoints = 0.5) +
  theme_minimal() +
  theme(legend.position = "none",
    plot.margin = unit(c(1, 1, 1, 1), "cm"))

# Create k-means clustering plot
cluster_plot <- fviz_cluster(list(data = pca_data$ind$coord[,1:2], cluster = kmeans_result$cluster),
         label = "none", # hide individual labels
         geom = "point",
         ellipse.type = "convex",
         palette = c("#00ac8c", "#1e3791", "#3e6481"),
         main = paste("k-means clustering (k =", k, ")"),
         addlabels = FALSE
) + theme_minimal() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
    legend.position = "top",
    legend.justification = "right",
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
  theme(panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())

# Add labels for the individual IDs
text(pca_data$ind$coord[,1], pca_data$ind$coord[,2], labels = data$ID)

# Save the PCA plot
ggsave(file.path(results_dir, "pca_plot.png"), pca_plot)

# Show the plot
pca_plot

# save the cluster plot as SVG
ggsave(file.path(results_dir, "cluster_plot.svg"), cluster_plot)

# Biplot of PCA
fviz_pca_biplot(pca_data, repel = TRUE)



fviz_nbclust(pca_data$ind$coord, kmeans, method = 'wss')



















# Perform k-means clustering on the PCA scores
install.packages("NbClust")
library(NbClust)

#

# Perform clustering on males
m <- subset(pca_data$ind$coord, Sex == "M")
k_males <- 3  # number of clusters for males
set.seed(123)  # for reproducibility
kmeans_result_males <- kmeans(m[,1:2], centers = k_males)
# Perform fviz_nbclust for males of the subset data
fviz_nbclust(m , kmeans, method = 'wss')

# Perform clustering on females
f <- subset(pca_data$ind$coord, Sex == "F")
k_females <- 3  # number of clusters for females
set.seed(123)  # for reproducibility
kmeans_result_females <- kmeans(f[,1:2], centers = k_females)

# Create an empty plot
library(factoextra)
library(ggplot2)
library(GGally)
library(dplyr)
library(GGally)

# Create parallel coordinates plot for males
parallel_coord_males <- ggparcoord(m, columns = 1:4, groupColumn = "cluster",
             showPoints = TRUE, alphaLines = 0.5, alphaPoints = 0.5) +
  theme_minimal() +
  theme(legend.position = "none",
    plot.margin = unit(c(1, 1, 1, 1), "cm"))

# Create parallel coordinates plot for females
parallel_coord_females <- ggparcoord(f, columns = 1:4, groupColumn = "cluster",
             showPoints = TRUE, alphaLines = 0.5, alphaPoints = 0.5) +
  theme_minimal() +
  theme(legend.position = "none",
    plot.margin = unit(c(1, 1, 1, 1), "cm"))

# Create k-means clustering plot for males
cluster_plot_males <- fviz_cluster(list(data = m[,1:2], cluster = kmeans_result_males$cluster),
         label = "none", # hide individual labels
         geom = "point",
         ellipse.type = "convex",
         palette = c("#00ac8c", "#1e3791", "#3e6481"),
         main = paste("k-means clustering for males (k =", k_males, ")"),
         addlabels = FALSE
) + theme_minimal() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
    legend.position = "top",
    legend.justification = "right",
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
  theme(panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())

# Create k-means clustering plot for females
cluster_plot_females <- fviz_cluster(list(data = f[,1:2], cluster = kmeans_result_females$cluster),
         label = "none", # hide individual labels
         geom = "point",
         ellipse.type = "convex",
         palette = c("#00ac8c", "#1e3791", "#3e6481"),
         main = paste("k-means clustering for females (k =", k_females, ")"),
         addlabels = FALSE
) + theme_minimal() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
    legend.position = "top",
    legend.justification = "right",
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
  theme(panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())

# Add labels for the individual IDs for males
text(m[,1], m[,2], labels = data$ID[Sex == "M"])

# Add labels for the individual IDs for females
text(f[,1], f[,2], labels = data$ID[Sex == "F"])

# Save the PCA plot for males
ggsave(file.path(results_dir, "pca_plot_males.png"), pca_plot)

# Save the PCA plot for females
ggsave(file.path(results_dir, "pca_plot_females.png"), pca_plot)

# Save the cluster plot for males as SVG
ggsave(file.path(results_dir, "cluster_plot_males.svg"), cluster_plot_males)

# Save the cluster plot for females as SVG
ggsave(file.path(results_dir, "cluster_plot_females.svg"), cluster_plot_females)

# Biplot of PCA for males
fviz_pca_biplot(m, repel = TRUE)

# Biplot of PCA for females
fviz_pca_biplot(f, repel = TRUE)

# Perform fviz_nbclust for males
fviz_nbclust(pca_data$ind$coord[Sex == "M", ], kmeans, method = 'wss')

# Perform fviz_nbclust for females
fviz_nbclust(pca_data$ind$coord[Sex == "F", ], kmeans, method = 'wss')
































































# Load required libraries
library(factoextra)
library(ggplot2)
library(dplyr)
library(cluster)

# Perform PCA with additional factors
pca_data <- PCA(data[, !(colnames(data) %in% c("ID", "Batch"))], scale.unit = TRUE, graph = FALSE, quali.sup = c("Group", "Sex"))

# Perform k-means clustering for different values of k
k_values <- 2  # Range of values for k
wss <- numeric(length(k_values))  # Initialize within-cluster sum of squares
sil <- numeric(length(k_values))  # Initialize silhouette scores

set.seed(123)  # For reproducibility
for (i in seq_along(k_values)) {
  kmeans_result <- kmeans(pca_data$ind$coord[,1:2], centers = k_values[i])
  wss[i] <- kmeans_result$tot.withinss
  cluster_factor <- as.factor(kmeans_result$cluster)  # Convert cluster to factor
  
  # Compute the distance matrix based on clustered data
  dist_matrix <- dist(pca_data$ind$coord[kmeans_result$cluster == cluster_factor, 1:2])
  
  # Compute silhouette
  sil_obj <- silhouette(as.numeric(cluster_factor), dist_matrix)
  
  # Print class of silhouette object
  print(class(sil_obj))
}


# Plot within-cluster sum of squares (Elbow method)
plot(k_values, wss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of Clusters (k)", ylab = "Within-cluster Sum of Squares")
title("Elbow Method")

# Plot silhouette scores
plot(k_values, sil, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of Clusters (k)", ylab = "Silhouette Score")
title("Silhouette Analysis")

# Determine the ideal number of clusters using the elbow method or silhouette scores
# Adjust the following code based on the chosen method

# Elbow method - find the "elbow" point where the decrease in wss starts to flatten
elbow_point <- 2  # Update with the identified elbow point

# Silhouette analysis - find the maximum silhouette score
max_sil_index <- which.max(sil)
optimal_k <- k_values[max_sil_index]

# Perform clustering with the ideal number of clusters
set.seed(123)  # For reproducibility
kmeans_result <- kmeans(pca_data$ind$coord[,1:2], centers = 2)

# Create parallel coordinates plot
parallel_coord <- ggparcoord(pca_data$ind$coord, columns = 1:4, groupColumn = "cluster",
             showPoints = TRUE, alphaLines = 0.5, alphaPoints = 0.5) +
  theme_minimal() +
  theme(legend.position = "none",
    plot.margin = unit(c(1, 1, 1, 1), "cm"))

# Create k-means clustering plot
cluster_plot <- fviz_cluster(list(data = pca_data$ind$coord[,1:2], cluster = kmeans_result$cluster),
         label = "none", # hide individual labels
         geom = "point",
         ellipse.type = "convex",
         palette = c("#00ac8c", "#1e3791", "#3e6481"),
         main = paste("k-means clustering (k =", optimal_k, ")"),
         addlabels = FALSE
) + theme_minimal() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
    legend.position = "top",
    legend.justification = "right",
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
  theme(panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())

# Add labels for the individual IDs
text(pca_data$ind$coord[,1], pca_data$ind$coord[,2], labels = data$ID)

# Save the cluster plot as SVG
ggsave(file.path(results_dir, "cluster_plot.svg"), cluster_plot)

# Perform clustering for each "Sex" factor (males and females)
set.seed(123)  # For reproducibility
kmeans_male <- kmeans(male_pca_data$ind$coord[, 1:2], centers = 2)
kmeans_female <- kmeans(female_pca_data$ind$coord[, 1:2], centers = 2)

# Create k-means clustering plot for males
cluster_plot_male <- fviz_cluster(list(data = male_pca_data$ind$coord[, 1:2], cluster = kmeans_male$cluster),
         label = "none", # hide individual labels
         geom = "point",
         ellipse.type = "convex",
         palette = c("#FF7E47", "#FFBB49", "#6BCAEB"),
         main = "k-means clustering (males)",
         addlabels = FALSE
) + theme_minimal() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
    legend.position = "top",
    legend.justification = "right",
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
  theme(panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())

# Create k-means clustering plot for females
cluster_plot_female <- fviz_cluster(list(data = female_pca_data$ind$coord[, 1:2], cluster = kmeans_female$cluster),
         label = "none", # hide individual labels
         geom = "point",
         ellipse.type = "convex",
         palette = c("#FF7E47", "#FFBB49", "#6BCAEB"),
         main = "k-means clustering (females)",
         addlabels = FALSE
) + theme_minimal() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
    legend.position = "top",
    legend.justification = "right",
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
  theme(panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())

# Save the cluster plots for males and females as SVG
ggsave(file.path(results_dir, "cluster_plot_male.svg"), cluster_plot_male, dpi = 300, height = 5, width = 5)
ggsave(file.path(results_dir, "cluster_plot_female.svg"), cluster_plot_female, dpi = 300, height = 5, width = 5)




# cluster analysis on raw data, no PCA beforehand
# Load necessary libraries
library(readxl)
library(tidyverse)
library(cluster)
library(factoextra)
library(ggplot2)
library(ggrepel)

# Define the results directory and sheet name
results_dir <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/SIS_Analysis/PCA/pcaFollowedByCluster/"
sheet_name <- "DLSsingleSlim"

# Read in the data
data <- read_excel("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/SIS_Analysis/E9_Behavior_Data.xlsx", sheet = sheet_name)
susAnimals <- c(readLines(paste0("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/sus_animals.csv")))
conAnimals <- c(readLines(paste0("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/con_animals.csv")))

# Rename the group of animals that have the Group "SIS" and are in the susAnimals list to "SUS". If they are SIS but not in the susAnimals list, rename to "RES"
data$Group <- ifelse(data$Group == "SIS" & data$ID %in% susAnimals, "SUS", ifelse(data$Group == "SIS" & !data$ID %in% susAnimals, "RES", data$Group))

# Function to perform clustering and plotting for each sex
perform_clustering <- function(data, sex) {
  cat("Processing for sex:", sex, "\n")
  
  # Filter data for the specified sex
  data_sex <- data %>% filter(Sex == sex)
  
  # Extract columns for clustering (excluding the first four columns)
  data_for_clustering_sex <- data_sex[, -(1:4)]
  
  # Handle missing values
  data_for_clustering_sex <- data_for_clustering_sex %>%
    mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
  
  # Standardize the data
  scaled_data_sex <- scale(data_for_clustering_sex)
  
  # Determine the optimal number of clusters using the Elbow method
  set.seed(42)
  elbow_plot <- fviz_nbclust(scaled_data_sex, kmeans, method = "wss") +
    geom_vline(xintercept = 3, linetype = 2) +
    labs(subtitle = paste("Elbow method for sex", sex))
  
  print(elbow_plot)
  
  # Save the elbow plot
  ggsave(filename = file.path(results_dir, paste0("elbow_plot_", sex, ".svg")), plot = elbow_plot, width = 8, height = 4)
  
  # Perform K-Means clustering with the optimal number of clusters
  optimal_clusters <- 3  # Adjust based on elbow plot
  set.seed(42)
  kmeans_result <- kmeans(scaled_data_sex, centers = optimal_clusters, nstart = 25)
  
  # Add the cluster labels to the original data
  data_sex$Cluster <- as.factor(kmeans_result$cluster)
  
  # Save clustering results to a CSV file
  write_csv(data_sex %>% select(ID, Group, Sex, Cluster), file.path(results_dir, paste0("clustering_results_", sex, ".csv")))
  
  # Calculate percentages of animals in each group for each cluster
  cluster_stats <- data_sex %>%
    group_by(Group, Cluster) %>%
    summarise(Percentage = n() / nrow(data_sex) * 100)
  
  # Calculate silhouette widths
  sil_width <- silhouette(kmeans_result$cluster, dist(scaled_data_sex))
  
  # Plot silhouette plot
  silhouette_plot <- fviz_silhouette(sil_width) +
    labs(title = paste("Silhouette plot for sex", sex))
  
  print(silhouette_plot)
  
  # Save the silhouette plot
  ggsave(filename = file.path(results_dir, paste0("silhouette_plot_", sex, ".svg")), plot = silhouette_plot, width = 6, height = 4)
  
  # PCA for visualization
  pca_result <- prcomp(scaled_data_sex, scale. = TRUE)
  pca_data <- as.data.frame(pca_result$x)
  pca_data$Cluster <- data_sex$Cluster
  pca_data$Group <- data_sex$Group  # Include Group information
  
  cluster_plot <- ggplot(pca_data, aes(x = PC1, y = PC2, color = Cluster, shape = Group)) +
    geom_point(size = 3) +
    labs(title = paste("Clusters Visualization for sex", sex),
         x = "Principal Component 1",
         y = "Principal Component 2",
         shape = "Group") +
    theme_minimal() 
  print(cluster_plot)
  
  # Save the cluster plot
  ggsave(filename = file.path(results_dir, paste0("clusters_visualization_", sex, ".svg")), plot = cluster_plot, width = 5, height = 4)
}

# Perform clustering for each sex group
unique_sex <- unique(data$Sex)
for (sex in unique_sex) {
  perform_clustering(data, sex)
}












## Cluster data on PCA
# Load necessary libraries
library(readxl)
library(tidyverse)
library(cluster)
library(factoextra)
library(ggplot2)
library(ggrepel)
library(FactoMineR)

# Define the results directory and sheet name
results_dir <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/SIS_Analysis/PCA/pcaFollowedByCluster/"
sheet_name <- "DLSsingleSlim"

# Read in the data
data <- read_excel("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/SIS_Analysis/E9_Behavior_Data.xlsx", sheet = sheet_name)
susAnimals <- c(readLines(paste0("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/sus_animals.csv")))
conAnimals <- c(readLines(paste0("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/con_animals.csv")))

# Rename the group of animals that have the Group "SIS" and are in the susAnimals list to "SUS". If they are SIS but not in the susAnimals list, rename to "RES"
data$Group <- ifelse(data$Group == "SIS" & data$ID %in% susAnimals, "SUS", ifelse(data$Group == "SIS" & !data$ID %in% susAnimals, "RES", data$Group))

# Function to perform clustering and plotting for each sex
perform_clustering <- function(data, sex) {
  cat("Processing for sex:", sex, "\n")
  
  # Filter data for the specified sex
  filtered_data <- data %>% filter(Sex == sex)
  
  # Extract columns for PCA
  data_for_pca <- data_sex[, -(1:4)]
  
  # Handle missing values
  data_for_pca <- data_for_pca %>%
    mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
  
  # PCA
  pca_result <- PCA(data_for_pca, scale.unit = TRUE, ncp = 2)
  filtered_data <- as.data.frame(pca_result$ind$coord)  # Extract principal component scores
  
  # Determine the optimal number of clusters using the Elbow method
  set.seed(42)
  elbow_plot <- fviz_nbclust(pca_data, kmeans, method = "wss") +
    geom_vline(xintercept = 3, linetype = 2) +
    labs(subtitle = paste("Elbow method for sex", sex))
  
  print(elbow_plot)
  
  # Save the elbow plot
  ggsave(filename = file.path(results_dir, paste0("elbow_plot_", sex, ".svg")), plot = elbow_plot, width = 8, height = 4)
  
  # Perform K-Means clustering with the optimal number of clusters
  optimal_clusters <- 3  # Adjust based on elbow plot
  set.seed(42)
  kmeans_result <- kmeans(pca_data, centers = optimal_clusters, nstart = 25)
  
  # Add the cluster labels to the original data
  data_sex$Cluster <- as.factor(kmeans_result$cluster)
  
  # Save clustering results to a CSV file
  write_csv(data_sex[, c("ID", "Group", "Sex", "Cluster")], file.path(results_dir, paste0("clustering_results_", sex, ".csv")))
  
  # Calculate percentages of animals in each group for each cluster
  cluster_stats <- data_sex %>%
    group_by(Group, Cluster) %>%
    summarise(Percentage = n() / nrow(data_sex) * 100)
  
  # Calculate silhouette widths
  sil_width <- silhouette(kmeans_result$cluster, dist(pca_data))
  
  # Plot silhouette plot
  silhouette_plot <- fviz_silhouette(sil_width) +
    labs(title = paste("Silhouette plot for sex", sex))
  
  print(silhouette_plot)
  
  # Save the silhouette plot
  ggsave(filename = file.path(results_dir, paste0("silhouette_plot_", sex, ".svg")), plot = silhouette_plot, width = 6, height = 4)
  
  # Plot PCA for visualization
  pca_data$Cluster <- data_sex$Cluster
  pca_data$Group <- data_sex$Group  # Include Group information
  
  pca_plot <- ggplot(pca_data, aes(x = Dim.1, y = Dim.2, color = Cluster, shape = Group)) +
    geom_point(size = 3) +
    labs(title = paste("PCA Visualization with Clusters for sex", sex),
         x = "Principal Component 1",
         y = "Principal Component 2",
         shape = "Group") +
    theme_minimal() +
    scale_color_manual(values = c("#FF7E47", "#FFBB49", "#6BCAEB"))  # Set color palette
  
  print(pca_plot)
  
  # Save the PCA plot
  ggsave(filename = file.path(results_dir, paste0("pca_visualization_", sex, ".svg")), plot = pca_plot, width = 5, height = 4)
}

# Perform clustering for each sex group
unique_sex <- unique(data$Sex)
for (sex in unique_sex) {
  perform_clustering(data, sex)
}




























# Load required libraries
library(factoextra)
library(ggplot2)
library(dplyr)
library(cluster)
library(fpc)
library(dbscan)

# Perform k-means clustering for different values of k
k_values <- 1:10  # Range of values for k
wss <- numeric(length(k_values))  # Initialize within-cluster sum of squares
sil <- numeric(length(k_values))  # Initialize silhouette scores

for (i in seq_along(k_values)) {
  set.seed(123)  # For reproducibility
  kmeans_result <- kmeans(DLSzscore$ind$coord[,1:2], centers = k_values[i])
  wss[i] <- kmeans_result$tot.withinss
  sil[i] <- silhouette(kmeans_result$cluster, dist(DLSzscore$ind$coord[,1:2]))$avg.width
}

# Plot within-cluster sum of squares (Elbow method)
plot(k_values, wss, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of Clusters (k)", ylab = "Within-cluster Sum of Squares")
title("Elbow Method")

# Plot silhouette scores
plot(k_values, sil, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of Clusters (k)", ylab = "Silhouette Score")
title("Silhouette Analysis")

# Determine the ideal number of clusters using the silhouette analysis
optimal_k <- k_values[which.max(sil)]

# Perform clustering with the ideal number of clusters using k-means
set.seed(123)  # For reproducibility
kmeans_result <- kmeans(DLSzscore$ind$coord[,1:2], centers = optimal_k)

# Create parallel coordinates plot
parallel_coord <- ggparcoord(DLSzscore$ind$coord, columns = 1:4, groupColumn = "cluster",
                             showPoints = TRUE, alphaLines = 0.5, alphaPoints = 0.5) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.margin = unit(c(1, 1, 1, 1), "cm"))

# Create k-means clustering plot
pca_plot <- fviz_cluster(list(data = DLSzscore$ind$coord[,1:2], cluster = kmeans_result$cluster),
                         label = "none", # hide individual labels
                         geom = "point",
                         ellipse.type = "convex",
                         palette = c("#00ac8c", "#1e3791", "#3e6481"),
                         main = paste("k-means clustering (k =", optimal_k, ")"),
                         addlabels = FALSE
) + theme_minimal() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        legend.position = "top",
        legend.justification = "right",
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Add labels for the individual IDs
text(DLSzscore$ind$coord[,1], DLSzscore$ind$coord[,2], labels = DLSzscore$ind$ID)

# Show the plot
pca_plot

# Perform DBSCAN clustering
dbscan_result <- dbscan(DLSzscore$ind$coord[, 1:2], eps = 0.5, minPts = 5)

# Create DBSCAN clustering plot
dbscan_plot <- fviz_cluster(
  list(data = DLSzscore$ind$coord[, 1:2], cluster = dbscan_result$cluster),
  label = "none", # hide individual labels
  geom = "point",
  main = "DBSCAN Clustering",
  palette = c("#00ac8c", "#1e3791", "#3e6481")
) + theme_minimal() +
  theme(
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    legend.position = "top",
    legend.justification = "right",
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0)
  ) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Add labels for the individual IDs
text(DLSzscore$ind$coord[, 1], DLSzscore$ind$coord[, 2], labels = DLSzscore$ind$ID)

# Show the plot
dbscan_plot






























library(factoextra)
library(gridExtra)
library(cluster)
library(cowplot)

# Standardize the data
DLSzscore <- scale(E9_Behavior_Data[,5:8])

# Perform k-means clustering for different values of k
wss <- sapply(1:10, function(k) kmeans(DLSzscore, centers=k)$tot.withinss)

# Plot the WSS for each value of k
plot(1:10, wss, type="b", xlab="Number of clusters (k)", ylab="Within-cluster sum of squares")

# Identify the elbow point
elbow <- kElbowPlot(wss, ndims = 1)

# Re-run k-means clustering with the identified number of clusters
k <- elbow$Best_nc
set.seed(123)
kmeans_result <- kmeans(DLSzscore, centers = k)

# Create parallel coordinates plot
parallel_coord <- ggparcoord(DLSzscore, columns = 1:4, groupColumn = "cluster",
                             showPoints = TRUE, alphaLines = 0.5) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.margin = unit(c(1, 1, 1, 1), "cm"))

# Create k-means clustering plot
pca_plot <- fviz_cluster(list(data = DLSzscore, cluster = kmeans_result$cluster),
                         label = "none", # hide individual labels
                         geom = "point",
                         ellipse.type = "convex",
                         palette = c("#00ac8c", "#1e3791", "#3e6481"),
                         main = paste("k-means clustering (k =", k, ")"),
                         addlabels = FALSE
) + theme_minimal() +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        legend.position = "top",
        legend.justification = "right",
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Add labels for the individual IDs
text(DLSzscore[,1], DLSzscore[,2], labels = E9_Behavior_Data$ID)

# Create a plot of variable contributions
contrib_plot <- fviz_contrib(kmeans_result, choice = "var", axes = 1:2)

# Arrange the plots in a grid
grid_arrange_shared_legend(pca_plot, parallel_coord, contrib_plot, ncol = 3, widths = c(2, 2, 1))





































































# Load necessary packages
library(ggplot2)
library(cowplot)
library(colorspace)
library(dplyr)
library(factoextra)
library(FactoMineR)
library(kernlab)
library(readxl)
library(tidyr)

# Read in data
E9_Behavior_Data <- read_excel("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/SIS_Analysis/E9_Behavior_Data.xlsx", sheet = "DLS")

# Run PCA and extract optimal number of PCs
DLSzscore <- PCA(E9_Behavior_Data[,c(-1:-2)], graph = FALSE)

# Define a function to extract optimal number of PCs
get_optimal_pcs <- function(pca_obj) {
  # Get the percentage of variance explained by each PC
  var_prop <- pca_obj$eig / sum(pca_obj$eig)
  
  # Calculate cumulative proportion of variance explained
  cum_var_prop <- cumsum(var_prop)
  
  # Find the number of PCs needed to explain 95% of the variance
  num_pcs <- min(which(cum_var_prop >= 0.95))
  
  # Return the optimal number of PCs
  return(num_pcs)
}

optimal_pcs <- get_optimal_pcs(DLSzscore)

# Define color palette
group_cols <- c("#1e3791","#00ac8c")

plot_theme <- theme_minimal() +
  theme(
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    legend.position = "top", # set legend to top
    legend.justification = "right",
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
    
    panel.grid.minor = element_blank()
  )

# Perform k-means clustering on the PCA scores
k <- 2  # number of clusters
set.seed(123)  # for reproducibility
kmeans_result <- kmeans(DLSzscore$ind$coord[,1:2], centers = k)

pca_plot <- fviz_pca_ind(DLSzscore, 
             label = "none", 
             habillage = as.factor(E9_Behavior_Data$Group), 
             palette = group_cols, 
             addEllipses = TRUE, 
             axes = c(1, 2), # only plot PC1 and PC2 on x and y axis
             choose.var = optimal_pcs,
             ggtheme = plot_theme) + # plot optimal number of PCs
  ggtitle("PCA of Individuals")
  
# Create k-means clustering plot
kmeans_plot <- fviz_cluster(list(data = DLSzscore$ind$coord[,1:2], cluster = kmeans_result$cluster),
                         label = "none", # hide individual labels
                         geom = "point",
                         ellipse.type = "convex",
                         palette = c("#00ac8c", "#1e3791"),
                         main = paste("k-means clustering (k =", k, ")"),
                         addlabels = FALSE,
                         ggtheme = plot_theme) +
  ggtitle("k-means clustering")

# Add labels for the individual IDs
pca_plot <- pca_plot + geom_text(aes(x = DLSzscore$ind$coord[,1], y = DLSzscore$ind$coord[,2], label = E9_Behavior_Data$ID), size = 2)

# Biplot of PCA
biplot <- fviz_pca_biplot(DLSzscore, repel = TRUE, ggtheme = plot_theme) +
  ggtitle("Biplot of PCA")

# Combine all plots using cowplot
combined_plot <- plot_grid(pca_plot, plot_grid(kmeans_plot, biplot, ncol = 2, labels = c("B", "C"), label_size = 12, label_fontface = "bold"), labels = c("A"), label_size = 12, label_fontface = "bold", ncol = 1, rel_heights = c(1, 1))

combined_plot

# Save the plot
ggsave("combined_plot.png", combined_plot, width = 12, height = 8, dpi = 300)























# Load the required packages
library(kernlab)
library(factoextra)

# Perform PCA on the data
DLSzscore <- PCA(E9_Behavior_Data[,c(-1:-2)], graph = FALSE)

# Compute the spectral clustering on the PCA scores
k <- 2  # number of clusters
set.seed(123)  # for reproducibility
spectral_result <- as.factor(specc(DLSzscore$ind$coord[,1:2], centers=k, algorithm="normalized.cut"))

# Create a scatterplot of the PCA scores, colored by cluster
fviz_pca_ind(DLSzscore, 
             habillage = spectral_result, 
             palette = c("#1e3791", "#00ac8c"),
             addEllipses = TRUE, 
             legend.title = "Cluster",
             ggtheme = theme_minimal()
)




# K-means clustering with ID labels
# Load required libraries
library(factoextra)  # for visualization of clustering results

# Set the number of clusters
k <- 2

# Perform PCA on the data
DLSzscore <- PCA(E9_Behavior_Data[,c(-1:-2)], graph = FALSE)

# Set the seed for reproducibility
set.seed(123)

# Perform k-means clustering on the first two principal components
kmeans_result <- kmeans(DLSzscore$ind$coord[,1:2], centers = k)

# Create a scatter plot of the PCA scores with colored clusters
pca_plot <- fviz_cluster(list(data = DLSzscore$ind$coord[,1:2], cluster = kmeans_result$cluster),
                         geom = "point", pointsize = 2.5,
                         ellipse.type = "convex", palette = c("#1e3791", "#00ac8c"),
                         ggtheme = theme_minimal(), main = paste("k-means clustering (k =", k, ")"),
                         xlab = paste("PC1 (", round(100 * PCA_var$eig[1]/sum(PCA_var$eig), 1), "%)"),
                         ylab = paste("PC2 (", round(100 * PCA_var$eig[2]/sum(PCA_var$eig), 1), "%)"))

# Add labels for the individual IDs
pca_plot + geom_text(aes(x = DLSzscore$ind$coord[,1], y = DLSzscore$ind$coord[,2], label = E9_Behavior_Data$ID),
                     size = 2, hjust = -0.1, vjust = 0.5)


# Spectral clustering
# Spectral clustering with ID labels
library(factoextra)

# Set the number of clusters
k <- 2

# Perform PCA on the data
DLSzscore <- PCA(E9_Behavior_Data[,c(-1:-2)], graph = FALSE)

# Compute the spectral clustering
sc_result <- e1071::specc(DLSzscore$ind$coord[,1:2], centers = k, degree = 2)

# Create a scatter plot of the PCA scores with colored clusters
pca_plot <- fviz_cluster(list(data = DLSzscore$ind$coord[,1:2], cluster = sc_result),
                         geom = "point", pointsize = 2.5,
                         ellipse.type = "convex", palette = c("#1e3791", "#00ac8c"),
                         ggtheme = theme_minimal(), main = paste("Spectral clustering (k =", k, ")"),
                         xlab = paste("PC1 (", round(100 * DLSzscore$var$contrib[1], 1), "%)"),
                         ylab = paste("PC2 (", round(100 * DLSzscore$var$contrib[2], 1), "%)"))

# Add labels for the individual IDs
pca_plot + geom_text(aes(x = DLSzscore$ind$coord[,1], y = DLSzscore$ind$coord[,2], label = E9_Behavior_Data$ID),
                     size = 2, hjust = 1.2, vjust = 0.5)




# DBSCAN clustering
# Load required libraries
library(factoextra)  # for visualization of clustering results
library(fpc)  # for computing the silhouette score
library(dbscan)  # for performing DBSCAN clustering
library(cluster)

# Set the parameters for DBSCAN clustering
eps <- 0.9  # maximum distance between points in a cluster
minPts <- 3  # minimum number of points required to form a dense region

# Perform PCA on the data
DLSzscore <- PCA(Behavior_Data[,c(-1:-2)], graph = FALSE)

# Perform DBSCAN clustering on the first two principal components
db_result <- dbscan(DLSzscore$ind$coord[,1:2], eps = eps, MinPts = minPts)

# Compute the silhouette score to evaluate the clustering
silhouette_score <- silhouette(DLSzscore$ind$coord[,1:2], db_result$cluster)$avg.width

# Create a scatter plot of the PCA scores with colored clusters
pca_plot <- fviz_cluster(list(data = DLSzscore$ind$coord[,1:2], cluster = db_result$cluster),
                         geom = "point", pointsize = 2.5,
                         ggtheme = theme_minimal(), main = paste("DBSCAN clustering (eps =", eps, ", minPts =", minPts, ")"),
                         xlab = paste("PC1 (", round(100 * PCA_var$eig[1]/sum(PCA_var$eig), 1), "%)"),
                         ylab = paste("PC2 (", round(100 * PCA_var$eig[2]/sum(PCA_var$eig), 1), "%)"))

# Add labels for the individual IDs
pca_plot + geom_text(aes(x = DLSzscore$ind$coord[,1], y = DLSzscore$ind$coord[,2], label = E9_Behavior_Data$ID),
                     size = 2, hjust = -0.1, vjust = 0.5)


E9_Behavior_Data <- read_excel("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/SIS_Analysis/E9_Behavior_Data.xlsx", sheet = "DLS")

# Ensemble clustering
# Load required libraries
library(mclust)
library(flexclust)
library(cluster)
library(ensembldb)
library(factoextra)

# Read in your data
E9_Behavior_Data <- read_excel("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/SIS_Analysis/E9_Behavior_Data.xlsx", sheet = "DLS")

# Exclude first two columns
my_data <- E9_Behavior_Data[, -c(1:2)]

# Normalize and scale the data
my_data_norm <- scale(my_data)

# Perform k-means clustering
k_means <- kmeans(my_data_norm, centers = 3)

# Perform hierarchical clustering
hclust_result <- hclust(dist(my_data_norm), method = "ward.D2")

# Perform model-based clustering using Mclust
mclust_model <- Mclust(my_data_norm)

# Create an ensemble of the three clustering results
ensemble <- ensembldb(k_means, hclust_result, mclust_model)

# Combine the individual clusterings into a single consensus clustering
consensus <- consensusCluster(ensemble, method = "ward", consensus = "HC1")

# Visualize the results
fviz_cluster(list(data = my_data_norm, cluster = consensus$consensus), geom = "point")

# Add cluster assignments to the original data
E9_Behavior_Data$cluster <- consensus$consensus

# Plot the clusters
plot(Behavior_Data$PC1, Behavior_Data$PC2, 
     col = E9_Behavior_Data$cluster, pch = 20, cex = 1.5, 
     xlab = "PC1", ylab = "PC2", main = "Ensemble clustering of E9_Behavior_Data")


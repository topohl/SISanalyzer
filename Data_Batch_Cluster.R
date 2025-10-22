#' Data Batch Clustering Analysis and Visualization
#'
#' This script performs data analysis and visualization for clustering experiments.
#' It installs and loads required packages, reads experimental data from an Excel file,
#' merges clustering results, and updates group classifications.
#'
#' Workflow Overview:
#'   Package Setup: Checks and installs required packages, then loads them.
#'   Data Import: Reads data from Excel and CSV files for SUS and clustering information.
#'   Data Preparation: Merges data, updates group assignments based on susceptibility,
#'                    and sets up factors.
#'   Statistical Testing: For each variable and sex, the following are performed:
#'       - Normality Check: Shapiro-Wilk tests per cluster.
#'       - Pairwise Tests: Uses t-test or Wilcoxon rank-sum test when only two clusters exist.
#'       - Multi-group Comparison: Applies ANOVA (with effect size calculation and Bonferroni-corrected
#'                                pairwise tests) or Kruskal-Wallis (with Dunn's test using Holm correction)
#'                                when more than two clusters are present.
#'   Visualization: Generates ggplot2-based plots with custom themes, and saves plots and dendrograms as SVG files.
#'
#' Functions:
#'   generate_plot:
#'       Creates a ggplot for a given variable and sex using jittered points,
#'       summary statistics, custom color scales, and a minimal theme.
#'
#'   perform_wilcoxon_test:
#'       Executes a Wilcoxon rank-sum test between two clusters given a minimum sample size;
#'       returns NULL if the sample size is insufficient.
#'
#'   perform_posthoc_anova:
#'       Performs ANOVA for a variable grouped by clusters, calculates effect size (eta squared),
#'       and conducts pairwise t-tests with Bonferroni correction.
#'
#'   perform_posthoc_kruskal:
#'       Conducts a Kruskal-Wallis test when normality assumptions are not met,
#'       followed by Dunn's pairwise tests with Holm correction.
#'
#'   test_and_plot_variable:
#'       Filters data by sex, performs normality assessments, selects the appropriate statistical test,
#'       and generates corresponding plots; returns a list containing test results, plot, and post hoc results.
#'
#'   generate_dendrogram:
#'       Generates and saves a dendrogram using hierarchical clustering on numeric data,
#'       assigns cluster-specific colors to leaf labels, and exports an SVG file.
#'
#' Output:
#'   CSV Files: Test results and post hoc analysis results are saved as CSV files in a designated results directory.
#'   SVG Files: Variable-specific plots and dendrogram visualizations are exported as SVG files for both males and females.

# Install libraries if not already installed
requiredPackages <- c("scales", "ggplot2", "dplyr", "openxlsx", "rstatix", "readxl", "svglite", "ggpubr", "cowplot", "reshape2", "ggradar", "devtools", "fmsb", "dendextend")

for (package in requiredPackages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package, dependencies = TRUE)
  } else {
    library(package, character.only = TRUE)
  }
}
  
# Define group colors
#group_cols <- c("#1e3791", "#76A2E8", "#F79719")
# group_cols <- c("#5e4c5f", "#999999", "#ffbb6f")
#group_cols <- c("#FFB6C1", "#FFE4B5", "#FF6347")  # Pastel Pink, Moccasin, Tomato
#group_cols <- c("#87CEFA", "#FFDAB9", "#20B2AA")  # Light Sky Blue, Peach, Light Sea Green
#group_cols <- c("#FFD700", "#FFE4B5", "#FF6347")  # Gold, Moccasin, Tomato
#group_cols <- c("#FFA07A", "#FFDAB9", "#9370DB")  # Light Salmon, Peach, Medium Purple
#group_cols <- c("#98FB98", "#FFE4B5", "#20B2AA")  # Pale Green, Moccasin, Light Sea Green
#group_cols <- c("#FFA07A", "#FFE4B5", "#20B2AA")  # Light Salmon, Moccasin, Light Sea Green
# group_cols <- c("#FF6347", "#FFDAB9", "#87CEEB")  # Tomato, Peach, Sky Blue
group_cols <- c("#FF7E47", "#FFBB49", "#6BCAEB")  # Orange, Yellow, Light Blue, this looks nice
#group_cols <- c("#FFD700", "#FFE4B5", "#FF6347")  # Gold, Moccasin, Tomato
#group_cols <- c("#87CEFA", "#FFDAB9", "#FF6347")  # Light Sky Blue, Peach, Tomato
#group_cols <- c("#98FB98", "#FFE4B5", "#FF6347")  # Pale Green, Moccasin, Tomato
#group_cols <- c("#FFB6C1", "#FFE4B5", "#20B2AA")  # Pastel Pink, Moccasin, Light Sea Green
#group_cols <- c("#FFD700", "#FFE4B5", "#87CEEB")  # Gold, Moccasin, Sky Blue
#group_cols <- c("#87CEFA", "#FFDAB9", "#FF4500")  # Light Sky Blue, Peach, Orange Red
#group_cols <- c("#FFA07A", "#FFDAB9", "#20B2AA")  # Light Salmon, Peach, Light Sea Green
#group_cols <- c("#98FB98", "#FFE4B5", "#FF4500")  # Pale Green, Moccasin, Orange Red
#group_cols <- c("#FF6347", "#FFDAB9", "#FFA07A")  # Tomato, Peach, Light Salmon
#group_cols <- c("#FFD700", "#FFE4B5", "#98FB98")  # Gold, Moccasin, Pale Green
#group_cols <- c("#87CEFA", "#FFDAB9", "#FF6347")  # Light Sky Blue, Peach, Tomato
#group_cols <- c("#FFB6C1", "#FFE4B5", "#FF6347")  # Pastel Pink, Moccasin, Tomato
#group_cols <- c("#98FB98", "#FFE4B5", "#FFD700")  # Pale Green, Moccasin, Gold

# Factors to be used in the ANOVA or pairwise tests
factors <- c("Cluster")

# Read data from Excel file
file_path <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/SIS_Analysis/E9_Behavior_Data.xlsx"
sheet_name <- "OverallZ"
data <- read_excel(file_path, sheet = sheet_name)

# Define the result directory
result_dir <- paste0("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/SIS_Analysis/statistics/cluster/", sheet_name, "/")

# Check if the directory exists, if not, create it
if (!dir.exists(result_dir)) {
  dir.create(result_dir, recursive = TRUE)
}

# Define SUS animals (csv file)
susAnimals <- c(readLines(paste0("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/sus_animals.csv")))

# Define cluster animals and get information of ID and cluster (csv file), selected Excel sheet name included in file output -- only activate if using clusters to analyze
clusterAnimals <- read.csv("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/", sheet_name, "clustering_results.csv", header = TRUE, stringsAsFactors = FALSE)

# add new row to data with cluster iformation of each ID
data <- merge(data, clusterAnimals[, c("ID", "Cluster")], by = "ID", all.x = TRUE)

# Update the Group column based on the ID and susAnimals
data <- data %>%
  mutate(Group = if_else(ID %in% susAnimals, "SUS",
                         if_else(Group == "SIS", "RES", Group)))

# set Cluster as discrete variable
data$Cluster <- as.factor(data$Cluster)

# Function to generate plots for each variable and sex
generate_plot <- function(data, variable_name, sex) {
  filtered_data <- data %>%
    filter(Sex == sex)

  # Create the plot using ggplot
  p <- ggplot(filtered_data, aes(Cluster, .data[[variable_name]], color = Cluster)) +
    # Customize the axes and labels
    scale_x_discrete(name = NULL, expand = c(0.3, 0.1)) + 
    scale_y_continuous(expand = c(0.1, 0.1)) +
    # Add jittered points and summary statistics
    geom_jitter(aes(fill = Cluster), size = 4, alpha = 0.7, width = 0.2, shape = 16) +
    stat_summary(
      fun.min = function(z) {quantile(z, 0.25)},
      fun.max = function(z) {quantile(z, 0.75)},
      fun = median,
      color = "black",
      size = 0.8,
      shape = 16,
      width = 1) +  # Set the width of the quantile line to 0.5
    # Customize the plot labels and colors
    labs(title = bquote(~bold(.(variable_name))),
         subtitle = paste("(", sex, ")", sep = ""),
         x = NULL,
         y = "z score [a.u.]") +
    scale_color_manual(name = NULL, values = group_cols) +
    scale_fill_manual(name = NULL, values = group_cols) +
    # Customize the plot theme
    theme_minimal_hgrid(12, rel_small = 1) +
    theme(plot.title = element_text(hjust = 0.5, face = "plain"),
          plot.subtitle = element_text(hjust = 0.5, size = 10, face = "plain"),
          legend.position = "none",
          axis.title.x = element_blank(),
          axis.text.x = element_text(),
          axis.ticks.x = element_blank())
return(p)
}

# Function to perform Wilcoxon rank-sum test
perform_wilcoxon_test <- function(data_cluster1, data_cluster2) {
  if (length(data_cluster1) >= 3 && length(data_cluster2) >= 3) {
    return(wilcox.test(data_cluster1, data_cluster2))
  } else {
    return(NULL) # Return NULL if there are not enough observations in one of the clusters
  }
}

# Function to perform post hoc pairwise tests for ANOVA
perform_posthoc_anova <- function(data, variable_name) {
  # Perform ANOVA
  anova_test <- aov(as.formula(paste(variable_name, "~ Cluster")), data = data)

  # Perform effecz size calculation
  eta_squared <- eta_squared(anova_test)

  # Perform pairwise t-test with Bonferroni correction
  pairwise_results <- pairwise_t_test(data, formula = as.formula(paste(variable_name, "~ Cluster")),
                                      p.adjust.method = "bonferroni")

  # Add a column specifying the Cluster comparison
  pairwise_results$Cluster_Comparison <- paste(pairwise_results$cluster1, "vs.", pairwise_results$cluster2)

  # Add column indicating the type of multiple comparison correction
  pairwise_results$Correction <- "Bonferroni"
  return(pairwise_results)
}

# Function to perform post hoc pairwise tests for Kruskal-Wallis
perform_posthoc_kruskal <- function(data, variable_name) {
  # Perform Kruskal-Wallis test
  kruskal_test <- kruskal.test(as.formula(paste(variable_name, "~ Cluster")), data = data)

  # Perform pairwise Dunn's test with Holm correction
  pairwise_results <- dunn_test(data, formula = as.formula(paste(variable_name, "~ Cluster")),
                                p.adjust.method = "holm")

  # Add a column specifying the Cluster comparison
  pairwise_results$Cluster_Comparison <- paste(pairwise_results$cluster1, "vs.", pairwise_results$cluster2)
  # Add column indicating the type of multiple comparison correction
  pairwise_results$Correction <- "Holm"
  return(pairwise_results)
}

# Function to perform normality test and appropriate test (ANOVA/Kruskal-Wallis or t-test/Wilcoxon rank-sum test) for each variable and sex
test_and_plot_variable <- function(data, variable_name, sex) {
  filtered_data <- data %>%
    filter(Sex == sex)

  # Perform normality test for each Cluster
  unique_clusters <- unique(filtered_data$Cluster)
  num_clusters <- length(unique_clusters)

  if (num_clusters == 2) {
    # Perform pairwise comparisons for cases with two clusters
    cluster1 <- unique_clusters[1]
    cluster2 <- unique_clusters[2]

    data_cluster1 <- filtered_data[[variable_name]][filtered_data$Cluster == cluster1]
    data_cluster2 <- filtered_data[[variable_name]][filtered_data$Cluster == cluster2]

    # Check if there are enough observations in both Clusters
    if (sum(!is.na(data_cluster1)) >= 3 && sum(!is.na(data_cluster2)) >= 3) {
      cluster1_norm <- shapiro.test(data_cluster1)
      cluster2_norm <- shapiro.test(data_cluster2)

      # Check if both Clusters are normally distributed
      if (cluster1_norm$p.value >= 0.05 && cluster2_norm$p.value >= 0.05) {
        # Perform t-test
        t_res <- t.test(data_cluster1, data_cluster2)

        # Store test results in a list
        test_results <- list(
          Variable = variable_name,
          Sex = sex,
          Test = "t-test",
          cluster1_Normality = cluster1_norm$p.value,
          cluster2_Normality = cluster2_norm$p.value,
          cluster3_Normality = NA,
          P_Value = t_res$p.value,
          Significance_Level = sprintf("%.3f", t_res$p.value)
        )

        # Generate the plot
        p <- generate_plot(filtered_data, variable_name, sex)

        return(list(test_results = test_results, plot = p, posthoc_results = NULL))
      } else {
        # Perform Wilcoxon rank-sum test
        wilcox_res <- perform_wilcoxon_test(data_cluster1, data_cluster2)

        # Check if the Wilcoxon test could be performed
        if (!is.null(wilcox_res)) {
          # Store test results in a list
          test_results <- list(
            Variable = variable_name,
            Sex = sex,
            Test = "Wilcoxon rank-sum test",
            cluster1_Normality = cluster1_norm$p.value,
            cluster2_Normality = cluster2_norm$p.value,
            Cluster3_Normality = NA,
            P_Value = wilcox_res$p.value,
            Significance_Level = sprintf("%.3f", wilcox_res$p.value)
          )

          # Generate the plot
          p <- generate_plot(filtered_data, variable_name, sex)

          return(list(test_results = test_results, plot = p, posthoc_results = NULL))
        }
      }
    }
  } else {
    # Perform normality test for each Cluster
    cluster1_norm <- shapiro.test(filtered_data[[variable_name]][filtered_data$Cluster == "1"])
    cluster2_norm <- shapiro.test(filtered_data[[variable_name]][filtered_data$Cluster == "2"])

    # Check if there is at least one sample in the SUS Cluster for the specified sex
    cluster3_exists <- any(filtered_data$Cluster == "3")
    if (cluster3_exists) {
      cluster3_norm <- shapiro.test(filtered_data[[variable_name]][filtered_data$Cluster == "3"])
    } else {
      cluster3_norm <- list(p.value = 1)  # Set p-value to 1 when SUS Cluster is missing
    }

    # Check if any of the normality tests result in a missing value (i.e., not enough observations in a Cluster)
    if (is.na(cluster1_norm$p.value) || is.na(cluster2_norm$p.value) || is.na(cluster3_norm$p.value)) {
      return(NULL) # Return NULL if there are not enough observations in one of the Clusters
    }

    # Store test results in a list
    test_results <- list(
      Variable = variable_name,
      Sex = sex,
      cluster1_Normality = cluster1_norm$p.value,
      cluster2_Normality = cluster2_norm$p.value,
      cluster3_Normality = cluster3_norm$p.value
    )

    # Create empty data frames to store test results and post hoc results
    test_results_df <- data.frame()
    posthoc_results_df <- data.frame()

    # Check if there are more than two Clusters
    if (num_clusters > 2) {
      # Perform ANOVA if all clusters are normal, otherwise use Kruskal-Wallis
      if (cluster1_norm$p.value >= 0.05 && cluster2_norm$p.value >= 0.05 && cluster3_norm$p.value >= 0.05) {
        # If all clusters are normal, use ANOVA
        anova_test <- aov(as.formula(paste(variable_name, "~ Cluster")), data = filtered_data)
        test_results$Test <- "ANOVA"
        test_results$P_Value <- summary(anova_test)[[1]][["Pr(>F)"]][1]
        test_results$Significance_Level <- sprintf("%.3f", test_results$P_Value)
        test_results$Eta_Squared <- eta_squared(anova_test)

        # Perform post hoc pairwise tests for ANOVA and store results
        posthoc_results_df <- perform_posthoc_anova(filtered_data, variable_name)
      } else {
        # If at least one Cluster is not normal, use Kruskal-Wallis test
        kruskal_test <- kruskal.test(as.formula(paste(variable_name, "~ Cluster")), data = filtered_data)
        test_results$Test <- "Kruskal-Wallis"
        test_results$P_Value <- kruskal_test$p.value
        test_results$Significance_Level <- sprintf("%.3f", p.adjust(test_results$P_Value, method = "BH"))

        # Calculate effect size (eta squared) for Kruskal-Wallis
        n_clusters <- length(unique(filtered_data$Cluster))
        n_total <- length(filtered_data$Cluster)
        h <- (kruskal_test$statistic - 1) / (n_total - n_clusters)
        effect_size <- h / (1 + (n_clusters - 1) / n_total)

        # Store effect size in test_results
        test_results$Eta_Squared <- as.numeric(sprintf("%.3f", effect_size))

        # Perform post hoc pairwise tests for Kruskal-Wallis and store results
        posthoc_results_df <- perform_posthoc_kruskal(filtered_data, variable_name)
      }

      # Merge posthoc_results_df with test_results_df if both data frames have the same columns
      if (!is.null(posthoc_results_df) && ncol(posthoc_results_df) > 0) {
        # Check if test_results_df and posthoc_results_df have the same columns
        if (identical(names(test_results_df), names(posthoc_results_df))) {
          test_results_df <- bind_rows(test_results_df, posthoc_results_df)
        } else {
          # In case the data frames have different columns, use rbind.fill to merge them
          test_results_df <- plyr::rbind.fill(test_results_df, posthoc_results_df)
        }
      }
    }

    # Generate the plot
    p <- generate_plot(filtered_data, variable_name, sex)

    return(list(test_results = test_results, plot = p, posthoc_results = test_results_df))
  }
}

# Get the list of columns to plot (excluding "ID", "Cluster", and "Sex")
columns_to_plot <- setdiff(names(data), c("ID", "Group", "Sex", "Batch", "Cluster"))

# Initialize empty lists to store test results and plots
all_test_results <- list()
all_plots <- list()
all_posthoc_results <- list()

# Iterate through each variable and sex, and perform tests
for (variable in columns_to_plot) {
  for (sex in c("m", "f")) {
    result <- test_and_plot_variable(data, variable, sex)
    if (!is.null(result)) {
      if (is.null(result$posthoc_results)) {
        # Store test results for t-test
        result$test_results$Sex <- sex  # Add sex column to test results
        all_test_results <- c(all_test_results, list(result$test_results))
      } else {
        # Store test results for Kruskal-Wallis
        result$test_results$Sex <- sex  # Add sex column to test results
        result$posthoc_results$Sex <- sex  # Add sex column to posthoc results
        all_test_results <- c(all_test_results, list(result$test_results))
        all_posthoc_results <- c(all_posthoc_results, list(result$posthoc_results))
      }
      all_plots <- c(all_plots, list(result$plot))
    }
  }
}

# Convert the list of test results to a data frame
all_test_results_df <- bind_rows(all_test_results)

# Save the test results data frame to a CSV file
write.csv(all_test_results_df, file = paste0(result_dir, "test_results_", sheet_name, ".csv"), row.names = FALSE)

# Save the post hoc results to a CSV file
if (!is.null(all_posthoc_results) && length(all_posthoc_results) > 0) {
  all_posthoc_results_df <- bind_rows(all_posthoc_results)
  write.csv(all_posthoc_results_df, file = paste0(result_dir, "posthoc_results_", sheet_name, ".csv"), row.names = FALSE)
}

# Save the plots for males and females separately
for (i in seq_along(all_plots)) {
  # Extract the variable name from the corresponding test results
  variable_name <- all_test_results[[i]]$Variable

  if (i %% 2 == 0) {
  ggsave(filename = paste0(result_dir, "female_", variable_name, ".svg"), plot = all_plots[[i]], width = 2.8, height = 3)
  } else {
  ggsave(filename = paste0(result_dir, "male_", variable_name, ".svg"), plot = all_plots[[i]], width = 2.8, height = 3)
  }
}

# Generate dendrograms for males and females and save them as SVG files with marked clusters
generate_dendrogram <- function(data, sex, result_dir, clusterAnimals) {
  filtered_data <- data %>%
    filter(Sex == sex)

  # Debug information
  #message("Number of data points for sex '", sex, "': ", nrow(filtered_data))
  #message("Columns before select: ", paste(colnames(filtered_data), collapse = ", "))

  # Select numeric columns
  numeric_columns <- dplyr::select(filtered_data, -ID, -Group, -Sex, -Cluster, -Batch)

  # Check if there are enough rows for clustering
  if (nrow(numeric_columns) < 2) {
    #message("Not enough data points for sex: ", sex)
    return(NULL)
  }

  # Debug information
  #message("Columns after select: ", paste(colnames(numeric_columns), collapse = ", "))

  # Compute the distance matrix
  distance_matrix <- dist(numeric_columns)

  # Perform hierarchical clustering
  clustering <- hclust(distance_matrix, method = "ward.D2")

  # assign IDs to the clustering
  clustering$labels <- filtered_data$ID

  # Generate the dendrogram
  dendrogram <- as.dendrogram(clustering)

  # show dendrogram labels
  #labels(dendrogram) <- filtered_data$ID

  # Extracting the IDs from clusterAnimals
  #actual_ids <- clusterAnimals$ID

  # Assigning the actual IDs to the dendrogram labels
  #labels(dendrogram) <- actual_ids

  # Get unique clusters
  unique_clusters <- unique(clusterAnimals$Cluster)

  # Assign colors to clusters
  cluster_colors <- rainbow(length(unique_clusters))

  # Function to assign colors to clusters
  assign_cluster_colors <- function(cluster) {
    cluster_color <- switch(cluster,
                            "1" = "#FF7E47",
                            "2" = "#FFBB49",
                            "3" = "#6BCAEB",
                            "#FF7E47")  # Default color if cluster value is not found
    return(cluster_color)
  }

  # Assign colors to dendrogram labels based on clusters
  dendrogram <- dendrapply(dendrogram, function(d) {
    if (is.leaf(d)) {
      # Get the ID of the leaf
      leaf_id <- attr(d, "label")
      # Get the cluster of the leaf
      leaf_cluster <- clusterAnimals$Cluster[clusterAnimals$ID == leaf_id]
      # Assign color to the leaf based on the cluster
      attr(d, "nodePar") <- c(attr(d, "nodePar"), lab.col = assign_cluster_colors(leaf_cluster))
    }
    return(d)
  })

  # Plot the dendrogram and save as SVG file
  svg_file <- paste0(result_dir, sex, "_dendrogram_with_clusters.svg")
  svg(svg_file)
  plot(dendrogram, main = paste("Dendrogram for", sex))
  dev.off()

  message("Dendrogram for ", sex, " with clusters marked saved as:", svg_file)
}

# Call the function for males and females
generate_dendrogram(data, "m", result_dir, clusterAnimals)
generate_dendrogram(data, "f", result_dir, clusterAnimals)
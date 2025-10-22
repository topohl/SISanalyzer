# This script installs and loads necessary libraries, reads data from an Excel file,
# updates the Group column based on certain conditions, generates plots for each variable and sex,
# performs statistical tests (T-test/Wilcoxon rank-sum test, post hoc pairwise tests for ANOVA/Kruskal-Wallis),
# and applies customizations to the plots.

# Install libraries if not already installed
requiredPackages <- c("scales", "ggplot2", "dplyr", "openxlsx", "rstatix", "readxl", "svglite", "ggpubr", "cowplot", "reshape2", "ggradar", "devtools", "fmsb", "rlang")

for (package in requiredPackages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package, dependencies = TRUE)
  } else {
    library(package, character.only = TRUE)
  }
}

# Define group colors
group_cols <- c("#1e3791", "#76A2E8", "#F79719")
#group_cols <- c("#919c4c", "#c03728", "#fd8f24")
#group_cols <- c("#2b323f", "#c03728", "#fd8f24")

# Factors to be used in the ANOVA or pairwise tests
factors <- c("Group")

# Read data from Excel file
file_path <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/SIS_Analysis/E9_Behavior_Data.xlsx"
sheet_name <- "DLSsingleSlim"
data <- read_excel(file_path, sheet = sheet_name)

# Define the result directory
result_dir <- paste0("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/SIS_Analysis/statistics/", sheet_name, "/")

# Check if the directory exists, if not, create it
if (!dir.exists(result_dir)) {
  dir.create(result_dir, recursive = TRUE)
}

# Define SUS animals (csv file)
susAnimals <- c(readLines(paste0("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/sus_animals_batchCorrected.csv")))

# Update the Group column based on the ID and susAnimals
data <- data %>%
  mutate(Group = if_else(ID %in% susAnimals, "SUS",
                         if_else(Group == "SIS", "RES", Group)))

# Function to calculate global min and max for a variable across both sexes
calculate_y_limits <- function(data, variable_name) {
  global_min <- min(data[[variable_name]], na.rm = TRUE)
  global_max <- max(data[[variable_name]], na.rm = TRUE)

  return(c(global_min, global_max))
}

# Function to generate plots for each variable and sex
generate_plot <- function(data, variable_name, sex, y_limits) {
  filtered_data <- data %>%
    filter(Sex == sex)

# Create the plot using ggplot
p <- ggplot(filtered_data, aes(Group, .data[[variable_name]], color = Group)) +
  # Customize the axes and labels
  scale_x_discrete(name = NULL, expand = c(0.3, 0.1)) + 
  scale_y_continuous(limits = y_limits, expand = c(0.1, 0.1)) +  # Set y-axis limits
  # Add jittered points and summary statistics
  geom_jitter(aes(fill = Group), size = 7, alpha = 0.7, width = 0.2, shape = 16) +
  stat_summary(fun.min = function(z) { quantile(z, 0.25) },
               fun.max = function(z) { quantile(z, 0.75) },
               fun = median,
               color = "black",
               size = 1,
               shape = 16,
               width = 5) +  # Set the width of the quantile line to 0.5
    # Customize the plot labels and colors
  labs(title = bquote(~bold(.(variable_name))),
     subtitle = paste("(", sex, ")", sep = ""),
       x = NULL,
       y = "z score [a.u.]") +
    scale_color_manual(name = NULL, values = group_cols) +
    scale_fill_manual(name = NULL, values = group_cols) +
    # Customize the plot theme
    theme_minimal_hgrid(12, rel_small = 1) +
  theme(panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "plain"),
    plot.subtitle = element_text(hjust = 0.5, size = 15, face = "plain"),
    legend.position = "none",
    axis.line = element_line(color = "black"),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 16),  # Increase x-axis text size
    axis.ticks.x = element_blank(),
    panel.background = element_blank(),
    axis.text.y = element_text(size = 16),  # Increase y-axis text size
    axis.title.y = element_text(size = 16)
  )
  return(p)
}

# Function to create radar chart
create_radar_chart <- function(data, sex, result_dir) {
  # Coerce tibble to dataframe
  data <- as.data.frame(data)

  # Filter data for the specified sex
  sex_data <- data[data$Sex == sex, ]
  # print sex_data head
  head(sex_data)

  # Extract variables to plot (excluding ID, Group, and Sex)
  variables <- setdiff(names(sex_data), c("ID", "Group", "Sex", "Batch"))

  # Calculate mean values for each group, including NA values
  group_means <- aggregate(sex_data[, variables], by = list(Group = sex_data$Group), FUN = function(x) mean(x, na.rm = TRUE))
  rownames(group_means) <- group_means$Group
  group_means <- group_means[, -1]  # Remove the "Group" column

  # calculate meadian values for each group, including NA values
  group_medians <- aggregate(sex_data[, variables], by = list(Group = sex_data$Group), FUN = function(x) median(x, na.rm = TRUE))
  rownames(group_medians) <- group_medians$Group
  group_medians <- group_medians[, -1]  # Remove the "Group" column

  # Identify Max and Min for each variable and create dataframe
  max_min <- data.frame(matrix(nrow = 2, ncol = length(variables)))
  colnames(max_min) <- variables
  rownames(max_min) <- c("Max", "Min")

  for (i in 1:length(variables)) {
    max_min[1, i] <- max(sex_data[, variables[i]], na.rm = TRUE)
    max_min[2, i] <- min(sex_data[, variables[i]], na.rm = TRUE)
  }

  # bind max and min to dataframe
  group_medians <- rbind(max_min, group_medians)

  # Create radar chart
  radarchart(group_medians, 
             pcol = group_cols, pfcol = scales::alpha(group_cols, 0.5),
             plwd = 2, plty = 1, cglcol = "grey", cglty = 1, cglwd = 0.8,
             axistype = 1, title = paste("Radar Chart for", sex))

  # Save radar chart
  filename <- paste0(result_dir, sex, "_radar_chart.svg")
  print(filename)  # Debugging: Print filename to check if it's correct
  dev.copy(svg, filename)
  dev.off()
}

# Create radar chart for males and females and save as SVG
create_radar_chart(data, c("m", "f"), result_dir)
create_radar_chart(data, "m", result_dir)  # For males
create_radar_chart(data, "f", result_dir)  # For females

# Function to perform Wilcoxon rank-sum test
perform_wilcoxon_test <- function(data_group1, data_group2) {
  if (length(data_group1) >= 3 && length(data_group2) >= 3) {
    return(wilcox.test(data_group1, data_group2))
  } else {
    return(NULL) # Return NULL if there are not enough observations in one of the groups
  }
}

# Function to perform post hoc pairwise tests for ANOVA
perform_posthoc_anova <- function(data, variable_name) {
  # Perform ANOVA
  anova_test <- aov(as.formula(paste(variable_name, "~ Group")), data = data)

  # Perform effecz size calculation
  eta_squared <- eta_squared(anova_test)

  # Perform pairwise t-test with Bonferroni correction
  pairwise_results <- pairwise_t_test(data, formula = as.formula(paste(variable_name, "~ Group")),
                                      p.adjust.method = "bonferroni")

  # Add a column specifying the group comparison
  pairwise_results$Group_Comparison <- paste(pairwise_results$group1, "vs.", pairwise_results$group2)

  # Add column indicating the type of multiple comparison correction
  pairwise_results$Correction <- "bonferroni"
  return(pairwise_results)
}

# Function to perform post hoc pairwise tests for ANOVA using Games-Howell correction
#perform_posthoc_anova <- function(data, variable_name) {
#  # Perform ANOVA
#  anova_test <- aov(as.formula(paste(variable_name, "~ Group")), data = data)
#
#  # Perform effect size calculation
#  eta_squared <- eta_squared(anova_test)
#  
#  # Perform pairwise comparisons using Games-Howell correction
#  pairwise_results <- games_howell_test(data, formula = as.formula(paste(variable_name, "~ Group")))
#
#  # Add a column specifying the group comparison
#  pairwise_results$Group_Comparison <- paste(pairwise_results$group1, "vs.", pairwise_results$group2)
#
#  # Add column indicating the type of multiple comparison correction
#  pairwise_results$Correction <- "Games-Howell"
#  return(pairwise_results)
#}

# Function to perform post hoc pairwise tests for Kruskal-Wallis
perform_posthoc_kruskal <- function(data, variable_name) {
  # Perform Kruskal-Wallis test
  kruskal_test <- kruskal.test(as.formula(paste(variable_name, "~ Group")), data = data)

  # Perform pairwise Dunn's test with Holm correction
  pairwise_results <- dunn_test(data, formula = as.formula(paste(variable_name, "~ Group")),
                                p.adjust.method = "holm")

  # Add a column specifying the group comparison
  pairwise_results$Group_Comparison <- paste(pairwise_results$group1, "vs.", pairwise_results$group2)
  # Add column indicating the type of multiple comparison correction
  pairwise_results$Correction <- "Holm"
  return(pairwise_results)
}

# Function to perform normality test and appropriate test (ANOVA/Kruskal-Wallis or t-test/Wilcoxon rank-sum test) for each variable and sex
test_and_plot_variable <- function(data, variable_name, sex) {
  # Calculate global y limits across both sexes
  y_limits <- calculate_y_limits(data, variable_name)

  filtered_data <- data %>%
    filter(Sex == sex)

  # Perform normality test for each group
  unique_groups <- unique(filtered_data$Group)
  num_groups <- length(unique_groups)

  if (num_groups == 2) {
    # Perform pairwise comparisons for cases with two groups
    group1 <- unique_groups[1]
    group2 <- unique_groups[2]

    data_group1 <- filtered_data[[variable_name]][filtered_data$Group == group1]
    data_group2 <- filtered_data[[variable_name]][filtered_data$Group == group2]

    # Check if there are enough observations in both groups
    if (sum(!is.na(data_group1)) >= 3 && sum(!is.na(data_group2)) >= 3) {
      con_norm <- shapiro.test(data_group1)
      res_norm <- shapiro.test(data_group2)

      # Check if both groups are normally distributed
      if (con_norm$p.value >= 0.05 && res_norm$p.value >= 0.05) {
        # Perform t-test
        t_res <- t.test(data_group1, data_group2)

        # Store test results in a list
        test_results <- list(
          Variable = variable_name,
          Sex = sex,
          Test = "t-test",
          CON_Normality = con_norm$p.value,
          RES_Normality = res_norm$p.value,
          SUS_Normality = NA,
          P_Value = t_res$p.value,
          Significance_Level = sprintf("%.3f", t_res$p.value)
        )

        # Generate the plot with the y_limits
        p <- generate_plot(filtered_data, variable_name, sex, y_limits)

        return(list(test_results = test_results, plot = p, posthoc_results = NULL))
      } else {
        # Perform Wilcoxon rank-sum test
        wilcox_res <- perform_wilcoxon_test(data_group1, data_group2)

        # Check if the Wilcoxon test could be performed
        if (!is.null(wilcox_res)) {
          # Store test results in a list
          test_results <- list(
            Variable = variable_name,
            Sex = sex,
            Test = "Wilcoxon rank-sum test",
            CON_Normality = con_norm$p.value,
            RES_Normality = res_norm$p.value,
            SUS_Normality = NA,
            P_Value = wilcox_res$p.value,
            Significance_Level = sprintf("%.3f", wilcox_res$p.value)
          )

          # Generate the plot with the y_limits
          p <- generate_plot(filtered_data, variable_name, sex, y_limits)

          return(list(test_results = test_results, plot = p, posthoc_results = NULL))
        }
      }
    }
  } else {
    # Perform normality test for each group
    con_norm <- shapiro.test(filtered_data[[variable_name]][filtered_data$Group == "CON"])
    res_norm <- shapiro.test(filtered_data[[variable_name]][filtered_data$Group == "RES"])

    # Check if there is at least one sample in the SUS group for the specified sex
    sus_group_exists <- any(filtered_data$Group == "SUS")
    if (sus_group_exists) {
      sus_norm <- shapiro.test(filtered_data[[variable_name]][filtered_data$Group == "SUS"])
    } else {
      sus_norm <- list(p.value = 1)  # Set p-value to 1 when SUS group is missing
    }

    # Check if any of the normality tests result in a missing value (i.e., not enough observations in a group)
    if (is.na(con_norm$p.value) || is.na(res_norm$p.value) || is.na(sus_norm$p.value)) {
      return(NULL) # Return NULL if there are not enough observations in one of the groups
    }

    # Store test results in a list
    test_results <- list(
      Variable = variable_name,
      Sex = sex,
      CON_Normality = con_norm$p.value,
      RES_Normality = res_norm$p.value,
      SUS_Normality = sus_norm$p.value
    )

    # Create empty data frames to store test results and post hoc results
    test_results_df <- data.frame()
    posthoc_results_df <- data.frame()

    # Check if there are more than two groups
    if (num_groups > 2) {
      # Perform ANOVA if all groups are normal, otherwise use Kruskal-Wallis
      if (con_norm$p.value >= 0.05 && res_norm$p.value >= 0.05 && sus_norm$p.value >= 0.05) {
        # If all groups are normal, use ANOVA
        anova_test <- aov(as.formula(paste(variable_name, "~ Group")), data = filtered_data)
        test_results$Test <- "ANOVA"
        test_results$P_Value <- summary(anova_test)[[1]][["Pr(>F)"]][1]
        test_results$Significance_Level <- sprintf("%.3f", test_results$P_Value)
        test_results$Eta_Squared <- eta_squared(anova_test)

        # Perform post hoc pairwise tests for ANOVA and store results
        posthoc_results_df <- perform_posthoc_anova(filtered_data, variable_name)
      } else {
        # If at least one group is not normal, use Kruskal-Wallis test
        kruskal_test <- kruskal.test(as.formula(paste(variable_name, "~ Group")), data = filtered_data)
        test_results$Test <- "Kruskal-Wallis"
        test_results$P_Value <- kruskal_test$p.value
        test_results$Significance_Level <- sprintf("%.3f", p.adjust(test_results$P_Value, method = "BH"))

        # Calculate effect size (eta squared) for Kruskal-Wallis
        n_groups <- length(unique(filtered_data$Group))
        n_total <- length(filtered_data$Group)
        h <- (kruskal_test$statistic - 1) / (n_total - n_groups)
        effect_size <- h / (1 + (n_groups - 1) / n_total)

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

    # Generate the plot with the y_limits
    p <- generate_plot(filtered_data, variable_name, sex, y_limits)

    return(list(test_results = test_results, plot = p, posthoc_results = test_results_df))
  }
}

# Get the list of columns to plot (excluding "ID", "Group", and "Sex")
columns_to_plot <- setdiff(names(data), c("ID", "Group", "Sex", "Batch"))

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
write.xlsx(all_test_results_df, file = paste0(result_dir, "test_results_", sheet_name, ".xlsx"), row.names = FALSE)

# Save the post hoc results to a CSV file
if (!is.null(all_posthoc_results) && length(all_posthoc_results) > 0) {
  all_posthoc_results_df <- bind_rows(all_posthoc_results)
  write.xlsx(all_posthoc_results_df, file = paste0(result_dir, "posthoc_results_", sheet_name, ".xlsx"), row.names = FALSE)
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

warnings()
## perform two way anova on data
## data is in the form of a matrix with rows as observations and columns as variables
## Grouping factors are Group and Sex, named in the columns
## The function returns a list with the following elements:

# Install required libraries
requiredPackages <- c("readxl", "dplyr", "multcomp", "broom", "tidyr", "ggplot2", "ggpubr", "ggthemes", "ggExtra", "ggforce", "ggalluvial", "lattice", "latticeExtra", "cowplot", "ggrepel", "ggsci", "ggplot2", "cowplot")

for (package in requiredPackages) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package, dependencies = TRUE)
  } else {
        library(package, character.only = TRUE)
  }
}

# Read data from Excel file
file_path <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/SIS_Analysis/E9_Behavior_Data.xlsx"
sheet_name <- "zscore3"
data <- read_excel(file_path, sheet = sheet_name)

# Define output directories and file names
result_dir <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/SIS_Analysis/statistics/zscore3/twoWayAnova/"
anova_result_file <- "2wANOVA_results.csv"
posthoc_result_file <- "posthoc_results.csv"

# Check if the result directory exists, if not, create it
if (!file.exists(result_dir)) {
  dir.create(result_dir, recursive = TRUE)
}

# Define SUS animals (csv file)
susAnimals <- c(readLines(paste0("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/sus_animals.csv")))

# Define the colors for the groups
group_cols <- c("#1e3791", "#76A2E8", "#F79719")

# create scatter plot of data
generate_2wANOVA_plot <- function(data, col, group_cols) {
    p <- ggplot(data, aes(x = Group, .data[[col]], color = Group, shape = Sex)) +
    # Customize the axes and labels
        scale_x_discrete(name = NULL, expand = c(0.3, 0.1)) + 
        scale_y_continuous(expand = c(0.1, 0.1)) +
        geom_jitter(aes(fill = Sex), size = 4, alpha = 0.7, position = position_dodge(width = 0.8)) +
        stat_summary(
            fun.min = function(z) {quantile(z, 0.25)},
            fun.max = function(z) {quantile(z, 0.75)},
            fun = median,
            aes(group = Sex),
            color = "black",
            size = 0.8,
            position = position_dodge(width = 0.8),
            legend = FALSE) +
        scale_color_manual(name = "Group", values = group_cols) +
        scale_shape_manual(name = "Sex", values = c(16, 1)) +
        labs(title = bquote(~bold(.(col))),
                 caption = paste0("Main effect of group = ", format(round(f.group, 3), nsmall = 3), ", p = ", ifelse(round(p.group, 3) == 0.01, ".01", format(round(p.group, 3), nsmall = 3)), "\nMain effect of sex = ", format(round(f.sex, 3), nsmall = 3), ", p = ", ifelse(round(p.sex, 3) == 0.01, ".01", format(round(p.sex, 3), nsmall = 3)), "\nInteraction = ", format(round(f.interaction, 3), nsmall = 3), ", p = ", ifelse(round(p.interaction, 3) == 0.01, ".01", format(round(p.interaction, 3), nsmall = 3))),
                 x = NULL,
                 y = "z score [a.u.]") +
        # Customize the plot theme
        theme_minimal_hgrid(12, rel_small = 1) +
        theme(plot.title = element_text(hjust = 0.5, face = "plain"),
            plot.subtitle = element_text(hjust = 0.5, size = 10, face = "plain"),
            legend.position = "none",
            legend.title = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(),
            axis.ticks.x = element_blank()
        )
    return(p)
}

# Update the Group column based on the ID and susAnimals
data <- data %>%
  mutate(Group = if_else(ID %in% susAnimals, "SUS",
                         if_else(Group == "SIS", "RES", Group)))

# Get the names of the columns to analyse
cols <- setdiff(names(data), c("ID", "Group", "Sex", "Batch"))

# Initialize an empty data frame to store the results
anova.result <- data.frame(
  variable = character(),
  f.group = numeric(),
  f.sex = numeric(),
  f.interaction = numeric(),
  p.group = numeric(),
  p.sex = numeric(),
  p.interaction = numeric(),
  df.group = numeric(),
  df.sex = numeric(),
  df.interaction = numeric(),
  sum_sq.group = numeric(),
  sum_sq.sex = numeric(),
  sum_sq.interaction = numeric(),
  mean_sq.group = numeric(),
  mean_sq.sex = numeric(),
  mean_sq.interaction = numeric(),
  stringsAsFactors = FALSE
)

# Create an empty data frame to store the post hoc results
posthoc.result <- data.frame(
  variable = character(),
  Comparison = character(),
  p.value = numeric(),
  stringsAsFactors = FALSE
)

# Loop over the columns to analyze
for (col in cols) {
  # Perform two-way ANOVA for each column
  aov.result <- aov(data[[col]] ~ Group * Sex, data = data)
  
  # Extract the ANOVA summary
  anova.summary <- summary(aov.result)

  # Initialize variables to store ANOVA results and effect size measures
  f.group <- f.sex <- f.interaction <- p.group <- p.sex <- p.interaction <- 0
  df.group <- df.sex <- df.interaction <- sum_sq.group <- sum_sq.sex <- sum_sq.interaction <- 0
  mean_sq.group <- mean_sq.sex <- mean_sq.interaction <- eta_sq.group <- eta_sq.sex <- eta_sq.interaction <- 0

  # Check if all expected components exist in the ANOVA summary
  if ("F value" %in% names(anova.summary[[1]]) && "Pr(>F)" %in% names(anova.summary[[1]])) {
    # Extract the F values, p values, and additional information
    f.group <- anova.summary[[1]]$"F value"[1]
    f.sex <- anova.summary[[1]]$"F value"[2]
    f.interaction <- anova.summary[[1]]$"F value"[3]
    p.group <- anova.summary[[1]]$"Pr(>F)"[1]
    p.sex <- anova.summary[[1]]$"Pr(>F)"[2]
    p.interaction <- anova.summary[[1]]$"Pr(>F)"[3]
    # Additional information: Degrees of freedom, Sum of squares, Mean squares
    df.group <- anova.summary[[1]]$Df[1]
    df.sex <- anova.summary[[1]]$Df[2]
    df.interaction <- anova.summary[[1]]$Df[3]
    sum_sq.group <- anova.summary[[1]]$"Sum Sq"[1]
    sum_sq.sex <- anova.summary[[1]]$"Sum Sq"[2]
    sum_sq.interaction <- anova.summary[[1]]$"Sum Sq"[3]
    mean_sq.group <- anova.summary[[1]]$"Mean Sq"[1]
    mean_sq.sex <- anova.summary[[1]]$"Mean Sq"[2]
    mean_sq.interaction <- anova.summary[[1]]$"Mean Sq"[3]
    # Calculate eta-squared and partial eta-squared
    total_ss <- sum_sq.group + sum_sq.sex + sum_sq.interaction
    eta_sq.group <- sum_sq.group / total_ss
    eta_sq.sex <- sum_sq.sex / total_ss
    eta_sq.interaction <- sum_sq.interaction / total_ss
  }

  # Create a row with the results for the current column
  row <- data.frame(
    variable = col,
    f.group = f.group,
    f.sex = f.sex,
    f.interaction = f.interaction,
    p.group = p.group,
    p.sex = p.sex,
    p.interaction = p.interaction,
    df.group = df.group,
    df.sex = df.sex,
    df.interaction = df.interaction,
    sum_sq.group = sum_sq.group,
    sum_sq.sex = sum_sq.sex,
    sum_sq.interaction = sum_sq.interaction,
    mean_sq.group = mean_sq.group,
    mean_sq.sex = mean_sq.sex,
    mean_sq.interaction = mean_sq.interaction,
    eta_sq.group = eta_sq.group,
    eta_sq.sex = eta_sq.sex,
    eta_sq.interaction = eta_sq.interaction,
    stringsAsFactors = FALSE
  )
  
  # Append the row to the data frame
  anova.result <- rbind(anova.result, row)
  
  # Generate plot
  p <- generate_2wANOVA_plot(data, col, group_cols)
  
  # Save plot as PNG
  ggsave(filename = paste0(result_dir, "plot_", col, ".svg"), plot = p, width = 3, height = 3.5)
}

# Print the first few rows of the data frame
head(anova.result)

# Perform Tukey's HSD test for post hoc analysis
# Initialize an empty data frame to store the post hoc results
posthoc.result <- data.frame(
  variable = character(),
  compared_columns = character(),
  Comparison = character(),
  p.value = numeric(),
  stringsAsFactors = FALSE
)

# Initialize an empty list to store the post hoc results
posthoc_results <- list()

# Loop over the columns to analyze
for (col in cols) {
    # Perform two-way ANOVA for each column
    aov.result <- aov(data[[col]] ~ Group * Sex, data = data)

    # Perform Tukey's HSD test for post hoc analysis
    posthoc <- TukeyHSD(aov.result)
        
    # Loop over the variables
    for (variable in names(posthoc)) {
        # Extract post hoc results for the variable
        posthoc_df <- as.data.frame(posthoc[[variable]])

        # Add variable name and compared column info to the dataframe
        posthoc_df$variable <- variable
        posthoc_df$compared_columns <- col
        posthoc_df$Comparison <- rownames(posthoc_df)  # Add the "Comparison" column

        # Store the results in the list
        posthoc_results[[length(posthoc_results) + 1]] <- posthoc_df
    }
}

# Combine all post hoc results into a single dataframe
posthoc.result <- do.call(rbind, posthoc_results)

# Reorder the columns
posthoc.result <- posthoc.result %>%
    select(compared_columns, variable, Comparison, diff, lwr, upr, `p adj`)

# add p adj.sign column
posthoc.result$p.adj.sign <- ifelse(posthoc.result$`p adj` < 0.0001, "****",
                      ifelse(posthoc.result$`p adj` < 0.001, "***",
                          ifelse(posthoc.result$`p adj` < 0.01, "**", 
                            ifelse(posthoc.result$`p adj` < 0.05, "*", "ns"))))

# Print the first few rows of the post hoc results dataframe
head(posthoc.result)

# Write the results to CSV files
write.csv(anova.result, file = file.path(result_dir, anova_result_file), row.names = FALSE)
write.csv(posthoc.result, file = file.path(result_dir, posthoc_result_file), row.names = FALSE)

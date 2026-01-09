###############################################################################
# PCA + Clustering Pipeline
# Author: Tobias Pohl
# Description: PCA followed by clustering with sex-specific analyses
###############################################################################

###############################################################################
# 1. DEPENDENCIES
###############################################################################

required_packages <- c(
  "pacman", "ggplot2", "cowplot", "colorspace", "dplyr", "factoextra",
  "FactoMineR", "readxl", "tidyr", "GGally", "missMDA", "MASS",
  "broom", "missForest", "corrplot", "NbClust"
)

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(char = required_packages, install = TRUE)

###############################################################################
# 2. GLOBAL SETTINGS
###############################################################################

sheet_name <- "overallRawReduced"

base_dir <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis"
data_file <- file.path(base_dir, "SIS_Analysis/E9_Behavior_Data.xlsx")

results_dir <- file.path(
  base_dir,
  "SIS_Analysis/PCA/pcaFollowedByCluster",
  sheet_name
)

dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

###############################################################################
# 3. CONSTANTS (PALETTES, SHAPES, THEMES)
###############################################################################

GROUP_COLS_ALL <- c(
  "#d7c49eff", "#d7c49eff", "#d7c49eff",
  "#343148ff", "#343148ff", "#343148ff"
)

SHAPE_MAPPING <- c(
  "CON.f" = 16, "CON.m" = 16,
  "SUS.f" = 16, "SUS.m" = 16,
  "RES.f" = 16, "RES.m" = 16
)

BASE_THEME <- theme_classic(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black", size = 20),
    legend.position = "top",
    panel.grid = element_blank()
  )

###############################################################################
# 4. HELPER FUNCTIONS
###############################################################################

get_optimal_pcs <- function(pca_obj, threshold = 0.95) {
  cum_var <- cumsum(pca_obj$eig / sum(pca_obj$eig))
  min(which(cum_var >= threshold))
}

prepare_numeric_data <- function(df, exclude_cols = 1:4) {
  df[, -exclude_cols] <- lapply(
    df[, -exclude_cols],
    function(x) as.numeric(as.character(x))
  )
  df
}

run_pca <- function(df, exclude_cols) {
  PCA(df[, -exclude_cols], scale.unit = TRUE, graph = FALSE)
}

save_svg <- function(plot, filename, w = 6, h = 5) {
  ggsave(filename, plot, width = w, height = h, dpi = 300)
}

plot_pca_individuals <- function(pca, meta, title, palette) {
  fviz_pca_ind(
    pca,
    label = "none",
    habillage = interaction(meta$Group, meta$Sex),
    palette = palette,
    addEllipses = TRUE,
    ellipse.level = 0.95,
    pointsize = 2,
    stroke = 1,
    ggtheme = BASE_THEME
  ) +
    ggtitle(title) +
    scale_shape_manual(values = SHAPE_MAPPING) +
    coord_equal()
}

plot_var_contrib <- function(pca, pc, title) {
    fviz_contrib(pca, choice = "var", axes = pc, top = 10,
                             fill = "#343148ff", color = "#343148ff") +
        theme_minimal(base_family = "Helvetica") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
        labs(title = title, x = "Variable", y = "Contribution (%)")
}

###############################################################################
# 5. LOAD & PREPARE DATA
###############################################################################

data <- read_excel(data_file, sheet = sheet_name)

susAnimals <- readLines(file.path(base_dir, "sus_animals.csv"))
conAnimals <- readLines(file.path(base_dir, "con_animals.csv"))

data$Group <- dplyr::case_when(
  data$Group == "SIS" & data$ID %in% susAnimals ~ "SUS",
  data$Group == "SIS"                          ~ "RES",
  TRUE                                         ~ data$Group
)

data <- prepare_numeric_data(data)

###############################################################################
# 6. PCA – ALL DATA
###############################################################################

pca_all <- run_pca(data, exclude_cols = 1:4)
opt_pcs <- get_optimal_pcs(pca_all)

pca_plot_all <- plot_pca_individuals(
  pca_all,
  data,
  paste("PCA of", sheet_name),
  GROUP_COLS_ALL
)

save_svg(
  pca_plot_all,
  file.path(results_dir, paste0("pca_plot_", sheet_name, ".svg"))
)

###############################################################################
# 7. VARIABLE CONTRIBUTIONS (ALL DATA)
###############################################################################

for (pc in 1:5) {
  p <- plot_var_contrib(pca_all, pc, paste("Variable Contribution – PC", pc))
  save_svg(
    p,
    file.path(results_dir, paste0("var_contrib_", sheet_name, "_PC", pc, ".svg"))
  )
}

###############################################################################
# 8. SEX-SPECIFIC PCA
###############################################################################

run_sex_specific_pca <- function(sex) {
  df <- data %>% filter(Sex == sex)
  pca <- run_pca(df, exclude_cols = c(1:4, which(names(df) == "CombZ")))

  plot <- plot_pca_individuals(
    pca,
    df,
    paste("PCA – Sex:", sex),
    if (sex == "m") GROUP_COLS_ALL[1:3] else GROUP_COLS_ALL[4:6]
  )

  save_svg(
    plot,
    file.path(results_dir, paste0("pca_plot_", sex, "_", sheet_name, ".svg"))
  )

  for (pc in 1:5) {
    vc <- plot_var_contrib(
      pca,
      pc,
      paste("Variable Contribution – PC", pc, "Sex", sex)
    )
    save_svg(
      vc,
      file.path(results_dir, paste0("var_contrib_", sheet_name, "_", sex, "_PC", pc, ".svg"))
    )
  }
}

run_sex_specific_pca("m")
run_sex_specific_pca("f")

###############################################################################
# 9. CLUSTERING ON PCA SCORES
###############################################################################

set.seed(123)
k <- 3
scores <- pca_all$ind$coord[, 1:2]
kmeans_res <- kmeans(scores, centers = k)

cluster_plot <- fviz_cluster(
  list(data = scores, cluster = kmeans_res$cluster),
  geom = "point",
  ellipse.type = "convex",
  palette = c("#00ac8c", "#1e3791", "#3e6481"),
  main = paste("k-means clustering (k =", k, ")")
) +
  theme_minimal() +
  theme(panel.grid = element_blank())

save_svg(
  cluster_plot,
  file.path(results_dir, "cluster_plot.svg")
)

###############################################################################
# 10. BIPLOT
###############################################################################

biplot <- fviz_pca_biplot(
    pca_all,
    label = "var",
    select.var = list(contrib = 10),
    repel = TRUE,
    geom.ind = "point",
    habillage = interaction(data$Group, data$Sex),
    palette = GROUP_COLS_ALL,
    addEllipses = FALSE,
    ellipse.level = 0.95,
    pointsize = 4,
    ggtheme = BASE_THEME
) +
    ggtitle("PCA Biplot – All Data (Top Contributors)") +
    scale_shape_manual(values = SHAPE_MAPPING) +
    coord_equal() +
    theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()
    )

save_svg(
  biplot,
  file.path(results_dir, "pca_biplot.svg")
)

###############################################################################
# END
###############################################################################

# Load libraries
library(ggplot2)
library(cowplot)
library(colorspace)
library(dplyr)
library(openxlsx)
library(tidyr)
library(broom)
library(readxl)
library(ggsignif)
library(ggpubr)
library(Hmisc)
library(rstatix)

# Read data
zscore1_data <- read_excel("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/SIS_Analysis/E9_Behavior_Data.xlsx", sheet = "zscore1")

# Select specific animals by ID and change their Group to "SUS"
sus_animals <- c("755","764","771","111","750","112","0004") # replace with your specific animal IDs
# sus_animals <- c("755","764","771","111","750","753","0004", "770","138","0003","112") # replace with your specific animal IDs
# sus_animals <- c("755","764","771","111","750","753","0004") # replace with your specific animal IDs
# sus_animals <- c("755","764","771","111","750","753","0004", "0001", "770", "120", "134", "26") # replace with your specific animal IDs
zscore1_data <- zscore1_data %>%
  mutate(Group = if_else(ID %in% sus_animals, "SUS",
                         if_else(Group == "SIS", "RES", Group)))

# Define group colors
group_cols <- c("#1e3791", "#00ac8c", "#F79719")

# Compute summary statistics by group
group_stats <- zscore1_data %>% 
  group_by(Group) %>% 
  dplyr::summarize(
    median = median(CombinedZ),
    lower = median(CombinedZ) - sd(CombinedZ),
    upper = median(CombinedZ) + sd(CombinedZ)
  )

# Calculate the means and standard deviations of ActivityIndex for each group
means <- zscore1_data %>% 
  group_by(Group) %>% 
  summarise(mean = mean(CombinedZ),
            sd_CombinedZ = sd(CombinedZ))

# Normality test for CTRL group
con_norm <- shapiro.test(zscore1_data$CombinedZ[means$Group == "CON"])
con_norm

# Normality test for RES group
res_norm <- shapiro.test(zscore1_data$CombinedZ[means$Group == "RES"])
res_norm

# Normality test for SUS group
sus_norm <- shapiro.test(zscore1_data$CombinedZ[means$Group == "SUS"])
sus_norm

# Perform ANOVA if all groups are normal, otherwise use Kruskal-Wallis
if (con_norm$p.value >= 0.05 & res_norm$p.value >= 0.05 & sus_norm$p.value >= 0.05) {
  # If all groups are normal, use ANOVA
  anova_test <- aov(CombinedZ ~ Group, data = zscore1_data)
  test_name <- "ANOVA"
  test_pval <- summary(anova_test)[[1]][["Pr(>F)"]][1]
  # Determine the significance level for ANOVA
  sig_levels_aov <- sprintf("%.3f", test_pval)
  pairwise_aov <- pairwise.t.test(zscore1_data$CombinedZ, zscore1_data$Group,
                  p.adjust.method = "bonferroni"
  )
} else {
  # If at least one group is not normal, use Kruskal-Wallis test
  kruskal_test <- kruskal.test(CombinedZ ~ Group, data = zscore1_data)
  test_name <- "Kruskal-Wallis"
  test_pval <- kruskal_test$p.value
  # Determine the significance level for Kruskal-Wallis test
  sig_levels_kw <- sprintf("%.3f", p.adjust(test_pval, method = "BH"))
  pairwise_kw <- dunn_test(zscore2_data, CombinedZ~Group,
                 p.adjust.method = "holm")
}

# Format the posthoc comparisons for geom_signif
formatted_comp <- posthoc_comp %>%
  as.data.frame() %>%
  mutate(.y = factor(.y, levels = unique(.y)))  # Ensure correct order of factor levels


# Plot data and t-test p-value
zscore1_data %>%
  ggplot(aes(Group, CombinedZ, color = Group)) +
  scale_x_discrete(name = NULL, expand = c(0.3, 0.1)) + 
  scale_y_continuous(expand = c(0.1, 0.1)) +
  geom_jitter(aes(fill = Group), size = 4, alpha = 0.7, width = 0.2, shape = 16) +
  stat_summary(
    fun.min=function(z) {quantile(z,0.25)},
    fun.max=function(z) {quantile(z,0.75)},
    fun=median,
    color="black",
    size=0.8,
    shape=16
  ) +
  labs(title = bquote(~bold("zscore1")),
       subtitle = "(Depressive Score)",
       caption = "",
       x = NULL,
       y = "z score [a.u.]") +
  scale_color_manual(name = NULL, values = group_cols) +
  scale_fill_manual(name = NULL, values = group_cols) +
  theme_minimal_hgrid(12, rel_small = 1) +
  theme(plot.title = element_text(hjust = 0.5, face = "plain"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "plain"),
        legend.position = "none",  # Remove the legend
        axis.title.x = element_blank(),
        axis.text.x = element_text(),  # Rotate and align x-axis labels
        axis.ticks.x = element_blank()
  )



## Using animals that are 1 SD below average of controls

# Read data
zscore1_data <- read_excel("S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/SIS_Analysis/E9_Behavior_Data.xlsx", sheet = "zscore1")

# Select specific animals by ID and change their Group to "SUS"
# sus_animals <- c("755","764","771","111","750","753","0004") # replace with your specific animal IDs
sus_animals <- c("0001", "0004", "750", "751", "755", "762", "764", "770", "771", "106", "111", "112", "113", "120", "134") # replace with your specific animal IDs
zscore1_data <- zscore1_data %>%
  mutate(Group = if_else(ID %in% sus_animals, "SUS",
                         if_else(Group == "SIS", "RES", Group)))

# Define group colors
group_cols <- c("#1e3791", "#00ac8c", "#F79719")

# Compute summary statistics by group
group_stats <- zscore1_data %>% 
  group_by(Group) %>% 
  dplyr::summarize(
    median = median(CombinedZ),
    lower = median(CombinedZ) - sd(CombinedZ),
    upper = median(CombinedZ) + sd(CombinedZ)
  )

# Calculate the means and standard deviations of ActivityIndex for each group
means <- zscore1_data %>% 
  group_by(Group) %>% 
  summarise(mean = mean(CombinedZ),
            sd_CombinedZ = sd(CombinedZ))

# Normality test for CTRL group
con_norm <- shapiro.test(zscore1_data$CombinedZ[means$Group == "CON"])
con_norm

# Normality test for RES group
res_norm <- shapiro.test(zscore1_data$CombinedZ[means$Group == "RES"])
res_norm

# Normality test for SUS group
sus_norm <- shapiro.test(zscore1_data$CombinedZ[means$Group == "SUS"])
sus_norm

# Perform ANOVA if all groups are normal, otherwise use Kruskal-Wallis
if (con_norm$p.value >= 0.05 & res_norm$p.value >= 0.05 & sus_norm$p.value >= 0.05) {
  # If all groups are normal, use ANOVA
  anova_test <- aov(CombinedZ ~ Group, data = zscore1_data)
  test_name <- "ANOVA"
  test_pval <- summary(anova_test)[[1]][["Pr(>F)"]][1]
  # Determine the significance level for ANOVA
  sig_levels_aov <- sprintf("%.3f", test_pval)
  pairwise_aov <- pairwise.t.test(zscore1_data$CombinedZ, zscore1_data$Group,
                                  p.adjust.method = "bonferroni"
  )
} else {
  # If at least one group is not normal, use Kruskal-Wallis test
  kruskal_test <- kruskal.test(CombinedZ ~ Group, data = zscore1_data)
  test_name <- "Kruskal-Wallis"
  test_pval <- kruskal_test$p.value
  # Determine the significance level for Kruskal-Wallis test
  sig_levels_kw <- sprintf("%.3f", p.adjust(test_pval, method = "BH"))
  pairwise_kw <- dunn_test(zscore2_data, CombinedZ~Group,
                           p.adjust.method = "holm")
}

# Format the posthoc comparisons for geom_signif
formatted_comp <- posthoc_comp %>%
  as.data.frame() %>%
  mutate(.y = factor(.y, levels = unique(.y)))  # Ensure correct order of factor levels


# Plot data and t-test p-value
zscore1_data %>%
  ggplot(aes(Group, CombinedZ, color = Group)) +
  scale_x_discrete(name = NULL, expand = c(0.3, 0.1)) + 
  scale_y_continuous(expand = c(0.1, 0.1)) +
  geom_jitter(aes(fill = Group), size = 4, alpha = 0.7, width = 0.2, shape = 16) +
  stat_summary(
    fun.min=function(z) {quantile(z,0.25)},
    fun.max=function(z) {quantile(z,0.75)},
    fun=median,
    color="black",
    size=0.8,
    shape=16
  ) +
  labs(title = bquote(~bold("zscore1")),
       subtitle = "(Depressive Score)",
       caption = "",
       x = NULL,
       y = "z score [a.u.]") +
  scale_color_manual(name = NULL, values = group_cols) +
  scale_fill_manual(name = NULL, values = group_cols) +
  theme_minimal_hgrid(12, rel_small = 1) +
  theme(plot.title = element_text(hjust = 0.5, face = "plain"),
        plot.subtitle = element_text(hjust = 0.5, size = 10, face = "plain"),
        legend.position = "none",  # Remove the legend
        axis.title.x = element_blank(),
        axis.text.x = element_text(),  # Rotate and align x-axis labels
        axis.ticks.x = element_blank()
  )

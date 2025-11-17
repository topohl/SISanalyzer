# Test for Batch effects using a linear mixed-effects model

# Load necessary packages
library(readxl)
library(ggplot2)
library(lmerTest)

# Load data
data_dir <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/SIS_Analysis"
filename <- "E9_Behavior_Data.xlsx"
sheet_name <- "NOR_SLEAP"

data <- read_excel(file.path(data_dir, filename), sheet = sheet_name)

# Convert variables to appropriate types
data$Sex <- factor(data$Sex)
data$Batch <- factor(data$Batch)

# Plot D2 by Batch and Sex
plot_obj <- ggplot(data, aes(x = Batch, y = D2, fill = Batch)) +
  geom_boxplot() +
  facet_wrap(~ Sex) +
  theme_minimal()

print(plot_obj)

# Linear Mixed Model: Batch as random effect, Sex as fixed effect
lmm <- lmer(D2 ~ Sex + (1 | Batch), data = data)

# Model summary
cat("\n--- Mixed Model Summary ---\n")
print(summary(lmm))

# ANOVA-style table for fixed effects
cat("\n--- ANOVA Table for Fixed Effects ---\n")
print(anova(lmm))

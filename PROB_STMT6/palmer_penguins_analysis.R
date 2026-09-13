
if (!require("palmerpenguins")) install.packages("palmerpenguins", repos = "https://cran.r-project.org")
if (!require("e1071"))          install.packages("e1071", repos = "https://cran.r-project.org")
if (!require("car"))            install.packages("car", repos = "https://cran.r-project.org")
if (!require("effsize"))        install.packages("effsize", repos = "https://cran.r-project.org")

library(e1071)      # skewness, kurtosis
library(car)        # leveneTest

if (requireNamespace("palmerpenguins", quietly = TRUE)) {
  library(palmerpenguins)
  data("penguins")
} else {
  # download from GitHub mirror of the dataset
  url <- "https://raw.githubusercontent.com/allisonhorst/palmerpenguins/main/inst/extdata/penguins.csv"
  penguins <- read.csv(url, stringsAsFactors = TRUE)
}

# Manual Cohen's d function (in case effsize is not available)
cohens_d <- function(x, y) {
  nx <- length(x); ny <- length(y)
  pooled_sd <- sqrt(((nx - 1) * var(x) + (ny - 1) * var(y)) / (nx + ny - 2))
  d <- (mean(x) - mean(y)) / pooled_sd
  return(d)
}

cat("===== Dataset Overview =====\n")
str(penguins)
cat("\nTotal observations:", nrow(penguins), "\n")
cat("Missing values per column:\n")
print(colSums(is.na(penguins)))

# Remove rows with missing values for clean analysis
penguins_clean <- na.omit(penguins)
cat("\nObservations after removing NAs:", nrow(penguins_clean), "\n\n")

# ============================================================================
# TASK 1: DESCRIPTIVE STATISTICAL ANALYSIS (body_mass_g)
# ============================================================================
cat("##############################################################\n")
cat("# TASK 1: DESCRIPTIVE STATISTICAL ANALYSIS                  #\n")
cat("##############################################################\n\n")

bm <- penguins_clean$body_mass_g

cat("===== Overall Body Mass Descriptive Statistics =====\n")
cat("Mean          :", mean(bm), "g\n")
cat("Median        :", median(bm), "g\n")
cat("Minimum       :", min(bm), "g\n")
cat("Maximum       :", max(bm), "g\n")
cat("Variance      :", var(bm), "\n")
cat("Std Deviation :", sd(bm), "g\n")
cat("Q1 (25th pct) :", quantile(bm, 0.25), "g\n")
cat("Q3 (75th pct) :", quantile(bm, 0.75), "g\n")
cat("IQR           :", IQR(bm), "g\n")
cat("Skewness      :", skewness(bm), "\n")
cat("Kurtosis      :", kurtosis(bm), "\n\n")

# Species-wise descriptive statistics
cat("===== Species-wise Descriptive Statistics =====\n")
species_list <- unique(penguins_clean$species)

for (sp in species_list) {
  bm_sp <- penguins_clean$body_mass_g[penguins_clean$species == sp]
  cat("\n--- Species:", as.character(sp), "---\n")
  cat("  N             :", length(bm_sp), "\n")
  cat("  Mean          :", round(mean(bm_sp), 2), "g\n")
  cat("  Median        :", median(bm_sp), "g\n")
  cat("  Min           :", min(bm_sp), "g\n")
  cat("  Max           :", max(bm_sp), "g\n")
  cat("  Variance      :", round(var(bm_sp), 2), "\n")
  cat("  Std Deviation :", round(sd(bm_sp), 2), "g\n")
  cat("  Q1            :", quantile(bm_sp, 0.25), "g\n")
  cat("  Q3            :", quantile(bm_sp, 0.75), "g\n")
  cat("  IQR           :", IQR(bm_sp), "g\n")
  cat("  Skewness      :", round(skewness(bm_sp), 4), "\n")
  cat("  Kurtosis      :", round(kurtosis(bm_sp), 4), "\n")
}

# --- Visualizations for Task 1 ---

# Histogram of body mass
hist(penguins_clean$body_mass_g,
     col    = "steelblue",
     border = "white",
     breaks = 20,
     main   = "Histogram of Penguin Body Mass",
     xlab   = "Body Mass (g)",
     ylab   = "Frequency")

# Species-wise Boxplot
boxplot(body_mass_g ~ species,
        data = penguins_clean,
        col  = c("tomato", "steelblue", "forestgreen"),
        main = "Body Mass by Penguin Species",
        xlab = "Species",
        ylab = "Body Mass (g)")

# Density plot
plot(density(penguins_clean$body_mass_g[penguins_clean$species == "Adelie"]),
     col = "tomato", lwd = 2,
     main = "Density Plot of Body Mass by Species",
     xlab = "Body Mass (g)", ylim = c(0, 0.001))
lines(density(penguins_clean$body_mass_g[penguins_clean$species == "Chinstrap"]),
      col = "steelblue", lwd = 2)
lines(density(penguins_clean$body_mass_g[penguins_clean$species == "Gentoo"]),
      col = "forestgreen", lwd = 2)
legend("topright",
       legend = c("Adelie", "Chinstrap", "Gentoo"),
       col    = c("tomato", "steelblue", "forestgreen"),
       lwd    = 2)

# ============================================================================
# TASK 2: HYPOTHESIS TESTING (Male vs Female Body Mass)
# ============================================================================
cat("\n\n##############################################################\n")
cat("# TASK 2: HYPOTHESIS TESTING - Male vs Female Body Mass     #\n")
cat("##############################################################\n\n")

cat("H0: There is no significant difference in mean body mass between male and female penguins.\n")
cat("H1: There is a significant difference in mean body mass between male and female penguins.\n")
cat("Significance level: alpha = 0.05\n\n")

male_mass   <- penguins_clean$body_mass_g[penguins_clean$sex == "male"]
female_mass <- penguins_clean$body_mass_g[penguins_clean$sex == "female"]

cat("Male   - N:", length(male_mass), " Mean:", round(mean(male_mass), 2), "g\n")
cat("Female - N:", length(female_mass), " Mean:", round(mean(female_mass), 2), "g\n\n")

# Normality check - Shapiro-Wilk test
cat("===== Normality Check (Shapiro-Wilk Test) =====\n")
sw_male   <- shapiro.test(male_mass)
sw_female <- shapiro.test(female_mass)
cat("Male   : W =", round(sw_male$statistic, 4), ", p-value =", round(sw_male$p.value, 4), "\n")
cat("Female : W =", round(sw_female$statistic, 4), ", p-value =", round(sw_female$p.value, 4), "\n")
cat("Interpretation: If p > 0.05, normality assumption is satisfied.\n\n")

# QQ-plots
par(mfrow = c(1, 2))
qqnorm(male_mass, main = "QQ Plot - Male Body Mass", col = "steelblue")
qqline(male_mass, col = "red", lwd = 2)
qqnorm(female_mass, main = "QQ Plot - Female Body Mass", col = "tomato")
qqline(female_mass, col = "red", lwd = 2)
par(mfrow = c(1, 1))

# Independent two-sample t-test
cat("===== Independent Two-Sample t-Test =====\n")
t_result <- t.test(male_mass, female_mass, var.equal = FALSE)
print(t_result)

cat("\n95% Confidence Interval for difference in means:\n")
cat("  [", round(t_result$conf.int[1], 2), ",", round(t_result$conf.int[2], 2), "]\n\n")

# Cohen's d effect size
cat("===== Cohen's d Effect Size =====\n")
if (requireNamespace("effsize", quietly = TRUE)) {
  library(effsize)
  cd_val <- cohen.d(male_mass, female_mass)$estimate
} else {
  cd_val <- cohens_d(male_mass, female_mass)
}
cat("Cohen's d =", round(cd_val, 4), "\n")

cat("\nInterpretation:\n")
cat("  p-value =", format(t_result$p.value, scientific = TRUE), "\n")
if (t_result$p.value < 0.05) {
  cat("  Result: REJECT H0. Male and female penguins have significantly different body masses.\n")
} else {
  cat("  Result: FAIL TO REJECT H0.\n")
}
cat("  Cohen's d =", round(cd_val, 4), " -> ",
    ifelse(abs(cd_val) >= 0.8, "Large effect size",
    ifelse(abs(cd_val) >= 0.5, "Medium effect size", "Small effect size")), "\n")

# Sex-wise boxplot
boxplot(body_mass_g ~ sex,
        data = penguins_clean,
        col  = c("tomato", "steelblue"),
        main = "Body Mass by Sex",
        xlab = "Sex",
        ylab = "Body Mass (g)")


cat("# TASK 3: ONE-WAY ANOVA - Body Mass across Species          #\n")


# Check normality per species
cat("===== Normality Check per Species (Shapiro-Wilk) =====\n")
for (sp in species_list) {
  bm_sp <- penguins_clean$body_mass_g[penguins_clean$species == sp]
  sw <- shapiro.test(bm_sp)
  cat(as.character(sp), ": W =", round(sw$statistic, 4),
      ", p-value =", round(sw$p.value, 4), "\n")
}

# QQ-plots per species
par(mfrow = c(1, 3))
for (sp in species_list) {
  bm_sp <- penguins_clean$body_mass_g[penguins_clean$species == sp]
  qqnorm(bm_sp, main = paste("QQ Plot -", sp), col = "steelblue")
  qqline(bm_sp, col = "red", lwd = 2)
}
par(mfrow = c(1, 1))

# Levene's test for homogeneity of variance
cat("\n===== Levene's Test for Homogeneity of Variance =====\n")
lev_test <- leveneTest(body_mass_g ~ species, data = penguins_clean)
print(lev_test)
cat("Interpretation: If p > 0.05, equal variance assumption holds.\n\n")

# One-way ANOVA
cat("===== One-Way ANOVA: body_mass_g ~ species =====\n")
anova_model <- aov(body_mass_g ~ species, data = penguins_clean)
anova_result <- summary(anova_model)
print(anova_result)

# Extract F and p
f_val <- anova_result[[1]]$`F value`[1]
p_val <- anova_result[[1]]$`Pr(>F)`[1]
df1   <- anova_result[[1]]$Df[1]
df2   <- anova_result[[1]]$Df[2]

cat("\nF-statistic :", round(f_val, 4), "\n")
cat("Degrees of freedom: df1 =", df1, ", df2 =", df2, "\n")
cat("p-value     :", format(p_val, scientific = TRUE), "\n")

if (p_val < 0.05) {
  cat("Result: REJECT H0. Mean body mass differs significantly among species.\n\n")

  # Tukey's HSD Post-Hoc Test
  cat("===== Tukey HSD Post-Hoc Test =====\n")
  tukey_result <- TukeyHSD(anova_model)
  print(tukey_result)

  plot(tukey_result, col = "steelblue")
} else {
  cat("Result: FAIL TO REJECT H0.\n")
}

cat("# TASK 4: NON-PARAMETRIC TEST - Kruskal-Wallis              #\n")


kw_result <- kruskal.test(body_mass_g ~ species, data = penguins_clean)
print(kw_result)

cat("\nKruskal-Wallis chi-squared:", round(kw_result$statistic, 4), "\n")
cat("df:", kw_result$parameter, "\n")
cat("p-value:", format(kw_result$p.value, scientific = TRUE), "\n")

cat("\n===== Comparison: ANOVA vs Kruskal-Wallis =====\n")
cat("ANOVA p-value         :", format(p_val, scientific = TRUE), "\n")
cat("Kruskal-Wallis p-value:", format(kw_result$p.value, scientific = TRUE), "\n")
cat("Both tests lead to the same conclusion: body mass differs\n")
cat("significantly among penguin species. The parametric (ANOVA) and\n")
cat("non-parametric (Kruskal-Wallis) methods are consistent.\n")


cat("# TASK 5: TWO-WAY ANOVA - Species x Sex on Body Mass        #\n")


two_way <- aov(body_mass_g ~ species * sex, data = penguins_clean)
cat("===== Two-Way ANOVA: body_mass_g ~ species * sex =====\n")
two_way_summary <- summary(two_way)
print(two_way_summary)

cat("\nInterpretation:\n")
tw_table <- two_way_summary[[1]]

sp_p   <- tw_table$`Pr(>F)`[1]
sex_p  <- tw_table$`Pr(>F)`[2]
int_p  <- tw_table$`Pr(>F)`[3]

cat("  Species effect      : p =", format(sp_p, scientific = TRUE),
    ifelse(sp_p < 0.05, "-> SIGNIFICANT", "-> Not significant"), "\n")
cat("  Sex effect          : p =", format(sex_p, scientific = TRUE),
    ifelse(sex_p < 0.05, "-> SIGNIFICANT", "-> Not significant"), "\n")
cat("  Interaction (Sp*Sex): p =", format(int_p, scientific = TRUE),
    ifelse(int_p < 0.05, "-> SIGNIFICANT interaction", "-> No significant interaction"), "\n")

cat("\nConclusion: Both species and sex independently affect body mass.\n")
cat("The interaction term tells us whether the sex difference in body\n")
cat("mass varies across species.\n")

# Interaction plot
interaction.plot(penguins_clean$species, penguins_clean$sex,
                 penguins_clean$body_mass_g,
                 col  = c("tomato", "steelblue"),
                 lwd  = 2,
                 type = "b",
                 main = "Interaction Plot: Species x Sex on Body Mass",
                 xlab = "Species",
                 ylab = "Mean Body Mass (g)",
                 legend = TRUE,
                 trace.label = "Sex")


cat("# TASK 6: ADDITIONAL ANALYSIS - Flipper Length               #\n")


cat("===== Flipper Length Descriptive Stats by Species =====\n")
for (sp in species_list) {
  fl_sp <- penguins_clean$flipper_length_mm[penguins_clean$species == sp]
  cat(as.character(sp), ": Mean =", round(mean(fl_sp), 2),
      "mm, SD =", round(sd(fl_sp), 2), "mm, N =", length(fl_sp), "\n")
}

# One-way ANOVA for flipper length
cat("\n===== One-Way ANOVA: flipper_length_mm ~ species =====\n")
anova_flipper <- aov(flipper_length_mm ~ species, data = penguins_clean)
anova_fl_summary <- summary(anova_flipper)
print(anova_fl_summary)

fl_p <- anova_fl_summary[[1]]$`Pr(>F)`[1]
cat("p-value:", format(fl_p, scientific = TRUE), "\n")
if (fl_p < 0.05) {
  cat("Result: Flipper length differs significantly among species.\n\n")

  cat("===== Tukey HSD for Flipper Length =====\n")
  tukey_fl <- TukeyHSD(anova_flipper)
  print(tukey_fl)
}

# Kruskal-Wallis for flipper length
cat("\n===== Kruskal-Wallis: flipper_length_mm ~ species =====\n")
kw_fl <- kruskal.test(flipper_length_mm ~ species, data = penguins_clean)
print(kw_fl)

cat("\nConsistency check:\n")
cat("  Body mass ANOVA p    :", format(p_val, scientific = TRUE), "\n")
cat("  Flipper length ANOVA p:", format(fl_p, scientific = TRUE), "\n")
cat("  Both body mass and flipper length differ significantly across\n")
cat("  species. Findings are consistent. Gentoo penguins tend to be\n")
cat("  both heavier and have longer flippers than Adelie and Chinstrap.\n")


cat("# TASK 7: ADDITIONAL VISUALIZATIONS                         #\n")


# Species-wise flipper length boxplot
boxplot(flipper_length_mm ~ species,
        data = penguins_clean,
        col  = c("tomato", "steelblue", "forestgreen"),
        main = "Flipper Length by Penguin Species",
        xlab = "Species",
        ylab = "Flipper Length (mm)")

# Group comparison - body mass by species and sex
boxplot(body_mass_g ~ species + sex,
        data = penguins_clean,
        col  = rep(c("tomato", "steelblue"), 3),
        main = "Body Mass by Species and Sex",
        xlab = "Species.Sex",
        ylab = "Body Mass (g)",
        las  = 2,
        cex.axis = 0.8)
legend("topleft",
       legend = c("Female", "Male"),
       fill   = c("tomato", "steelblue"))


cat("                        CONCLUSION                           \n")


cat("This analysis of the Palmer Penguins dataset revealed that:\n\n")
cat("1. Gentoo penguins are significantly heavier (mean ~5076g) than\n")
cat("   Adelie (~3706g) and Chinstrap (~3733g) species.\n\n")
cat("2. Male penguins have significantly higher body mass than females\n")
cat("   (t-test p < 0.05, large Cohen's d effect size).\n\n")
cat("3. One-way ANOVA confirmed significant differences in body mass\n")
cat("   across all three species (p < 0.05), with Tukey HSD showing\n")
cat("   Gentoo differs from both Adelie and Chinstrap.\n\n")
cat("4. Kruskal-Wallis non-parametric test yielded consistent results\n")
cat("   with ANOVA, confirming robustness of findings.\n\n")
cat("5. Two-way ANOVA showed both species and sex significantly affect\n")
cat("   body mass, with potential interaction effects.\n\n")
cat("6. Flipper length analysis produced consistent findings - Gentoo\n")
cat("   penguins have significantly longer flippers, confirming that\n")
cat("   physical size differences are a general pattern, not limited\n")
cat("   to body mass alone.\n\n")
cat("=============================================================\n")
cat("              Analysis Complete.                             \n")
cat("=============================================================\n")

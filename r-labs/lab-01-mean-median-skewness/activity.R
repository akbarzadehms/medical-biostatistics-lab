# Created by: Mahdi Akbarzadeh
# Purpose: Activity for mean, median, skewness, and outliers.
#
# Question:
# How does an extreme value affect the mean and median of a medical variable?
#
# Key idea:
# The mean uses every value and is sensitive to extreme observations.
# The median is based on rank order and is more resistant to outliers.
#
# Data note:
# The teaching dataset is generated inside this script.
# No external data file is required.

set.seed(2026)

cat("Medical Biostatistics Lab - Activity 01\n")
cat("Topic: Mean, median, skewness, and outliers\n")
cat("Created by: Mahdi Akbarzadeh\n\n")

# ------------------------------------------------------------
# 1) Simulate a small systolic blood pressure dataset
# ------------------------------------------------------------

systolic_bp <- round(rnorm(n = 30, mean = 128, sd = 8), 1)
systolic_bp_with_outlier <- c(systolic_bp, 220)

# ------------------------------------------------------------
# 2) Compute summaries
# ------------------------------------------------------------

summary_table <- data.frame(
  scenario = c("Original data", "With one extreme high value"),
  n = c(length(systolic_bp), length(systolic_bp_with_outlier)),
  mean = c(mean(systolic_bp), mean(systolic_bp_with_outlier)),
  median = c(median(systolic_bp), median(systolic_bp_with_outlier)),
  sd = c(sd(systolic_bp), sd(systolic_bp_with_outlier))
)

cat("Summary table:\n")
print(summary_table)

# ------------------------------------------------------------
# 3) Create output folder and save the figure
# ------------------------------------------------------------

output_dir <- file.path(getwd(), "activity_outputs")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

output_file <- file.path(output_dir, "activity-01-mean-median-skewness.png")

png(output_file, width = 1100, height = 520)
par(mfrow = c(1, 2), mar = c(5, 4, 4, 2))

hist(
  systolic_bp,
  breaks = 10,
  main = "Original simulated BP data",
  xlab = "Systolic blood pressure",
  col = "lightblue",
  border = "white"
)
abline(v = mean(systolic_bp), col = "blue", lwd = 3, lty = 2)
abline(v = median(systolic_bp), col = "red", lwd = 3)
legend(
  "topright",
  legend = c("Mean", "Median"),
  col = c("blue", "red"),
  lwd = 3,
  lty = c(2, 1),
  bty = "n"
)

hist(
  systolic_bp_with_outlier,
  breaks = 12,
  main = "After adding one extreme value",
  xlab = "Systolic blood pressure",
  col = "lightblue",
  border = "white"
)
abline(v = mean(systolic_bp_with_outlier), col = "blue", lwd = 3, lty = 2)
abline(v = median(systolic_bp_with_outlier), col = "red", lwd = 3)
legend(
  "topright",
  legend = c("Mean", "Median"),
  col = c("blue", "red"),
  lwd = 3,
  lty = c(2, 1),
  bty = "n"
)

dev.off()

cat("\nFigure saved to:\n")
cat(normalizePath(output_file, mustWork = FALSE), "\n")

# ------------------------------------------------------------
# 4) Student interpretation task
# ------------------------------------------------------------

cat("\nStudent interpretation task:\n")
cat("1. Which summary changed more after adding the extreme value: mean or median?\n")
cat("2. In a skewed medical variable, would you report mean ± SD or median (IQR)? Why?\n")

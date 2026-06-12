# Created by: Mahdi Akbarzadeh
# Medical Biostatistics Teaching Lab
# Module 05: Normal distribution and medical measurements

set.seed(140405)

# This activity uses simulated systolic blood pressure values.
# No external data file is required.

n <- 400
systolic_bp <- rnorm(n = n, mean = 120, sd = 15)

bp_mean <- mean(systolic_bp)
bp_sd <- sd(systolic_bp)
bp_median <- median(systolic_bp)

within_1sd <- mean(systolic_bp >= bp_mean - bp_sd & systolic_bp <= bp_mean + bp_sd) * 100
within_2sd <- mean(systolic_bp >= bp_mean - 2 * bp_sd & systolic_bp <= bp_mean + 2 * bp_sd) * 100
within_3sd <- mean(systolic_bp >= bp_mean - 3 * bp_sd & systolic_bp <= bp_mean + 3 * bp_sd) * 100

summary_table <- data.frame(
  Statistic = c("Sample size", "Mean", "Median", "Standard deviation",
                "Percent within mean +/- 1 SD",
                "Percent within mean +/- 2 SD",
                "Percent within mean +/- 3 SD"),
  Value = c(
    n,
    round(bp_mean, 2),
    round(bp_median, 2),
    round(bp_sd, 2),
    paste0(round(within_1sd, 1), "%"),
    paste0(round(within_2sd, 1), "%"),
    paste0(round(within_3sd, 1), "%")
  )
)

cat("\nSummary table\n")
print(summary_table, row.names = FALSE)

output_dir <- file.path(getwd(), "activity_outputs")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

output_file <- file.path(output_dir, "module05_normal_distribution_systolic_bp.png")

png(filename = output_file, width = 1200, height = 800, res = 130)

hist(
  systolic_bp,
  breaks = 28,
  probability = TRUE,
  main = "Simulated systolic blood pressure values",
  xlab = "Systolic blood pressure (mmHg)",
  ylab = "Density",
  border = "white"
)

lines(
  density(systolic_bp),
  lwd = 3
)

abline(v = bp_mean, lwd = 3)
abline(v = c(bp_mean - bp_sd, bp_mean + bp_sd), lwd = 2, lty = 2)
abline(v = c(bp_mean - 2 * bp_sd, bp_mean + 2 * bp_sd), lwd = 2, lty = 3)

legend(
  "topright",
  legend = c("Mean", "Mean +/- 1 SD", "Mean +/- 2 SD"),
  lwd = c(3, 2, 2),
  lty = c(1, 2, 3),
  bty = "n"
)

dev.off()

cat("\nFigure saved to:\n")
cat(normalizePath(output_file, mustWork = FALSE), "\n")

cat("\nStudent interpretation task\n")
cat("1. Is the simulated distribution approximately symmetric?\n")
cat("2. What percentage of values fell within mean +/- 1 SD? Compare it with the 68% rule.\n")
cat("3. What percentage of values fell within mean +/- 2 SD? Compare it with the 95% rule.\n")
cat("4. Why should normality be checked before using normal-based interpretations in real medical data?\n")

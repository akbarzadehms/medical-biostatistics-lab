# Created by: Mahdi Akbarzadeh
# Purpose: Activity for coefficient of variation.
#
# Question:
# Can a smaller raw SD still correspond to larger relative variability?
#
# Key idea:
# CV = SD / Mean * 100. It compares variability relative to the mean.
#
# Data note:
# The teaching dataset is generated inside this script.
# No external data file is required.

set.seed(2026)

cat("Medical Biostatistics Lab - Activity 04\n")
cat("Topic: Coefficient of variation\n")
cat("Created by: Mahdi Akbarzadeh\n\n")

# ------------------------------------------------------------
# 1) Simulate repeated measurements from two laboratory analyzers
# ------------------------------------------------------------

analyzer_a <- round(rnorm(n = 12, mean = 250, sd = 12), 1)
analyzer_b <- round(rnorm(n = 12, mean = 5.0, sd = 0.35), 2)

# ------------------------------------------------------------
# 2) Compute mean, SD, and CV
# ------------------------------------------------------------

cv_percent <- function(x) {
  sd(x) / mean(x) * 100
}

summary_table <- rbind(
  data.frame(
    analyzer = "Analyzer A",
    mean = mean(analyzer_a),
    sd = sd(analyzer_a),
    cv_percent = cv_percent(analyzer_a)
  ),
  data.frame(
    analyzer = "Analyzer B",
    mean = mean(analyzer_b),
    sd = sd(analyzer_b),
    cv_percent = cv_percent(analyzer_b)
  )
)

cat("Summary table:\n")
print(summary_table)

# ------------------------------------------------------------
# 3) Create output folder and save the figure
# ------------------------------------------------------------

raw_sd <- summary_table$sd
names(raw_sd) <- summary_table$analyzer

cv_values <- summary_table$cv_percent
names(cv_values) <- summary_table$analyzer

output_dir <- file.path(getwd(), "activity_outputs")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

output_file <- file.path(output_dir, "activity-04-coefficient-of-variation.png")

png(output_file, width = 1000, height = 520)
par(mfrow = c(1, 2), mar = c(6, 4, 4, 2))

barplot(
  raw_sd,
  main = "Raw standard deviation",
  ylab = "SD",
  col = "lightblue",
  las = 2
)

barplot(
  cv_values,
  main = "Coefficient of variation",
  ylab = "CV (%)",
  col = "lightgreen",
  las = 2
)

dev.off()

cat("\nFigure saved to:\n")
cat(normalizePath(output_file, mustWork = FALSE), "\n")

# ------------------------------------------------------------
# 4) Student interpretation task
# ------------------------------------------------------------

cat("\nStudent interpretation task:\n")
cat("1. Which analyzer has the smaller raw SD?\n")
cat("2. Which analyzer has the smaller CV?\n")
cat("3. Why can raw SD alone be misleading when measurement scales are different?\n")

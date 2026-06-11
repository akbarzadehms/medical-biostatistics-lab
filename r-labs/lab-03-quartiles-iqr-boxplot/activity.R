# Created by: Mahdi Akbarzadeh
# Purpose: Activity for quartiles, IQR, boxplots, and outliers.
#
# Question:
# Does every value above Q3 count as an outlier?
#
# Key idea:
# IQR describes the middle 50% of data. Outlier screening requires fences.
#
# Data note:
# The teaching dataset is generated inside this script.
# No external data file is required.

set.seed(2026)

cat("Medical Biostatistics Lab - Activity 03\n")
cat("Topic: Quartiles, IQR, boxplots, and outliers\n")
cat("Created by: Mahdi Akbarzadeh\n\n")

# ------------------------------------------------------------
# 1) Simulate a right-skewed hospital-stay dataset
# ------------------------------------------------------------

typical_stays <- rpois(n = 40, lambda = 4) + 1
long_stays <- c(18, 24, 36, 55)
hospital_stay_days <- c(typical_stays, long_stays)

# ------------------------------------------------------------
# 2) Compute quartiles, IQR, and outlier fences
# ------------------------------------------------------------

q1 <- as.numeric(quantile(hospital_stay_days, 0.25))
median_value <- median(hospital_stay_days)
q3 <- as.numeric(quantile(hospital_stay_days, 0.75))
iqr_value <- IQR(hospital_stay_days)

lower_fence <- q1 - 1.5 * iqr_value
upper_fence <- q3 + 1.5 * iqr_value

outliers <- hospital_stay_days[
  hospital_stay_days < lower_fence | hospital_stay_days > upper_fence
]

summary_table <- data.frame(
  q1 = q1,
  median = median_value,
  q3 = q3,
  iqr = iqr_value,
  lower_fence = lower_fence,
  upper_fence = upper_fence,
  outlier_count = length(outliers)
)

cat("Summary table:\n")
print(summary_table)

cat("\nOutlier values detected by the 1.5 x IQR rule:\n")
print(data.frame(outlier_values = outliers))

# ------------------------------------------------------------
# 3) Create output folder and save the figure
# ------------------------------------------------------------

output_dir <- file.path(getwd(), "activity_outputs")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

output_file <- file.path(output_dir, "activity-03-quartiles-iqr-boxplot.png")

png(output_file, width = 900, height = 520)
boxplot(
  hospital_stay_days,
  horizontal = TRUE,
  main = "Hospital stay: IQR and outlier fences",
  xlab = "Days",
  col = "lightblue"
)
abline(v = lower_fence, col = "orange", lwd = 2, lty = 2)
abline(v = upper_fence, col = "orange", lwd = 2, lty = 2)
text(
  x = upper_fence,
  y = 1.25,
  labels = "Upper fence",
  col = "orange",
  pos = 4
)
dev.off()

cat("\nFigure saved to:\n")
cat(normalizePath(output_file, mustWork = FALSE), "\n")

# ------------------------------------------------------------
# 4) Student interpretation task
# ------------------------------------------------------------

cat("\nStudent interpretation task:\n")
cat("1. What does the IQR represent in this hospital-stay dataset?\n")
cat("2. Why is a value above Q3 not automatically an outlier?\n")

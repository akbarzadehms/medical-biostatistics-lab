# Created by: Mahdi Akbarzadeh
# Purpose: Activity for variability, variance, and standard deviation.
#
# Question:
# Can two datasets with similar centers have very different spread?
#
# Key idea:
# The mean alone is not enough. Spread changes the interpretation of a dataset.
#
# Data note:
# The teaching dataset is defined inside this script.
# No external data file is required.

cat("Medical Biostatistics Lab - Activity 02\n")
cat("Topic: Variability, variance, and standard deviation\n")
cat("Created by: Mahdi Akbarzadeh\n\n")

# ------------------------------------------------------------
# 1) Define two small teaching datasets
# ------------------------------------------------------------

class_a <- c(10, 12, 18, 22, 28)
class_b <- c(15, 16, 16, 17, 16)

scores <- data.frame(
  class = rep(c("Class A", "Class B"), each = 5),
  score = c(class_a, class_b)
)

# ------------------------------------------------------------
# 2) Compute center and spread summaries
# ------------------------------------------------------------

compute_summary <- function(x) {
  data.frame(
    mean = mean(x),
    median = median(x),
    range = diff(range(x)),
    variance = var(x),
    sd = sd(x)
  )
}

summary_table <- rbind(
  data.frame(class = "Class A", compute_summary(class_a)),
  data.frame(class = "Class B", compute_summary(class_b))
)

cat("Summary table:\n")
print(summary_table)

class_a_deviation_table <- data.frame(
  score = class_a,
  deviation_from_mean = class_a - mean(class_a),
  squared_deviation = (class_a - mean(class_a))^2
)

cat("\nClass A deviation table:\n")
print(class_a_deviation_table)

# ------------------------------------------------------------
# 3) Create output folder and save the figure
# ------------------------------------------------------------

output_dir <- file.path(getwd(), "activity_outputs")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

output_file <- file.path(output_dir, "activity-02-variability-standard-deviation.png")

png(output_file, width = 900, height = 520)
stripchart(
  score ~ class,
  data = scores,
  vertical = TRUE,
  method = "jitter",
  pch = 19,
  col = "steelblue",
  main = "Similar center, different variability",
  xlab = "Group",
  ylab = "Score",
  ylim = c(5, 32)
)
points(
  x = c(1, 2),
  y = c(mean(class_a), mean(class_b)),
  pch = 18,
  col = "red",
  cex = 2
)
legend(
  "topright",
  legend = c("Individual scores", "Mean"),
  col = c("steelblue", "red"),
  pch = c(19, 18),
  bty = "n"
)
dev.off()

cat("\nFigure saved to:\n")
cat(normalizePath(output_file, mustWork = FALSE), "\n")

# ------------------------------------------------------------
# 4) Student interpretation task
# ------------------------------------------------------------

cat("\nStudent interpretation task:\n")
cat("1. If two groups have similar means but different SDs, what extra information does SD provide?\n")
cat("2. Why is it incomplete to compare groups using only the mean?\n")

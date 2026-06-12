# Created by: Mahdi Akbarzadeh
# Medical Biostatistics Teaching Lab
# Module 06: Z-scores, percentiles, and relative position

# This activity uses reference distributions defined inside the script.
# No external data file is required.

calculate_z <- function(value, reference_mean, reference_sd) {
  (value - reference_mean) / reference_sd
}

birth_weight_value <- 3300
birth_weight_mean <- 3100
birth_weight_sd <- 450

test_score_value <- 82
test_score_mean <- 70
test_score_sd <- 10

birth_weight_z <- calculate_z(
  value = birth_weight_value,
  reference_mean = birth_weight_mean,
  reference_sd = birth_weight_sd
)

test_score_z <- calculate_z(
  value = test_score_value,
  reference_mean = test_score_mean,
  reference_sd = test_score_sd
)

birth_weight_percentile <- pnorm(birth_weight_z) * 100
test_score_percentile <- pnorm(test_score_z) * 100

results_table <- data.frame(
  Example = c("Birth weight", "Test score"),
  Raw_value = c(birth_weight_value, test_score_value),
  Reference_mean = c(birth_weight_mean, test_score_mean),
  Reference_sd = c(birth_weight_sd, test_score_sd),
  Z_score = round(c(birth_weight_z, test_score_z), 2),
  Percentile = paste0(round(c(birth_weight_percentile, test_score_percentile), 1), "%")
)

cat("\nZ-score and percentile results\n")
print(results_table, row.names = FALSE)

output_dir <- file.path(getwd(), "activity_outputs")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

output_file_birth_weight <- file.path(output_dir, "module06_birth_weight_z_score.png")
output_file_test_score <- file.path(output_dir, "module06_test_score_z_score.png")

draw_z_curve <- function(z_value, output_file, main_title) {
  x <- seq(-4, 4, length.out = 1000)
  y <- dnorm(x)

  png(filename = output_file, width = 1200, height = 800, res = 130)

  plot(
    x,
    y,
    type = "l",
    lwd = 3,
    main = main_title,
    xlab = "Z-score",
    ylab = "Density"
  )

  shaded_x <- x[x <= z_value]
  shaded_y <- dnorm(shaded_x)

  polygon(
    c(min(shaded_x), shaded_x, max(shaded_x)),
    c(0, shaded_y, 0),
    border = NA
  )

  lines(x, y, lwd = 3)
  abline(v = 0, lwd = 2, lty = 2)
  abline(v = z_value, lwd = 3)

  legend(
    "topright",
    legend = c("Mean (Z = 0)", paste0("Observed value (Z = ", round(z_value, 2), ")")),
    lwd = c(2, 3),
    lty = c(2, 1),
    bty = "n"
  )

  dev.off()
}

draw_z_curve(
  z_value = birth_weight_z,
  output_file = output_file_birth_weight,
  main_title = "Birth weight: relative position under a normal model"
)

draw_z_curve(
  z_value = test_score_z,
  output_file = output_file_test_score,
  main_title = "Test score: relative position under a normal model"
)

cat("\nFigures saved to:\n")
cat(normalizePath(output_file_birth_weight, mustWork = FALSE), "\n")
cat(normalizePath(output_file_test_score, mustWork = FALSE), "\n")

cat("\nStudent interpretation task\n")
cat("1. Interpret the birth-weight Z-score in one complete sentence.\n")
cat("2. Interpret the birth-weight percentile as relative position, not as a percentage of health.\n")
cat("3. Interpret the test-score Z-score in one complete sentence.\n")
cat("4. Explain why raw values should not be compared without a reference distribution.\n")

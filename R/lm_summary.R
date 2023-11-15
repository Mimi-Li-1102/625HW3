#' lm_summary
#'
#' @param model linear model object
#' @param Clevel the confidence level for the intervals. Default is 0.95.
#' @return an extended summary table with the coefficient estimates and
#' confidence intervals.
#' @export
#'
#' @examples
#' x = 1
lm_summary <- function(model, Clevel = 0.95) {
  # Obtain the original summary function
  old_summary <- summary(model)

  # Extract coefficients, standard errors, and p-values
  coeff <- old_summary$coef[, "Estimate"]
  se <- old_summary$coef[, "Std. Error"]
  p_values <- old_summary$coef[, "Pr(>|t|)"]

  # Calculate confidence intervals
  z_value <- qnorm(1 - (1 - Clevel) / 2)
  lower_b <- coeff - z_value * se
  upper_b <- coeff + z_value * se

  # Determine significance depending the Clevel
  significant <- p_values < (1 - Clevel)

  # Create a new summary table with confidence intervals and significance
  with_intervals <- cbind(
    old_summary$coef[, c("Estimate", "Std. Error", "t value", "Pr(>|t|)")],
    CI = sprintf("(%0.4f, %0.4f)", lower_b, upper_b),
    Significant = significant
  )

  # Print the formatted table
  cat("Coefficients:\n")
  print(xtable::xtable(with_intervals, digits = c(3, 3, 3, 3, 3, 3, 3)),
        include.rownames = FALSE)

  # Return the summary with intervals and significance
  invisible(with_intervals)
}


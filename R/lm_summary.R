#' lm_summary
#' @import HW625
#' @param model linear model object
#' @param Clevel the confidence level for the intervals. Default is 0.95.
#' @return an summary table with information of coefficient and additional model information.
#' @examples
#' #load the mtcars dataset
#' data(mtcars)
#'
#' #fit a linear regression model
#' example_model <- lm(mpg ~ hp + wt, data = mtcars)
#'
#' #obtain summary of the linear model
#' lm_summary(example_model)
#' @export
lm_summary <- function(model, Clevel = 0.95) {
  # Calculate coefficients, standard errors, t-stats, and p-values
  coeff <- coef(model)
  se <- obtain_se(model)
  t_stats <- obtain_t_stats(model)
  p_values <- obtain_p_value(model)

  # Calculate confidence intervals
  z_value <- qnorm(1 - (1 - Clevel) / 2)
  lower_b <- coeff - z_value * se
  upper_b <- coeff + z_value * se

  # Determine significance
  significant <- p_values < (1 - Clevel)

  # Create a summary table with coefficients, intervals, and significance
  summary_table <- data.frame(
    Coefficient = names(coeff),
    Estimate = coeff,
    SE = se,
    t_Stat = t_stats,
    p_Value = formatC(p_values, format = "e", digits = 4),
    CI = sprintf("(%0.4f, %0.4f)", lower_b, upper_b),
    Significant = significant
  )

  # Print the formatted coefficient table
  cat("Coefficients:\n")
  cat(sprintf("%-15s %-15s %-15s %-15s %-15s %-15s\n", "Coefficient", "Estimate", "SE", "t_Stat", "p_Value", "Significant"))
  for (i in seq_along(coeff)) {
    cat(sprintf("%-15s %-15.4f %-15.4f %-15.4f %-15s %-15s\n",
                names(coeff)[i], coeff[i], se[i], t_stats[i], p_values[i],
                ifelse(significant[i], "TRUE", "FALSE")))
  }

  # Obtain and print additional model information
  residual_info <- obtain_residual_info(model)
  r_squared_info <- obtain_r_square(model)
  f_stat_info <- obtain_f_stats(model)

  cat("\nResidual standard error:", round(residual_info$residual_se, 4), "on", residual_info$df_resi, "degrees of freedom\n")
  cat("Multiple R-squared:", round(r_squared_info$r_square, 4), ",\tAdjusted R-squared:", round(r_squared_info$adj_r_square, 4), "\n")
  cat("F-statistic:", round(f_stat_info$f_statistic, 4), "on", f_stat_info$df_model, "and", f_stat_info$df_resi, "DF,  p-value:", format(f_stat_info$p_value, scientific = TRUE), "\n")

  # Return the summary table invisibly
  invisible(summary_table)
}


#' lm_summary
#'
#' @param model linear model object
#' @param Clevel the confidence level for the intervals. Default is 0.95.
#' @return an summary table with information of coefficient and additional model information.
#' @export
#'
#' @examples
#' x = 1
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
    p_Value = p_values,
    CI = sprintf("(%0.4f, %0.4f)", lower_b, upper_b),
    Significant = significant
  )

  # Print the formatted coefficient table
  cat("Coefficients:\n")
  print(xtable::xtable(summary_table, digits = c(4, 4, 4, 4, 4, 4)), include.rownames = FALSE)

  # Obtain and print additional model information
  residual_info <- obtain_residual_info(model)
  r_squared_info <- obtain_r_square(model)
  f_stat_info <- obtain_f_stats(model)

  cat("\nResidual standard error:", round(residual_info$residual_se, 4), "on", residual_info$df_resi, "degrees of freedom\n")
  cat("Multiple R-squared:", round(r_squared_info$r_square, 4), ",\tAdjusted R-squared:", round(r_squared_info$adj_r_square, 4), "\n")
  cat("F-statistic:", round(f_stat_info$f_statistic, 4), "on", f_stat_info$df_model, "and", f_stat_info$df_resi, "DF,  p-value:", format(p_value, scientific = TRUE), "\n")

  # Return the summary table invisibly
  invisible(summary_table)
}


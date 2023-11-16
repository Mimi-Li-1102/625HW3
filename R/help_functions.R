#' obtain_SSE
#'
#' @description
#' This function gives the SSE of the linear model.
#'
#'
#' @param model linear model object
#' @return SSE of the linear model
#' @export
#' @examples
#' data(mtcars)
#' example_model <- lm(mpg ~ hp + wt, data = mtcars)
#'
#' #obtain SSE
#' obtain_SSE(example_model)
obtain_SSE <- function(model){
  # Obtain residuals
  residual <- model$residuals

  # Calculate SSE
  sse <- sum(residual ^ 2)
  return(sse)
}


#' obtain_se
#'
#' @description
#' This function provides the standard errors for each coefficient in the linear model.
#'
#' @param model linear model object
#' @return coefficients standard errors
#' @export
#' @examples
#' data(mtcars)
#' example_model <- lm(mpg ~ hp + wt, data = mtcars)
#'
#' #obtain coefficients standard errors
#' obtain_se(example_model)
obtain_se <- function(model){
  # Get the variance-covariance matrix
  mtrx <- vcov(model)

  # Calculate standard error
  se <- sqrt(diag(mtrx))
  return(se)
}


#' obtain_t_stats
#'
#' @description
#' This function provides the t-statistics for each coefficient in the linear model.
#'
#' @param model linear model object
#' @return t-statistics
#' @export
#' @examples
#' data(mtcars)
#' example_model <- lm(mpg ~ hp + wt, data = mtcars)
#'
#' #obtain t statistics
#' obtain_t_stats(example_model)
obtain_t_stats <- function(model){
  t_stats <- coef(model) / obtain_se(model)
  return(t_stats)
}


#' obtain_p_value
#'
#' @description
#' This function provides the p-values corresponding to the t-statistics for each coefficient in the linear model.
#'
#' @param model linear model object
#' @return p values corresponding to each t-statistic for the linear model
#' @export
#' @examples
#' data(mtcars)
#' example_model <- lm(mpg ~ hp + wt, data = mtcars)
#'
#' #obtain p values
#' obtain_p_value(example_model)
obtain_p_value <- function(model){
  # Obtain the degrees of freedom associated with the residuals
  df_resi <- df.residual(model)

  # Calculate p-values
  t_stats <- obtain_t_stats(model)
  p_values <- 2 * (1 - pt(abs(t_stats), df_resi))
  return(p_values)
}


#' obtain_residual_info
#'
#' @description
#' This function provides the standard error and degrees of freedom for the residuals in the linear model.
#'
#' @param model linear model object
#' @return residuals standard error and degrees of freedom of the linear model
#' @export
#' @examples
#' data(mtcars)
#' example_model <- lm(mpg ~ hp + wt, data = mtcars)
#'
#' #obtain residual standard error and degrees of freedom
#' obtain_residual_info(example_model)
obtain_residual_info <- function(model){
  # Calculate residuals df
  df_resi <- df.residual(model)

  # Calculate residual standard error
  se <- sqrt(obtain_SSE(model) / df_resi)

  return(list(residual_se = se, df_resi = df_resi))
}


#' obtain_r_square
#'
#' @description
#' This function provides the R-square and adjusted R-square of the linear model.
#'
#' @param model linear model object
#' @return R square and adjusted R square of the linear model
#' @export
#' @examples
#' data(mtcars)
#' example_model <- lm(mpg ~ hp + wt, data = mtcars)
#'
#' #obtain R square and adjusted R square
#' obtain_r_square(example_model)
obtain_r_square <- function(model){
  # Obtain y_hat
  y_hat <- model$fitted.values

  # Obtain observed value of y
  y <- y_hat + model$residuals

  # Obtain mean of observed y
  y_bar <- mean(y)

  # Obtain SSY and SSE
  SSY <- sum((y - y_bar) ^ 2)
  SSE <- obtain_SSE(model)

  # Calculate R by 1 - SSE/SSY
  r_square <- 1 - SSE / SSY

  # Obtain n and degrees of freedom of residuals
  n <- length(model$residuals)
  df_resi <- df.residual(model)

  # Calculate adjusted R square
  adj_r_square <- 1 - (SSE / df_resi) / (SSY / (n - 1))

  return(list(r_square = r_square, adj_r_square = adj_r_square))
}


#' obtain_F_stats
#'
#' @description
#' This function gives F statistic, numerator degrees of freedom, residuals degrees of freedom, and the corresponding p-value of the linear model.
#'
#' @param model linear model object
#' @return F statistic, numerator degrees of freedom, residuals degrees of freedom, and the corresponding p-value.
#' @export
#' @examples
#' data(mtcars)
#' example_model <- lm(mpg ~ hp + wt, data = mtcars)
#'
#' #obtain F statistic and its p-value
#' obtain_f_stats(example_model)
obtain_f_stats <- function(model){
  # Calculate SSR and SSE
  y_hat <- model$fitted.values
  y_bar <- mean(y_hat + model$residuals)
  SSR <- sum((y_hat - y_bar) ^ 2)
  SSE <- obtain_SSE(model)

  # Obtain degrees of freedom of residuals and p
  df_resi <- df.residual(model)
  p <- length(coef(model))

  # Calculate mean square regression (MSR) and mean square error (MSE)
  MSR <- SSR / (p - 1)
  MSE <- SSE / (df_resi)

  # Calculate F statistic
  F_stats <- MSR / MSE

  # Calculate p-value
  p_value <- pf(F_stats, (p - 1), df_resi, lower.tail = FALSE)

  formatted_p_values <- format(p_value, digits = 4, scientific = TRUE)

  return(list(f_statistic = F_stats, df_model = p - 1, df_resi = df_resi, p_value = formatted_p_values))
}



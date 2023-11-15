#' obtain_SSE
#'
#' @param model linear model object
#' @return SSE in the linear model
#' @export
#'
#' @examples
obtain_SSE <- function(model){
  #obtain residuals
  residual <- model$residuals
  #calculate SSE
  sse <- sum(residual ^ 2)
  return(sse)
}

#' obtain_se
#'
#' @param model linear model object
#' @return coefficients standard errors of the linear model
#' @export
#'
#' @examples

obtain_se <- function(model){
  #get the variance-covariance matrix
  mtrx <- vcov(model)
  #calculate standard error
  se <- sqrt(diag(mtrx))
  return(se)
}

#' obtain_t_stats
#'
#' @param model linear model object
#' @return t statistics of the linear model
#' @export
#'
#' @examples

obtain_t_stats <- function(model){
  t_stats <- coef(model) / obtain_se(model)
  return(t_stats)
}

#' obtain_p_value
#'
#' @param model linear model object
#' @return p values of the linear model
#' @export
#'
#' @examples

obtain_p_value <- function(model){
  #obtain the degrees of freedom associated with the residuals
  df_resi <- df.residual(model)
  #calculate p-values
  p_values <- 2 * (1 - pt(abs(obtain_t_stats(model)), df_resi))
  return(p_values)
}

#' obtain_residual_info
#'
#' @param model linear model object
#' @return residual standard error and degrees of freedom of the linear model
#' @export
#'
#' @examples
obtain_residual_info <- function(model){
  #calculate residuals df
  df_resi <- df.residual(model)

  #calculate residual standard error
  se <- sqrt(obtain_SSE(model) / df_resi)

  return(list(residual_se = se, df_resi = df_resi))
}

#' obtain_r_square
#'
#' @param model linear model object
#' @return R square and adjusted R square of the linear model
#' @export
#'
#' @examples
#'
obtain_r_square <- function(model){
  #obtain y_hat
  y_hat <- model$fitted.values

  #obtain observed value of y
  y <- y_hat + obtain_residuals(model)

  #obtain mean of observed y
  y_bar <- mean(y)

  #obtain SSY and SSE
  SSY <- sum((y - y_bar) ^ 2)
  SSE <- obtain_SSE(model)

  #calculate R by 1 - SSE/SSY
  r_square <- 1 - SSE / SSY

  #obtain n and degrees of freedom of residuals
  n <- length(model$residuals)
  df_resi <- df.residual(model)

  #calculate adjusted R square
  adj_r_square <- 1 - (SSE / df_resi) / (SSY / (n - 1))

  return(list(r_square = r_square, adj_r_square = adj_r_square))
}


#' obtain_F_stats
#'
#' @param model linear model object
#' @return F statistic and its p-value of the linear model
#' @export
#'
#' @examples
#'

obtain_f_stats <- function(model){
  #calculate SSR and SSE
  SSR <- sum(model$residuals ^ 2)
  SSE <- obtain_SSE(model)

  #obtain degrees of freedom of residuals and p
  df_resi <- df.residual(model)
  p <- length(coef(model))

  #calculate F statistic
  f_stats <- (SSR / (p-1)) / (SSE / df_resi)

  #calculate p-value
  p_value <- pf(f_stats, (p - 1), df_resi, lower.tail = FALSE)
  return(list(f_statistic = f_stats, df_model = p - 1, df_resi = df_resi, p_value = p_value))
}


## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(HW625)
data(mtcars)
# For efficiency comparing
library(bench)

## -----------------------------------------------------------------------------
# Fit a linear model
model <- lm(mpg ~ wt + hp + qsec + am, data = mtcars)

# Original `summary()` function
model_summary <- summary(model)

## -----------------------------------------------------------------------------
SSE <- obtain_SSE(model)
print(SSE)

# Compare the result with the original function
check_sse <- sum(model_summary$residuals ^ 2)
all.equal(check_sse, SSE)

## -----------------------------------------------------------------------------
results <- bench::mark(
  SSE,
  check_sse,
  check = FALSE
)
print(results)

## -----------------------------------------------------------------------------
SE <- obtain_se(model)
print(SE)

# Compare the result with the original function
check_se <- model_summary$coefficients[, "Std. Error"]
all.equal(check_se, SE)

## -----------------------------------------------------------------------------
results <- bench::mark(
  SE,
  check_se,
  check = FALSE
)
print(results)

## -----------------------------------------------------------------------------
t_stats <- obtain_t_stats(model)
print(t_stats)

# Compare the result with the original function
check_t <- model_summary$coefficients[, "t value"]
all.equal(check_t, t_stats)

## -----------------------------------------------------------------------------
results <- bench::mark(
  t_stats,
  check_t,
  check = FALSE
)
print(results)

## -----------------------------------------------------------------------------
p_values <- obtain_p_value(model)
print(p_values)

# Compare the result with the original function
check_p <- model_summary$coefficients[, "Pr(>|t|)"]
all.equal(check_p, p_values)

## -----------------------------------------------------------------------------
results <- bench::mark(
  p_values,
  check_p,
  check = FALSE
)
print(results)

## -----------------------------------------------------------------------------
resi_se <- obtain_residual_info(model)
print(resi_se)

# Compare the residual standard error with the original function
check_resi_se <- model_summary$sigma
all.equal(resi_se$residual_se, check_resi_se)

# Compare the degrees of freedom with the original function
check_resi_df <- model_summary$df[2]
all.equal(resi_se$df_resi, check_resi_df)

## -----------------------------------------------------------------------------
results1 <- bench::mark(
  resi_se$residual_se,
  check_resi_se,
  check = FALSE
)
print(results1)

results2 <- bench::mark(
  resi_se$df_resi,
  check_resi_df,
  check = FALSE
)
print(results2)

## -----------------------------------------------------------------------------
R_sq <- obtain_r_square(model)
print(R_sq)

# Compare R-square with the original function
check_R_sq <- model_summary$r.squared
all.equal(R_sq$r_square, check_R_sq)

# Compare adjusted R-square with the original function
check_adj_R_sq <- model_summary$adj.r.squared
all.equal(R_sq$adj_r_square, check_adj_R_sq)

## -----------------------------------------------------------------------------
results1 <- bench::mark(
  R_sq$r_square,
  check_R_sq,
  check = FALSE
)
print(results1)

results2 <- bench::mark(
  R_sq$adj_r_square,
  check_adj_R_sq,
  check = FALSE
)
print(results2)

## -----------------------------------------------------------------------------
f_stats <- obtain_f_stats(model)
print(f_stats)

# Compare F statistic with the original function
check_F <- model_summary$fstatistic[1]
all.equal(f_stats$f_statistic, check_F, check.attributes = FALSE)

# Compare numerator degrees of freedom with the original function
check_df <- model_summary$fstatistic[2]
all.equal(f_stats$df_model, check_df, check.attributes = FALSE)

## -----------------------------------------------------------------------------
results1 <- bench::mark(
  f_stats$f_statistic,
  check_F,
  check = FALSE
)
print(results1)

results2 <- bench::mark(
  f_stats$df_model,
  check_df,
  check = FALSE
)
print(results2)

## -----------------------------------------------------------------------------
# Apply lm_summary at confidence level 0.95
smr <- lm_summary(model)
cat(smr)

## -----------------------------------------------------------------------------
model_summary

## -----------------------------------------------------------------------------
results <- bench::mark(
  lm_summary(model),
  model_summary,
  check = FALSE
)
print(results)


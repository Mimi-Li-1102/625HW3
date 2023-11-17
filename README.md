# HW625

<!-- badges: start -->

[![R-CMD-check](https://github.com/Mimi-Li-1102/HW625/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Mimi-Li-1102/HW625/actions/workflows/R-CMD-check.yaml) [![Codecov test coverage](https://codecov.io/gh/Mimi-Li-1102/HW625/branch/main/graph/badge.svg)](https://app.codecov.io/gh/Mimi-Li-1102/HW625?branch=main)

<!-- badges: end -->

## Overview

This package provides information specific to linear models. Its functions are similar to \`summary\`, but there are additional information, such as the confidence intervals.

-   `lm_summary` gives a summary of information for the linear model, including coefficients, residuals, and additional model information. The output is similar to \`summary\`.

-   `obtain_SSE` gives the SSE of the linear model.

-   `obtain_se` gives the standard errors for all coefficients of the linear model.

-   `obtain_t_stats` gives the t-statistics for all coefficients of the linear model.

-   `obtain_p_value` gives p-values corresponding to each t-statistic.

-   `obtain_residual_info` gives residuals standard error and degrees of freedom of the linear model

-   `obtain_r_square` gives R square and adjusted R square of the linear model

-   `obtain_F_stats` gives F-statistic and its corresponding p-value of the linear model.

## Installation

```{r}
remotes::install_github("https://github.com/Mimi-Li-1102/HW625.git")
```

## Usage

```{r}
library(HW625)
data(mtcars)

# Fit a linear model
model <- lm(mpg ~ wt + hp + qsec + am, data = mtcars)

# Find SSE of the linear model
SSE <- obtain_SSE(model)
print(SSE)

# Find the standard errors for each coefficient in the linear model
SE <- obtain_se(model)
print(SE)

# Find the t-statistics for each coefficient in the linear model
t_stats <- obtain_t_stats(model)
print(t_stats)

# Find the p-values corresponding to the t-statistics for each coefficient in the linear model
p_values <- obtain_p_value(model)
print(p_values)

# Find the standard error and degrees of freedom for the residuals in the linear model
resi_se <- obtain_residual_info(model)
print(resi_se)

# Find R-square and adjusted R-square
R_sq <- obtain_r_square(model)
print(R_sq)

# Find F statistic, numerator degrees of freedom, residuals degrees of freedom, and the corresponding p-value of the linear model
f_stats <- obtain_f_stats(model)
print(f_stats)

# Print the summary of the model
lm_summary(model)
```

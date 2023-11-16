# HW625

<!-- badges: start -->

[![R-CMD-check](https://github.com/Mimi-Li-1102/HW625/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Mimi-Li-1102/HW625/actions/workflows/R-CMD-check.yaml) [![Codecov test coverage](https://codecov.io/gh/Mimi-Li-1102/HW625/branch/main/graph/badge.svg)](https://app.codecov.io/gh/Mimi-Li-1102/HW625?branch=main)

<!-- badges: end -->

## Overview

This package provides information specific to linear models. Its functions are similar to \`summary\`, but the information that can be extracted is slightly different.

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
install.packages("HW625")
```

## Usage

For detailed examples and use cases, please refer to the vignettes.

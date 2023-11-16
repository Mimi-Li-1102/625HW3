data(mtcars)
lm <- lm(mpg ~ wt + hp + qsec, data = mtcars)
sm <- summary(lm)
test_summary <- lm_summary(lm)

# Test estimates in `lm_summary`
test_that("lm_summary_estimates1", {
  expect_equal(test_summary$Estimate[1], sm$coefficients[1, 1])
})

test_that("lm_summary_estimates2", {
  expect_equal(test_summary$Estimate[2], sm$coefficients[2, 1])
})

# Test `residual_se`
test_that("residual_se", {
  expect_equal(obtain_residual_info(lm)[1]$residual_se, sm$sigma)
})


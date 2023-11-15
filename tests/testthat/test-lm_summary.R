data(mtcars)
lm <- lm(mpg ~ wt + hp + qsec, data = mtcars)
sm <- summary(lm)
test_summary <- lm_summary(lm)
test_that("lm_summary_estimates", {
  expect_equal(test_summary$Estimate[1], sm$coefficients[1, 1])
})

data(mtcars)
lm <- lm(mpg ~ wt + hp + qsec, data = mtcars)
sm <- summary(lm)

# Test `residual_se`
test_that("residual_se", {
  expect_equal(obtain_residual_info(lm)[1]$residual_se, sm$sigma)
})


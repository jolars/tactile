library(tactile)

test_that("xyplot.Arima", {
  fit <- arima(lh, c(0, 0, 1))
  expect_error(xyplot(fit, which = 1:5))
  expect_error(xyplot(fit, main = letters[1:5]))
})

test_that("xyplot.forecast", {
  fit <- forecast::forecast(arima(lh, c(0, 0, 1)))
  expect_error(fit, NA)
})


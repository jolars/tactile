library(latticework)

test_that("We receive the proper classes", {
  fit <- stats::lm(rnorm(100) ~ runif(100))
  expect_error(xyplot(fit), NA)
  obj <- xyplot(fit, which = 1:6)
  obj2 <- xyplot(fit, which = 1)

  expect_is(obj, "grob")
  expect_is(obj2, "trellis")
})

test_that("xyplot.lm throws errors with incorrect input", {
  fit <- stats::lm(rnorm(100) ~ runif(100))
  expect_error(xyplot(fit, which = "something"))
})



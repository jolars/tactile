library(latticework)

test_that("We return a list of grobs", {
  fit <- stats::lm(rnorm(100) ~ runif(100))
  obj <- lattice::xyplot(fit)
  expect_is(obj, "grob")
})

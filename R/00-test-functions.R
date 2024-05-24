
library("here")
source(here("R/functions.R"))
library("testthat")

# tests -------------------------------------------------------------------

test_that("modify_stan_template() adds the correct number of rows", {
  expect_equal(length(modify_stan_template(2)), length(modify_stan_template(0)) + 14)
})

test_that("build_standatalist() builds the expected list", {
  x <- data.frame(x = rnorm(10), y = rnorm(10), z = rnorm(10))
  cens <- as.data.frame(apply(x, 2, \(x) x < 0))
  standata <- build_standatalist(x, 1, cens)
  expect_equal(standata$N, nrow(x))
  expect_equal(standata$D, ncol(x))
  expect_equal(standata$M, 1)
  expect_equal(standata$y, x)
  expect_equal(standata$Ncens_y1, sum(x$x < 0))
  expect_equal(standata$Ncens_y2, sum(x$y < 0))
  expect_equal(standata$Jcens_y1, which(x$x < 0))
  expect_equal(standata$Jcens_y2, which(x$y < 0))
  expect_equal(standata$U_y1, x$x[x$x < 0])
  expect_equal(standata$U_y2, x$y[x$y < 0])
})

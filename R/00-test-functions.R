
# notes -------------------------------------------------------------------

# this script contains a few tests for the functions used to build the PPCA stan program and
# input

# setup -------------------------------------------------------------------

library("here")
source(here("R/functions-ppca.R"))
library("testthat")

# tests -------------------------------------------------------------------

test_that("modify_stan_template() adds the correct number of rows", {
  x <- data.frame(x = rnorm(10), y = rnorm(10))
  cens <- as.data.frame(apply(x, 2, \(x) x < 0))
  standata <- build_standatalist(x, 1, cens)
  expect_equal(length(modify_stan_template(standata)), length(modify_stan_template(list())) + 14)
  # non-empty input but no censoring:
  x <- data.frame(x = rlnorm(10), y = rlnorm(10))
  cens <- as.data.frame(apply(x, 2, \(x) x < 0))
  standata <- build_standatalist(x, 1, cens)
  expect_equal(modify_stan_template(standata), modify_stan_template(list()))
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

test_that("build_standatalist() builds the expected list when there is no censoring", {
  x <- data.frame(x = rlnorm(10), y = rnorm(10), z = rlnorm(10))
  cens <- as.data.frame(apply(x, 2, \(x) x < 0))
  standata <- build_standatalist(x, 1, cens)
  expect_null(standata$Ncens_y1)
  expect_null(standata$Ncens_y3)
  expect_null(standata$Jcens_y1)
  expect_null(standata$Jcens_y3)
  expect_null(standata$U_y1)
  expect_null(standata$U_y3)
})

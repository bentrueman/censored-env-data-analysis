
# notes -------------------------------------------------------------------

# ppca model diagnosed at the time of fitting using cmdstan_diagnose()

# setup -------------------------------------------------------------------

library("here")

source(here("R/03-censored-response.R"))
source(here("R/04-censored-predictor.R"))
source(here("R/05-correlation-matrix.R"))
source(here("R/06-simulation.R"))

library("testthat")
library("rstan")

# functions ---------------------------------------------------------------

model_list <- list(
  "censored response" = model_censored_response,
  "censored predictor" = model_censored_predictor,
  "correlation matrix" = model_correlation_matrix
) |>
  c(simulation$model_censoring) |>
  c(simulation$model_naive)

sampler_params <- model_list |>
  map(
    # warmup draws should all be excluded b/c they aren't saved in the CSV files
    ~ get_sampler_params(.x$fit, inc_warmup = FALSE) |>
      map(as_tibble) |>
      bind_rows(.id = ".chain") |>
      mutate(max_treedepth = .x$fit@stan_args[[4]]$max_depth)
  ) |>
  list_rbind(names_to = "model")

draw_summary <- model_list |>
  map(
    ~ as_draws(.x)  |>
      posterior::summarise_draws()
  ) |>
  list_rbind(names_to = "model")

test_that("max_treedepth never exceeded", {
  exceedances <- sampler_params |>
    filter(treedepth__ >= max_treedepth)
  n <- nrow(exceedances)
  expect_equal(n, 0)
})

test_that("no divergent transitions", {
  divergences <- sampler_params |>
    filter(divergent__ != 0)
  n <- nrow(divergences)
  expect_equal(n, 0)
})

test_that("no rhats greater than 1.05", {
  hi_rhat <- draw_summary |>
    filter(rhat >= 1.05)
  n <- nrow(hi_rhat)
  expect_equal(n, 0)
})

test_that("no low ESS (bulk)", {
  lo_ess <- draw_summary |>
    filter(ess_bulk < 400)
  n <- nrow(lo_ess)
  expect_equal(n, 0)
})

test_that("no low ESS (tail)", {
  lo_ess <- draw_summary |>
    filter(ess_tail < 400)
  n <- nrow(lo_ess)
  expect_equal(n, 0)
})

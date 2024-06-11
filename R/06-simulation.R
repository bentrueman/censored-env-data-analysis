
# notes -------------------------------------------------------------------

# this script simulates linear regression with a censored response, as described in the text

# create models directory -------------------------------------------------

if (!file.exists("models")) dir.create("models")
if (!file.exists("models/simulation")) dir.create("models/simulation")

# setup -------------------------------------------------------------------

library("brms")
library("tibble")
library("dplyr")
library("purrr")
library("tidyr")
library("withr")

options(cmdstanr_write_stan_file_dir = "models/simulation")

# simulation parameters ---------------------------------------------------

n_simulations <- 100
n_observations <- 25
sigma <- .5 # residual standard deviation
beta0 <- 3 # intercept
beta1 <- .15 # slope
left_censoring_limit <- 4.5 # determines the censoring rate

# put this into a tibble:

true_values <- tibble(
  b_Intercept = beta0,
  b_x = beta1,
  sigma = sigma
) |>
  pivot_longer(everything(), names_to = "param")

# simulation --------------------------------------------------------------

datasets <- with_seed(15928, {
  replicate(
    n_simulations,
    {
      tibble(
        x = seq_len(n_observations),
        y = beta0 + beta1 * x + rnorm(n_observations, 0, sigma)
      ) |>
        mutate(
          y_observed = pmax(y, left_censoring_limit), # censor y values
          censored = if_else(y < left_censoring_limit, "left", "none")
        )
    },
    simplify = FALSE
  )
})

# compile models ----------------------------------------------------------

# n.b., models have to be recompiled to run this on a new machine

priors <- c(
  prior(student_t(3, 0, 2.5), class = b),
  prior(student_t(3, 0, 2.5), class = Intercept),
  prior(student_t(3, 0, 2.5), class = sigma)
)

model_censoring <- brm(
  y_observed | cens(censored) ~ x,
  data = datasets[[1]],
  chains = 0,
  prior = priors,
  backend = "cmdstanr",
  file = "models/simulation/simulation-censoring-compiled"
)

model_naive <- brm(
  y_observed ~ x,
  data = datasets[[1]] |>
    # substitute half the detection limit:
    mutate(y_observed = if_else(censored == "left", 0.5 * y_observed, y_observed)),
  chains = 0,
  prior = priors,
  backend = "cmdstanr",
  file = "models/simulation/simulation-naive-compiled"
)

# run simulation ----------------------------------------------------------

simulation <- tibble(
  simulation = seq(n_simulations),
  data = datasets
) |>
  mutate(
    # model with censoring:
    model_censoring = map2(data, simulation, ~ update(
      model_censoring,
      newdata = .x,
      cores = parallel::detectCores(),
      file = paste0("models/simulation/simulation-censoring", .y)
    )),
    params_censoring = map(model_censoring, ~ as_draws_df(.x)),
    params_censoring = map(params_censoring, ~ suppressWarnings(select(.x, b_Intercept, b_x, sigma))),
    params_censoring = map(params_censoring, ~ pivot_longer(.x, everything(), names_to = "param", values_to = "value_censoring")),
    # model without censoring:
    data_substituted = map(data, ~ mutate(.x, y_observed = if_else(censored == "left", 0.5 * y_observed, y_observed))),
    model_naive = map2(data_substituted, simulation, ~ update(
      model_naive,
      newdata = .x,
      cores = parallel::detectCores(),
      file = paste0("models/simulation/simulation-naive", .y)
    )),
    params_naive = map(model_naive, ~ as_draws_df(.x)),
    params_naive = map(params_naive, ~ suppressWarnings(select(.x, b_Intercept, b_x, sigma))),
    params_naive = map(params_naive, ~ pivot_longer(.x, everything())),
    params_naive = map(params_naive, ~ select(.x, value_naive = value))
  )

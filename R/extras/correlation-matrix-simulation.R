
# notes -------------------------------------------------------------------

# this simulation tests the correlation matrix model, ensuring that it returns
# reasonable estimates of the true correlation coefficients even under heavy missingness
# and left-censoring

# create models directory -------------------------------------------------

if (!file.exists("models")) dir.create("models")
if (!file.exists("models/simulation")) dir.create("models/simulation")

# setup -------------------------------------------------------------------

library("MASS") # random generation for multivariate normal
library("tidyverse") # data wrangling
library("withr") # set random seed
library("trialr") # generate random correlation matrices
library("brms") # generate Stan code template
library("bgamcar1") # customize Stan code and fit models
library("cmdstanr") # postprocess model
library("posterior") # postprocess model
library("ggplot2") # visualize
library("patchwork") # visualize

theme_set(theme_bw())

options(mc.cores = parallel::detectCores())

# simulation inputs -------------------------------------------------------

filename_prefix <- "simulation-correlation-matrix-" # for saving cmdstan fits

n_variables <- 5

n_observations <- 100

n_simulations <- 25

proportion_missing <- 0.2

n_missing <- round(proportion_missing * n_observations)

# this is the proportion of sigma to subtract from the mean, yielding the censoring threshold:

proportion_censored <- 0.25

# parameters for lognormal distributions of mu and sigma:

mean_mu <- 0

sd_mu <- 1

meanlog_sigma <- 1

sdlog_sigma <- 1

# make variable/indicator names:

these_variables <- paste0("x", seq(n_variables))

these_indicators <- paste0("cens_x", seq(n_variables))

# simulate data -----------------------------------------------------------

this_seed <- 124256764

with_seed(this_seed, {

  simulation_inputs <- replicate(n_simulations, {
    tibble(
      variable = these_variables,
      mu = rnorm(n_variables, mean_mu, sd_mu),
      sigma = rlnorm(n_variables, meanlog_sigma, sdlog_sigma),
      censoring_thresholds = mu - proportion_censored * sigma
    )
  }, simplify = FALSE)

  correlation_matrices <- replicate(n_simulations, {
    trialr::rlkjcorr(1, n_variables)
  }, simplify = FALSE)

  data <- map2(correlation_matrices, simulation_inputs, \(x, y) {

    sigma <- diag(y$sigma)

    covariance_matrix <- sigma %*% x %*% sigma

    data <- mvrnorm(n = n_observations, mu = y$mu, Sigma = covariance_matrix)

    # add in missings and censored:

    data_missing <- data |>
      apply(2, \(u) {u[sample(seq(n_observations), n_missing)] <- NA; u})


    censoring_indicators <- data_missing |>
      sweep(2, y$censoring_thresholds, FUN = "<") |>
      apply(2, \(u) if_else(u, "left", "none")) |>
      apply(2, \(u) replace_na(u, "none"))

    data_censored <- data_missing |>
      sweep(2, y$censoring_thresholds, FUN = pmax)

    # convert to tibble:

    colnames(data_censored) <- these_variables

    colnames(censoring_indicators) <- these_indicators

    bind_cols(data.frame(data_censored), data.frame(censoring_indicators)) |>
      as_tibble()

  })
})

# calculate percent censoring:

data |>
  list_rbind(names_to = "simulation") |>
  summarize(across(starts_with("cens_"), ~ mean(.x == "left")))

# generate model inputs ---------------------------------------------------

these_formulas <- lapply(these_variables, \(x) bf(paste0(x, " | mi() ~ 1")))

multivariate_formula <- mvbrmsformula(flist = these_formulas) + set_rescor(TRUE)

# fit models --------------------------------------------------------------

# load from CSVs:

get_models <- function(...) {

  filenames <- map_chr(seq(n_simulations), ~ paste0("models/simulation/", filename_prefix, .x)) |>
    map(~ paste0(.x, "-", 1:4, ".csv"))

  try(map(filenames, as_cmdstan_fit))

}

censored_model_fitted <- get_models()

# if they don't exist, sample:

if (class(censored_model_fitted) == "try-error") {

  map(seq_along(data), ~ fit_stan_model(
    file = paste0("models/simulation/", filename_prefix, .x),
    seed = this_seed,
    bform = multivariate_formula,
    bdata = data[[.x]],
    car1 = FALSE,
    var_xcens = these_variables,
    cens_ind = these_indicators,
    lcl = simulation_inputs[[.x]]$censoring_thresholds,
    family = "gaussian",
    backend = "cmdstanr"
  ))

  censored_model_fitted <- get_models()

}

# identify models that didn't converge ------------------------------------

diagnostics_1 <- map(censored_model_fitted, ~ .x$diagnostic_summary()) |>
  map_lgl(
    ~ all(.x$num_divergent == 0) & all(.x$num_max_treedepth == 0) & all(.x$ebfmi >= 0.3)
  )

diagnostics_2 <- map(censored_model_fitted, ~ .x$summary()) |>
  map_lgl(
    ~ with(.x,
           all(rhat < 1.05, na.rm = TRUE) &
             all(ess_bulk >= 400, na.rm = TRUE) &
             all(ess_tail >= 400, na.rm = TRUE))
  )

diagnostics <- tibble(simulation = seq(n_simulations), converged = diagnostics_1 & diagnostics_2)

# get true parameter values -----------------------------------------------

these_variable_pairs <- data.frame(t(combn(these_variables, 2))) |>
  unite(col = variable, X1, X2)

correlation_matrices_tbl <- correlation_matrices |>
  map(\(x) x[lower.tri(x)]) |>
  map(\(x) mutate(these_variable_pairs, rho = x)) |>
  list_rbind(names_to = "simulation")

simulation_inputs_tbl <- simulation_inputs |>
  list_rbind(names_to = "simulation")

# compare estimates with true values --------------------------------------

model_draws <- censored_model_fitted |>
  map(~ .x$draws(format = "draws_df")) |>
  map(as_tibble) |>
  list_rbind(names_to = "simulation") |>
  # rename correlation parameters:
  rename_with(
    .cols = matches("^Rescor\\[\\d\\,\\d\\]"),
    .fn = ~ str_replace(.x, "(\\d+),(\\d+)", "x\\1,x\\2")
  )

estimates <- list(
  mu_estimated = "Intercept$", sigma_estimated = "^sigma", rho_estimated = "^Rescor\\[x\\d,x\\d\\]"
) |>
  map2(
    list(simulation_inputs_tbl, simulation_inputs_tbl, correlation_matrices_tbl),
    ~ model_draws |>
      select(c(simulation, matches(.x))) |>
      pivot_longer(-simulation, names_to = "param") |>
      mutate(
        variable = str_extract_all(param, paste(these_variables, collapse = "|")),
        variable = map(variable, ~ paste(.x, collapse = "_")),
        variable = unlist(variable)
      ) |>
      group_by(variable, simulation) |>
      summarize(
        lower = quantile(value, .025),
        upper = quantile(value, .975),
        estimate = quantile(value, .5)
      ) |>
      ungroup() |>
      right_join(.y, by = c("simulation", "variable")) |>
      left_join(diagnostics, by = "simulation")
  )

# plot estimates ----------------------------------------------------------

plot_estimates <- \(x, param) {
  x |>
    mutate(
      ci_contains_true_value = upper - .data[[param]] > 0 & lower - .data[[param]] < 0,
      across(c(converged, ci_contains_true_value), ~ if_else(.x, "Yes", "No"))
    ) |>
    ggplot(aes(.data[[param]], estimate, col = ci_contains_true_value, shape = converged)) +
    geom_abline() +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0, linewidth = 0.1) +
    geom_point(alpha = 0.5)
}

these_plots <- map2(
  estimates,
  list("mu", "sigma", "rho"),
  plot_estimates
)

# combine -----------------------------------------------------------------

wrap_plots(
  these_plots$mu_estimated + labs(title = "Means"),
  these_plots$sigma_estimated +
    scale_x_log10() +
    scale_y_log10() +
    labs(title = "Standard\ndeviations"),
  these_plots$rho_estimated + labs(title = "Correlation\ncoefficients"),
  ncol = 3
) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom", legend.direction = "vertical") &
  labs(
    x = "True value", y = "Estimate",
    shape = "Did the model converge?",
    col = "Does the CI contain the true value?"
  )


# notes -------------------------------------------------------------------

# this simulation tests the correlation matrix model, ensuring that it returns
# reasonable estimates of the true correlation coefficients even under heavy missingness
# and left-censoring

# setup -------------------------------------------------------------------

library("MASS") # random generation for multivariate normal
library("tidyverse") # data wrangling
library("withr") # set random seed
library("trialr") # generate random correlation matrices
library("brms") # generate Stan code
library("bgamcar1") # customize Stan code
library("cmdstanr") # run Stan code
library("posterior") # postprocess model
library("ggplot2") # visualize
library("patchwork") # visualize

theme_set(theme_bw())

options(mc.cores = parallel::detectCores())

# simulation inputs -------------------------------------------------------

filename_prefix <- "correlation-matrix-simulation-" # for saving cmdstan fits

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

with_seed(124256764, {

  simulation_inputs <- tibble(
    variable = these_variables,
    mu = rnorm(n_variables, mean_mu, sd_mu),
    sigma = rlnorm(n_variables, meanlog_sigma, sdlog_sigma),
    censoring_thresholds = mu - proportion_censored * sigma
  )

  correlation_matrices <- replicate(n_simulations, {
    trialr::rlkjcorr(1, n_variables)
  }, simplify = FALSE)

  data <- map(correlation_matrices, \(x) {

    sigma <- diag(simulation_inputs$sigma)

    covariance_matrix <- sigma %*% x %*% sigma

    data <- mvrnorm(n = n_observations, mu = simulation_inputs$mu, Sigma = covariance_matrix)

    # add in missings and censored:

    data_missing <- data |>
      apply(2, \(u) {u[sample(seq(n_observations), n_missing)] <- NA; u})


    censoring_indicators <- data_missing |>
      sweep(2, simulation_inputs$censoring_thresholds, FUN = "<") |>
      apply(2, \(u) if_else(u, "left", "none")) |>
      apply(2, \(u) replace_na(u, "none"))

    data_censored <- data_missing |>
      sweep(2, simulation_inputs$censoring_thresholds, FUN = pmax)

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

these_priors <- c(
  set_prior("student_t(3, 0, 2.5)", class = "Intercept", resp = these_variables),
  set_prior("student_t(3, 0, 2.5)", class = "sigma", resp = these_variables, lb = 0),
  set_prior("lkj(1)", class = "rescor")
)

# generate model code -----------------------------------------------------

multivariate_formula <- mvbrmsformula(flist = these_formulas) + set_rescor(TRUE)

generated_stancode <- make_stancode(
  multivariate_formula,
  prior = these_priors,
  data = data[[1]],
  family = "gaussian"
) |>
  modify_stancode(
    modify = "xcens",
    var_xcens = these_variables,
    lcl = simulation_inputs$censoring_thresholds
  )

# generate standata -------------------------------------------------------

generated_standata <- map(data, ~ make_standata(
  multivariate_formula,
  data = .x,
  prior = these_priors,
  family = "gaussian"
)) |>
  map2(data, ~ modify_standata(
    .x,
    .y,
    lcl = simulation_inputs$censoring_thresholds,
    var_xcens = these_variables,
    cens_ind = these_indicators
  ))

# fit model ---------------------------------------------------------------

# load from CSVs:

filenames <- map_chr(seq(n_simulations), ~ paste0("models/", filename_prefix, .x)) |>
  map(~ paste0(.x, "-", 1:4, ".csv"))

censored_model_fitted <- try(map(filenames, as_cmdstan_fit))

# sample:

if (class(censored_model_fitted) == "try-error") {

  censored_model_compiled <- cmdstan_model(stan_file = write_stan_file(generated_stancode))

  censored_model_fitted <- map(
    generated_standata, ~ censored_model_compiled$sample(data = .x)
  )

  map2(
    censored_model_fitted, seq_along(censored_model_fitted),
    ~ .x$save_output_files(
      dir = "models",
      basename = paste0(filename_prefix, .y),
      random = FALSE,
      timestamp = FALSE
    )
  )

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

# compare estimates with true values --------------------------------------

these_variable_pairs <- data.frame(t(combn(these_variables, 2))) |>
  rename(variable_1 = X1, variable_2 = X2)

correlation_matrices_tbl <- correlation_matrices |>
  map(\(x) x[lower.tri(x)]) |>
  map(\(x) mutate(these_variable_pairs, rho = x)) |>
  list_rbind(names_to = "simulation")

model_draws <- censored_model_fitted |>
  map(~ .x$draws(format = "draws_df")) |>
  map(as_tibble) |>
  list_rbind(names_to = "simulation")

estimates <- list(mu_estimated = "Intercept$", sigma_estimated = "^sigma") |>
  map(
    ~ model_draws |>
      select(c(simulation, matches(.x))) |>
      pivot_longer(-simulation, names_to = "param") |>
      group_by(variable = str_extract(param, paste(these_variables, collapse = "|")), simulation) |>
      summarize(
        lower = quantile(value, .025),
        upper = quantile(value, .975),
        estimate = quantile(value, .5)
      ) |>
      ungroup() |>
      left_join(simulation_inputs, by = "variable") |>
      left_join(diagnostics, by = "simulation")
  )

rho_estimated <- model_draws |>
  select(c(simulation, matches("^Rescor\\[\\d\\,\\d\\]"))) |>
  pivot_longer(-simulation, names_to = "param") |>
  group_by(
    param,
    variable_1 = paste0("x", str_extract(param, "(?<=\\[)\\d")),
    variable_2 = paste0("x", str_extract(param, "(?<=\\,)\\d")),
    simulation
  ) |>
  summarize(
    lower = quantile(value, .025),
    upper = quantile(value, .975),
    estimate = quantile(value, .5)
  ) |>
  filter(variable_1 < variable_2) |>
  arrange(simulation) |>
  left_join(correlation_matrices_tbl, by = c("variable_1", "variable_2", "simulation")) |>
  left_join(diagnostics, by = "simulation")

# plot estimates ----------------------------------------------------------

plot_mu <- estimates$mu_estimated |>
  mutate(
    ci_contains_true_value = upper - mu > 0 & lower - mu < 0,
    across(c(converged, ci_contains_true_value), ~ if_else(.x, "Yes", "No"))
  ) |>
  ggplot(aes(mu, estimate, col = ci_contains_true_value, shape = converged)) +
  geom_abline() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0, linewidth = 0.1) +
  geom_point() +
  labs(title = "Means")

plot_sigma <- estimates$sigma_estimated |>
  mutate(
    ci_contains_true_value = upper - sigma > 0 & lower - sigma < 0,
    across(c(converged, ci_contains_true_value), ~ if_else(.x, "Yes", "No"))
  ) |>
  ggplot(aes(sigma, estimate, col = ci_contains_true_value, shape = converged)) +
  scale_x_log10() +
  scale_y_log10() +
  geom_abline() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0, linewidth = 0.1) +
  geom_point() +
  labs(title = "Standard\ndeviations")

plot_correlations <- rho_estimated |>
  mutate(
    ci_contains_true_value = upper - rho > 0 & lower - rho < 0,
    across(c(converged, ci_contains_true_value), ~ if_else(.x, "Yes", "No"))
  ) |>
  ggplot(aes(rho, estimate, col = ci_contains_true_value, shape = converged)) +
  geom_abline() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0, linewidth = 0.1) +
  geom_point() +
  labs(title = "Correlation\ncoefficients")

# combine -----------------------------------------------------------------

wrap_plots(plot_mu, plot_sigma, plot_correlations, ncol = 3) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom", legend.direction = "vertical") &
  labs(
    shape = "Did the model converge?",
    col = "Does the CI contain the true value?"
  )

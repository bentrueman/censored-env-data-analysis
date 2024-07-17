
# notes -------------------------------------------------------------------

# this script estimates the pairwise correlations among the metals concentrations on a different dataset

# create models directory -------------------------------------------------

if (!file.exists("models")) dir.create("models")

# setup -------------------------------------------------------------------

library("here")
source(here("R/extras/01-prepare-data-al-i.R"))
library("stringr")
library("purrr")
library("brms")
library("bgamcar1")
library("cmdstanr")

options(mc.cores = parallel::detectCores())

# identify variables with no zero lower bound -----------------------------

no_lower_bound <- apply(pca_in, 2, \(x) min(x, na.rm = TRUE) < 0)

no_lower_bound <- names(no_lower_bound)[no_lower_bound]

# remove variables with > 30% censored or missing values ------------------

these_columns <- apply(pca_censoring, 2, mean) <= 0.3 &
  apply(pca_in, 2, \(x) mean(is.na(x))) <= 0.3

pca_in <- pca_in[, these_columns]

pca_censoring <- pca_censoring[, these_columns]

# scale the data and the lower bounds -------------------------------------

pca_in_scaled <- scale(pca_in)

pca_in <- as_tibble(pca_in_scaled)

these_lower_bounds <- (rep(0, ncol(pca_in)) - attr(pca_in_scaled, "scaled:center")) /
  attr(pca_in_scaled, "scaled:scale")

these_lower_bounds[no_lower_bound] <- NA

# other model inputs ------------------------------------------------------

# include only censored variables in the censoring scheme:

these_indicators <- names(pca_censoring)[apply(pca_censoring, 2, \(x) sum(x) > 0)] |>
  sort()

these_variables <- str_remove(these_indicators, "cens_")

# subset lower bounds to include only censored variables:

these_lower_bounds <- these_lower_bounds[these_variables]

# build model formula:

these_formulas <- lapply(names(pca_in), \(x) bf(paste0(x, " | mi() ~ 1")))

# get censoring limits:

these_limits <- al_i_subset |>
  select(-c(watershed, date, yday)) |>
  select(all_of(c(names(pca_in), names(pca_censoring)))) |>
  rename_with(~ paste0("value_", .x), -starts_with("cens_")) |>
  pivot_longer(
    c(starts_with("value_"), starts_with("cens_")),
    names_to = c(".value", "param"),
    names_pattern = "([^_]+)_(.+)"
  ) |>
  filter(cens == "left") |>
  nest_by(param) |>
  ungroup() |>
  mutate(new = lapply(data, \(x) x$value)) |>
  with(setNames(new, param))

# check that censoring limits, censoring indicators, lower bounds, and variable names
# are in the same order:

stopifnot(
  all(duplicated(
    list(
      names(these_limits),
      names(these_lower_bounds),
      these_variables,
      str_remove(these_indicators, "cens_")
    )
  )[-1])
)

# fit model ---------------------------------------------------------------

stanseed <- 86534

variational_model_data <- cbind(pca_in, select(al_i_subset, starts_with("cens_")))

variational_model_formula <- mvbrmsformula(flist = these_formulas) + set_rescor(TRUE)

variatonal_model_list <- list(gaussian = "gaussian", student = "student") |>
  map(~ make_standata(
    variational_model_formula,
    data = variational_model_data,
    family = .x
  )) |>
  map(~ modify_standata(
    .x,
    variational_model_data,
    these_limits,
    var_xcens = these_variables,
    cens_ind = these_indicators
  ))

variational_model_code <- list(gaussian = "gaussian", student = "student") |>
  map(~ make_stancode(
    variational_model_formula,
    data = variational_model_data,
    family = .x
  )) |>
  map(~ modify_stancode(
    .x,
    modify = "xcens",
    var_xcens = these_variables,
    lower_bound = these_lower_bounds,
    lcl = these_limits
  ))

variational_model_gaussian <- cmdstan_model(stan_file = write_stan_file(variational_model_code$gaussian))
variational_model_student <- cmdstan_model(stan_file = write_stan_file(variational_model_code$student))

variational_model_gaussian <- variational_model_gaussian$variational(
  data = variatonal_model_list$gaussian, seed = stanseed,
  algorithm = "meanfield",
  init = 0
)

variational_model_student <- variational_model_student$variational(
  data = variatonal_model_list$student, seed = stanseed,
  algorithm = "meanfield",
  init = 0
)

variational_model_gaussian$cmdstan_diagnose()
variational_model_student$cmdstan_diagnose()

variational_model_gaussian$save_output_files(
  dir = "models", basename = "correlation-matrix-al-i", random = FALSE, timestamp = FALSE
)

variational_model_student$save_output_files(
  dir = "models", basename = "correlation-matrix-robust-al-i", random = FALSE, timestamp = FALSE
)

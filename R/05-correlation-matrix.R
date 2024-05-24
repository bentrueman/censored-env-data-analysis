
# create models directory -------------------------------------------------

if (!file.exists("models")) dir.create("models")

# setup -------------------------------------------------------------------

library("here")
source(here("R/01-prepare-data.R"))
library("stringr")
library("brms")
library("bgamcar1")

options(mc.cores = parallel::detectCores())

# fit model ---------------------------------------------------------------

# include only censored variables in the censoring scheme:

these_indicators <- names(biosolids_censoring)[apply(biosolids_censoring,2, \(x) sum(x) > 0)]

these_variables <- str_replace(these_indicators, "censored_", "value_")

# build formula:

these_formulas <- lapply(names(pca_in), \(x) bf(paste0(x, " | mi() ~ 1")))

# censoring limits:

these_limits <- biosolids_wide |>
  pivot_longer(
    c(starts_with("value_"), starts_with("censored_")),
    names_to = c(".value", "element"),
    names_pattern = "([^_]+)_([^_]+)"
  ) |>
  filter(censored == "left") |>
  nest_by(element) |>
  ungroup() |>
  mutate(new = lapply(data, \(x) x$value)) |>
  with(setNames(new, element))

# priors:

priors <- c(
  set_prior("student_t(3, 0, 10)", class = "Intercept", resp = str_remove(names(pca_in), "_"), lb = 0),
  set_prior("student_t(3, 0, 10)", class = "sigma", resp = str_remove(names(pca_in), "_"), lb = 0),
  set_prior("lkj(2)", class = "rescor")
)

# visualize priors:

# priors |>
#   tidybayes::parse_dist() |>
#   ggplot(aes(y = 0, dist = .dist, args = .args, fill = prior)) +
#   facet_wrap(vars(class)) +
#   tidybayes::stat_slab(normalize = "panels")

# fit model

stanseed <- 215678

model_correlation_matrix <- fit_stan_model(
  file = "models/correlation-matrix",
  seed = stanseed,
  bform = mvbrmsformula(flist = these_formulas) + set_rescor(TRUE),
  bdata = biosolids_wide,
  bpriors = priors,
  car1 = FALSE,
  var_xcens = these_variables,
  cens_ind = these_indicators,
  lcl = these_limits,
  lower_bound = 0,
  family = "gaussian",
  backend = "cmdstanr"
)

model_correlation_matrix_robust <- fit_stan_model(
  file = "models/correlation-matrix-robust",
  seed = stanseed,
  bform = mvbrmsformula(flist = these_formulas) + set_rescor(TRUE),
  bdata = biosolids_wide,
  bpriors = priors,
  car1 = FALSE,
  var_xcens = these_variables,
  cens_ind = these_indicators,
  lcl = these_limits,
  lower_bound = 0,
  backend = "cmdstanr"
)


# create models directory -------------------------------------------------

if (!file.exists("models")) dir.create("models")

# setup -------------------------------------------------------------------

library("here")
source(here("R/01-prepare-data.R"))
library("brms")
library("bgamcar1")

options(mc.cores = parallel::detectCores())

# fit model ---------------------------------------------------------------

stanseed <- 494871

priors <- c(
  prior(student_t(3, 0, 2.5), b),
  prior(student_t(3, 0, 2.5), Intercept, resp = valueCo),
  prior(student_t(3, 0, 2.5), Intercept, resp = valueCd)
)

these_limits <- list(biosolids_wide$value_Cd[biosolids_wide$censored_Cd == "left"])

# regenerate mu in generated quantities block:

save_mu <- c(
  "generated quantities" =
  "// vector combining observed and missing responses
  vector[N_valueCd] Yl_valueCd_gq = Y_valueCd;
  // initialize linear predictor term
  vector[N_valueCo] mu_valueCo_gq = rep_vector(0.0, N_valueCo);
  Yl_valueCd_gq[Jmi_valueCd] = Ymi_valueCd;
  Yl_valueCd_gq[Jcens_valueCd] = Ycens_valueCd; // add imputed left-censored values
  mu_valueCo_gq += Intercept_valueCo;
  for (n in 1:N_valueCo) {
    // add more terms to the linear predictor
    mu_valueCo_gq[n] += (bsp_valueCo[1]) * Yl_valueCd_gq[n];
  }"
)


# robust version:

model_censored_predictor_robust <- fit_stan_model(
  file = "models/linear-regression-robust",
  seed = stanseed,
  bform = bf(value_Co | cens(censored_Co) ~ mi(value_Cd)) +
    bf(value_Cd | mi() ~ 1) + set_rescor(FALSE),
  car1 = FALSE,
  var_xcens = "value_Cd",
  cens_ind = "censored_Cd",
  lcl = these_limits,
  lower_bound = 0,
  bdata = biosolids_wide,
  bpriors = priors,
  backend = "cmdstanr",
  stancode = save_mu
)

# Gaussian version:

model_censored_predictor <- fit_stan_model(
  file = "models/linear-regression",
  seed = stanseed,
  bform = bf(value_Co | cens(censored_Co) ~ mi(value_Cd)) +
    bf(value_Cd | mi() ~ 1) + set_rescor(FALSE),
  car1 = FALSE,
  var_xcens = "value_Cd",
  cens_ind = "censored_Cd",
  lcl = these_limits,
  lower_bound = 0,
  bdata = biosolids_wide,
  bpriors = priors,
  backend = "cmdstanr",
  stancode = save_mu,
  family = "gaussian"
)

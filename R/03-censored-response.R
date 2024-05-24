
# create models directory -------------------------------------------------

if (!file.exists("models")) dir.create("models")

# setup -------------------------------------------------------------------

library("here")
source(here("R/01-prepare-data.R"))
library("brms")
library("bgamcar1")
library("assertr")

options(mc.cores = parallel::detectCores())

# make model input --------------------------------------------------------

gam_in <- biosolids_wide |>
  # aggregate values by site and date:
  group_by(site = paste("site", site), date_collection) |>
  summarize(
    value_Ti = median(value_Ti),
    censored_Ti = median(censored_Ti == "left"),
    # - less than 0.5 means that a minority of the values to be aggregated are censored, so median(value_Ti) remains uncensored
    # - greater than 0.5 means that a majority of the values to be aggregated are censored, so median(value_Ti) gets censored
    # - equal to 0.5 means that median(value_Ti) is the average of a censored and an uncensored value, so it gets censored
    censored_Ti = if_else(censored_Ti >= 0.5, "left", "none")
  ) |>
  arrange(site, date_collection) |>
  mutate(
    date_numeric = as.numeric(date_collection),
    date_numeric = date_numeric - min(date_numeric) + 1,
    logvalue_Ti = log(value_Ti),
    # dividing by 14 normalizes to the most common time difference:
    d_x = replace_na(date_numeric - lag(date_numeric), 0) / 14,
  ) |>
  ungroup() |>
  # confirm that d_x == 1 occurs at the maximum frequency:
  verify(with(as.data.frame(table(d_x)), d_x[which.max(Freq)]) == 1) |>
  verify(d_x >= 0)

# fit model ---------------------------------------------------------------

stanseed <- 215678

priors <- c(
  prior(student_t(3, 0, 2.5), b),
  prior(student_t(3, 0, 2.5), Intercept),
  prior(normal(.5, .5), ar, lb = 0, ub = 1)
)

model_censored_response <- fit_stan_model(
  file = "models/gam-w-car1",
  seed = stanseed,
  bform = bf(
    logvalue_Ti | cens(censored_Ti) ~
      1 + site +
      s(date_numeric) +
      ar(time = date_numeric, gr = site)
  ),
  bdata = gam_in,
  bpriors = priors,
  backend = "cmdstanr"
)

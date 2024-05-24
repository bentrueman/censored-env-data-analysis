
# setup -------------------------------------------------------------------

library("here")
library("readr")
library("dplyr")
library("tidyr")

# read --------------------------------------------------------------------

biosolids <- read_csv(here("data-clean/biosolids-subset-clean.csv"))

# wrangle -----------------------------------------------------------------

biosolids_wide <- biosolids |>
  pivot_wider(
    id_cols = c(site, sample_number, date_collection),
    names_from = element,
    values_from = c(value, censored)
  ) |>
  mutate(across(starts_with("censored_"), ~ replace_na(.x, "none")))

biosolids_censoring <- biosolids_wide |>
  transmute(across(starts_with("censored_"), ~ .x == "left"))

pca_in <- biosolids_wide |>
  select(-c(site, sample_number, date_collection, starts_with("censored_")))

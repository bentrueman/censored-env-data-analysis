
# notes -------------------------------------------------------------------

# testing the ppca functions on a different dataset. this script is called from "02-ppca-al-i-dataset.R

# setup -------------------------------------------------------------------

library("here")
library("readr")
library("dplyr")
library("tidyr")

# read --------------------------------------------------------------------

al_i_data <- read_csv("https://raw.githubusercontent.com/bentrueman/al-i-prediction/main/data-clean/data-al-i-train.csv")

# wrangle -----------------------------------------------------------------

al_i_subset <- al_i_data |>
  select(-c(latitude, longitude, matches("lab"))) |>
  select(watershed, date, yday, where(is.numeric), starts_with("cens_"))

pca_in <- al_i_subset |>
  select(-yday) |>
  select(where(is.numeric))

pca_censoring <- al_i_subset |>
  transmute(across(starts_with("cens_"), ~ .x == "left"))

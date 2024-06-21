
# notes -------------------------------------------------------------------

# testing the ppca functions on a different dataset

# create models directory -------------------------------------------------

if (!file.exists("models")) dir.create("models")

# setup -------------------------------------------------------------------

library("here")
library("cmdstanr")
source(here("R/extras/01-prepare-data-al-i.R"))
source(here("R/functions-ppca.R"))

options(mc.cores = parallel::detectCores())

# ppca model inputs -------------------------------------------------------

standata_cens <- build_standatalist(pca_in, 2, pca_censoring)

stanseed <- 215678

ppca_scode <- modify_stan_template(standata_cens)

# load ppca model from CSVs -----------------------------------------------

model_ppca <- try(as_cmdstan_fit(list.files("models", pattern = "ppca-al-i-\\d", full.names = TRUE)))

# fit ppca model ----------------------------------------------------------

if (all(class(model_ppca) != "CmdStanFit")) {

  model <- cmdstan_model(stan_file = write_stan_file(ppca_scode))

  model_ppca <- model$variational(data = standata_cens, seed = stanseed, algorithm = "meanfield")

  model_ppca$cmdstan_diagnose()

  model_ppca$save_output_files(
    dir = "models", basename = "ppca-al-i", random = FALSE, timestamp = FALSE
  )

}

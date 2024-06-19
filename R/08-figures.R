
# setup -------------------------------------------------------------------

library("here")
library("ggplot2")
library("ggdist")
library("ggtext")
library("tidytext")
library("patchwork")
library("assertr")
library("PNWColors")
library("purrr")
library("stringr")
library("ggrepel")
library("devtools", include.only = "session_info")

theme_set(
  theme_bw() +
    theme(
      legend.position = "bottom",
      plot.tag = element_text(face = "bold"),
      axis.title = element_markdown(),
      legend.margin = margin()
    )
)

colour_palette <- pnw_palette("Bay", 7)

# correlation matrix ------------------------------------------------------

source(here("R/figure-scripts/figure-correlation-matrix.R"))

# ppca --------------------------------------------------------------------

source(here("R/figure-scripts/figure-ppca.R"))

# censored response -------------------------------------------------------

source(here("R/figure-scripts/figure-censored-response.R"))

# censored predictor ------------------------------------------------------

source(here("R/figure-scripts/figure-censored-predictor.R"))

# simulation --------------------------------------------------------------

source(here("R/figure-scripts/figure-simulation.R"))

# session info ------------------------------------------------------------

writeLines(capture.output(devtools::session_info()), "session-info.txt")

# write r packages to bibtex file -----------------------------------------

knitr::write_bib(file = "Rmarkdown/references-packages.bib")

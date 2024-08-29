
# run this from "08-figures.R"

# setup -------------------------------------------------------------------

source(here("R/06-simulation.R"))
library("readr")

# make plot ---------------------------------------------------------------

these_labels <-tibble(
  x = c(20, 12),
  y_observed = c(4.5, 2.5),
  labels = c("Observed", "Non-detects")
)

this_simulation <- 20

predictions_censored <- simulation$data[[this_simulation]] |>
  bind_cols(fitted(simulation$model_censoring[[this_simulation]]))

predictions_naive <- simulation$data[[this_simulation]] |>
  bind_cols(fitted(simulation$model_naive[[this_simulation]]))

toc_art <- simulation$data[[this_simulation]] |>
  ggplot(aes(x, y_observed)) +
  scale_fill_manual(values = colour_palette[c(1, 4, 7)]) +
  scale_color_manual(values = colour_palette[c(1, 4, 7)]) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  geom_label(
    data = these_labels,
    aes(label = labels),
    label.size = 0,
    alpha = 0.3,
    size = 3
  ) +
  geom_point(
    data = \(x) x |>
      filter(censored == "none")
  ) +
  geom_segment(
    data = \(x) x |>
      filter(censored == "left"),
    aes(xend = x, y = -Inf, yend = y_observed),
    linetype = 3, linewidth = 0.5, col = "grey"
  ) +
  # censored model:
  geom_ribbon(
    data = predictions_censored,
    aes(ymin = Q2.5, ymax = Q97.5, fill = "Censored\nmodel"),
    alpha = 0.3
  ) +
  geom_line(
    data = predictions_censored,
    aes(y = Estimate, col = "Censored\nmodel")
  ) +
  # naive model:
  geom_ribbon(
    data = predictions_naive,
    aes(ymin = Q2.5, ymax = Q97.5, fill = "Naive\nmodel"),
    alpha = 0.3
  ) +
  geom_line(
    data = predictions_naive,
    aes(y = Estimate, col = "Naive\nmodel")
  ) +
  # true relationship:
  geom_line(
    data = \(x) x |>
      reframe(x = range(x), y_observed = beta0 + beta1 * x),
    aes(col = "True\nrelationship", fill = "True\nrelationship"),
    linetype = 3, linewidth = 1
  ) +
  labs(col = NULL, fill = NULL)

# save --------------------------------------------------------------------

ggsave(
  here("figures/figure-toc-art.png"),
  toc_art,
  width = 3.33, height = 1.665,
  dev = "png", dpi = 600
)


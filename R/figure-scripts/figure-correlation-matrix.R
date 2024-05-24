
# run this from "08-figures.R"

# inputs ------------------------------------------------------------------

source(here("R/05-correlation-matrix.R"))

plot_in <- list(
  "student-t" = model_correlation_matrix_robust,
  "gaussian" = model_correlation_matrix
  # "naive" = model_correlation_matrix_naive
) |>
  map(as_draws_df) |>
  list_rbind(names_to = "model") |>
  select(model, starts_with("rescor__")) |>
  pivot_longer(-model, names_to = "parameter") |>
  group_by(
    model, parameter,
    v1 = str_extract(parameter, "(?<=rescor__value)[A-Z][a-z]?"),
    v2 = str_extract(parameter, "[A-Z][a-z]?$")
  ) |>
  summarize(correlation = median(value)) |>
  ungroup()

# plot_in <- model_correlation_matrix |>
#   as_draws_df() |>
#   select(starts_with("rescor__")) |>
#   pivot_longer(everything()) |>
#   group_by(
#     name,
#     v1 = str_extract(name, "(?<=rescor__value)[A-Z][a-z]?"),
#     v2 = str_extract(name, "[A-Z][a-z]?$")
#   ) |>
#   summarize(correlation = median(value)) |>
#   ungroup()

# to keep colour scale symmetric, get most extreme correlation:
extent <- max(abs(plot_in$correlation))

# correlation matrix ------------------------------------------------------

plot_correlation_matrix <- plot_in |>
  ggplot(aes(v2, v1, fill = correlation)) +
  facet_wrap(
    vars(model),
    # labeller = as_labeller(c("gaussian" = "Gaussian likelihood", "student-t" = "Student-t likelihood", "naive" = "Naive model")),
    labeller = as_labeller(c("gaussian" = "Gaussian likelihood", "student-t" = "Student-t likelihood")),
    ncol = 1
  ) +
  scale_fill_gradientn(colors = colour_palette[c(2, 4, 7)], limits = c(-extent, extent)) +
  geom_tile() +
  theme(panel.grid = element_blank()) +
  guides(size = "none") +
  labs(
    x = NULL,
    y = NULL,
    fill = "Bayesian\ncorrelation"
  )

plot_correlation_matrix <- plot_in |>
  ggplot(aes(col = correlation, size = abs(correlation))) +
  geom_point(
    data = \(x) x |>
      filter(model == "gaussian"),
    aes(v1, v2)
  ) +
  geom_point(
    data = \(x) x |>
      filter(model != "gaussian"),
    aes(v2, v1)
  ) +
  geom_abline() +
  # geom_text(
  geom_richtext(
    data = tibble(
      x = c(5.95, 6.3),
      y = c(6.3, 5.9),
      label = c("Gaussian likelihood &uarr;", "Student-t likelihood &darr;")
    ),
    aes(x, y, label = label), angle = 42, size = 2,
    inherit.aes = FALSE, label.size = 0,
    # alpha = 0.5,
    label.padding = unit(0, "lines")
  ) +
  theme(panel.grid = element_blank()) +
  scale_color_gradientn(colors = colour_palette[c(2, 4, 7)], limits = c(-extent, extent)) +
  guides(size = "none") +
  labs(
    x = NULL,
    y = NULL,
    col = "Bayesian\ncorrelation"
  )

plot_correlation_matrix <- plot_in |>
  ggplot(aes(col = correlation, size = abs(correlation))) +
  geom_point(
    data = \(x) x |>
      filter(model == "gaussian"),
    aes(v1, v2)
  ) +
  geom_point(
    data = \(x) x |>
      filter(model != "gaussian"),
    aes(v2, v1)
  ) +
  geom_polygon(
    data = tibble(
      x = c(0, 10, 0, 0, 10, 10), y = c(0, 10, 10, 0, 0, 10),
      likelihood = rep(c("Gaussian likelihood", "Student t likelihood"), each = 3)
    ),
    aes(x, y, fill = likelihood),
    inherit.aes = FALSE,
    alpha = 0.3
  ) +
  # geom_abline() +
  # geom_text(
  # geom_richtext(
  #   data = tibble(
  #     x = c(5.95, 6.3),
  #     y = c(6.3, 5.9),
  #     label = c("Gaussian likelihood &uarr;", "Student-t likelihood &darr;")
  #   ),
  #   aes(x, y, label = label), angle = 42, size = 2,
  #   inherit.aes = FALSE, label.size = 0,
  #   # alpha = 0,
  #   label.padding = unit(0, "lines")
  # ) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    legend.box = "vertical",
    legend.margin = margin(r = 25)
  ) +
  scale_color_gradientn(colors = colour_palette[c(2, 4, 7)], limits = c(-extent, extent)) +
  scale_fill_manual(values = c("grey", "white")) +
  guides(fill = guide_legend(theme = theme(legend.key = element_rect(colour = "black")))) +
  guides(size = "none") +
  labs(
    x = NULL,
    y = NULL,
    col = "Bayesian\ncorrelation",
    fill = NULL
  )

# plot_correlation_matrix$layers <- plot_correlation_matrix$layers[c(3, 1, 2,4)]

# ecdf --------------------------------------------------------------------

plot_ecdf <- plot_in |>
  ggplot(aes(abs(correlation), col = model)) +
  scale_color_manual(
    values = colour_palette[c(4, 1)],
    labels = c("Gaussian likelihood", "Student t likelihood")
  ) +
  theme(legend.margin = margin(r = 25)) +
  stat_ecdf() +
  labs(
    x = "Bayesian correlation<br>(absolute value)",
    y = "Empirical cumulative<br>distribution function",
    col = NULL
  )

# combine -----------------------------------------------------------------

plot_correlation <- wrap_plots(plot_correlation_matrix, plot_ecdf, ncol = 1, heights = c(1.2, 0.8)) +
  plot_annotation(tag_levels = "a")

# save --------------------------------------------------------------------

ggsave(
  here("R/figures/figure-correlation-matrix.png"),
  plot_correlation,
  width = 3.33, height = 6.5,
  dev = "png", dpi = 600
)


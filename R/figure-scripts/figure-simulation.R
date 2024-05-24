
# run this from "08-figures.R"

# setup -------------------------------------------------------------------

# inputs ------------------------------------------------------------------

source(here("R/06-simulation.R"))
library("readr")

# make plot ---------------------------------------------------------------

# panel a:

this_simulation <- 20

predictions_censored <- simulation$data[[this_simulation]] |>
  bind_cols(fitted(simulation$model_censoring[[this_simulation]]))

predictions_naive <- simulation$data[[this_simulation]] |>
  bind_cols(fitted(simulation$model_naive[[this_simulation]]))

panel_a <- simulation$data[[this_simulation]] |>
  ggplot(aes(x, y_observed)) +
  # scale_fill_manual(values = colour_palette[c(1, 3, 5)]) +
  # scale_color_manual(values = colour_palette[c(1, 3, 5)]) +
  scale_fill_manual(values = colour_palette[c(1, 4, 7)]) +
  scale_color_manual(values = colour_palette[c(1, 4, 7)]) +
  theme(plot.margin = margin(b = 0)) +
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
  # guides(fill = "none") +
  labs(
    y = "y<sub>observed</sub>",
    col = NULL,
    fill = NULL
  )

# panel b:

panel_b_in <- simulation |>
  unnest(starts_with("params_")) |>
  group_by(simulation, param) |>
  summarize(
    across(
      starts_with("value"),
      .fns = list(".median" = median, ".lower" = ~ quantile(.x, .025), ".upper" = ~ quantile(.x, .975)),
      .names = "{.fn}_{.col}"
    )
  ) |>
  ungroup() |>
  pivot_longer(
    starts_with("."),
    names_to = c(".value", "model"),
    names_pattern = "([^_]+)_(.+)"
  ) |>
  left_join(true_values, by = "param") |>
  mutate(
    correct = sign(.lower - value) != sign(.upper - value),
    correct_label = if_else(correct, "Yes", "No")
  ) |>
  arrange(model, param, .median) |>
  mutate(simulation_reordered = rep(seq(n_simulations), 6))

# panel_b <- panel_b_in |>
#   nest_by(model) |>
#   with(data) |>
#   map(
#     \(x) x |>
#       ggplot(aes(simulation_reordered, .median, col = correct)) +
#       facet_wrap(
#         vars(param), scales = "free",
#         labeller = as_labeller(c(
#           "b_Intercept" = "&beta;<sub>0</sub>", "b_x" = "&beta;<sub>1</sub>", "sigma" = "&sigma;"
#         ))
#       ) +
#       scale_color_manual(values = c("grey", colour_palette[2])) +
#       theme(strip.text = element_markdown()) +
#       geom_errorbar(aes(ymin = .lower, ymax = .upper), width = 0, position = position_dodge(width = 1)) +
#       geom_point(position = position_dodge(width = 1)) +
#       geom_hline(data = true_values, aes(yintercept = value), linetype = 3) +
#       guides(col = "none") +
#       labs(x = "Simulation", y = "Estimate")
#   ) |>
#   wrap_plots(ncol = 1) +
#   plot_layout(guides = "collect")

panel_b <- panel_b_in |>
  ggplot(aes(simulation_reordered, .median, col = correct_label)) +
  facet_grid(
    rows = vars(param), cols = vars(model), scales = "free",
    labeller = labeller(
      .rows = as_labeller(c(
        "b_Intercept" = "&beta;<sub>0</sub>", "b_x" = "&beta;<sub>1</sub>", "sigma" = "&sigma;"
      )),
      .cols = c("value_censoring" = "Censored model", "value_naive" = "Naive model")
    )
  ) +
  # scale_color_manual(values = c("grey", colour_palette[2])) +
  # scale_color_manual(values = colour_palette[c(4, 1)]) +
  scale_color_manual(values = c("grey", colour_palette[1])) +
  theme(strip.text = element_markdown()) +
  geom_errorbar(aes(ymin = .lower, ymax = .upper), width = 0, position = position_dodge(width = 1), linewidth = 0.2) +
  geom_point(position = position_dodge(width = 1), size = 0.5, alpha = 0.5, shape = 16) +
  geom_hline(data = true_values, aes(yintercept = value), linetype = 3) +
  guides(col = guide_legend(override.aes = list(size = 2, linewidth = 0.3))) +
  labs(x = "Simulation", y = "Estimate", col = "95% credible interval\nincludes true value")

# combine plots -----------------------------------------------------------

plot_simulation_output <- wrap_plots(panel_a, panel_b, ncol = 1, heights = c(1, 1.5)) +
  plot_annotation(tag_levels = "a")

# save --------------------------------------------------------------------

ggsave(
  here("R/figures/figure-simulation.png"),
  plot_simulation_output,
  width = 3.33, height = 5,
  dev = "png", dpi = 600
)

# write output summary for rmd file ---------------------------------------

percent_correct <- panel_b_in |>
  group_by(model, param) |>
  summarize(percent_correct = 100 * mean(correct)) |>
  ungroup()

write_csv(percent_correct, "data-clean/simulation-study-summary.csv")

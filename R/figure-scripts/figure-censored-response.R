
# run this from "08-figures.R"

# inputs ------------------------------------------------------------------

source(here("R/03-censored-response.R"))
library("tibble")

predictions <- add_pred_draws_car1(gam_in, model_censored_response) |>
  summarize_preds(retrans = FALSE)

draws_censored_response <- as_draws_df(model_censored_response)

site_differences <- draws_censored_response |>
  transmute(
    site_1 = b_Intercept,
    site_2 = b_Intercept + b_sitesite2,
    site_3 = b_Intercept + b_sitesite3
  ) |>
  pivot_longer(everything(), names_to = "site")

# plot --------------------------------------------------------------------

limit_upper_y <- 175 # upper limit on y axis

plot_timeseries <- predictions |>
  mutate(
    site = str_to_title(site),
    across(c(.lower, .epred, .upper), exp)
  ) |>
  ggplot(aes(date_collection, value_Ti, col = site)) +
  # scale_color_manual(values = colour_palette[c(1, 3, 5)]) +
  # scale_fill_manual(values = colour_palette[c(1, 3, 5)]) +
  scale_color_manual(values = colour_palette[c(1, 4, 7)]) +
  scale_fill_manual(values = colour_palette[c(1, 4, 7)]) +
  geom_line(
    data = \(x) filter(x, censored_Ti == "none"),
    linewidth = 0.2
  ) +
  geom_segment(
    data = \(x) filter(x, censored_Ti == "left"),
    aes(yend = 0, xend = date_collection),
    linetype = 3
  ) +
  geom_ribbon(
    aes(ymin = .lower, ymax = .upper, fill = site),
    alpha = 0.3, col = NA,
  ) +
  geom_line(aes(y = .epred)) +
  geom_richtext(
    data = \(x) filter(x, value_Ti > limit_upper_y) |>
      mutate(label = paste(round(value_Ti), "&mu;g g<sup>-1</sup> &uarr;"), value_Ti = pmin(value_Ti, limit_upper_y)),
    aes(label = label),
    label.size = 0, alpha = 1, size = 2.5,
    show.legend = FALSE
  ) +
  labs(
    x = NULL,
    y = "[Ti] (&mu;g g<sup>-1</sup> dry wt.)",
    col = NULL, fill = NULL
  ) +
  ylim(c(0, 175))

plot_differences <- site_differences |>
  ggplot(aes(exp(value), fill = site)) +
  scale_fill_manual(values = colour_palette[c(1, 4, 7)]) +
  stat_slab(alpha = 0.7, show.legend = FALSE) +
  labs(
    x = "[Ti] (&mu;g g<sup>-1</sup> dry wt.)",
    y = "Density"
  )

# combine -----------------------------------------------------------------

plot_timeseries_combined <- wrap_plots(plot_timeseries, plot_differences, ncol = 1, heights = c(2, 1)) +
  plot_annotation(tag_levels = "a") +
  plot_layout(guides = "collect")

# save --------------------------------------------------------------------

ggsave(
  here("R/figures/figure-censored-response.png"),
  plot_timeseries_combined,
  width = 3.33, height = 4,
  dev = "png", dpi = 600
)

# write draws data for rmd document ---------------------------------------

quantile(draws_censored_response$`ar[1]`, c(0.025, 0.5, 0.975)) |>
  as_tibble_row() |>
  write_csv("data-clean/censored-response-draws-car1.csv")

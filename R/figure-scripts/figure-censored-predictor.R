
# run this from "08-figures.R"

# setup -------------------------------------------------------------------

library("stringr")

# inputs ------------------------------------------------------------------

source(here("R/04-censored-predictor.R"))

model_list <- list(
  "Gaussian likelihood" = model_censored_predictor,
  "Student t likelihood" = model_censored_predictor_robust
)

predictions <- model_list |>
  map(as_draws_df) |>
  list_rbind(names_to = "model") |>
  select(model, starts_with("mu_valueCo_gq")) |>
  group_by(model) |>
  summarize(across(
    everything(),
    .fns = list(".epred" = median, ".lower" = ~ quantile(.x, .025), ".upper" = ~ quantile(.x, .975))
  )) |>
  pivot_longer(
    -model,
    names_to = c("id", ".value"),
    names_pattern = "([^\\.]+)(\\..+)"
  ) |>
  mutate(
    id = str_extract(id, "(?<=\\[)\\d+(?=\\])"),
    value_Cd = rep(biosolids_wide$value_Cd, length(model_list)),
    censored_Cd = rep(biosolids_wide$censored_Cd, length(model_list))
  ) |>
  filter(censored_Cd == "none")

# plot --------------------------------------------------------------------

max_y <- 15
max_x <- 5

plot_regression <- biosolids_wide |>
  # verify there are no censored Co/detected Cd pairs:
  verify(sum(censored_Cd == "none" & censored_Co == "left") == 0) |>
  ggplot(aes(value_Cd, value_Co)) +
  geom_point(
    data = . %>% filter(censored_Cd == "none", censored_Co == "none"),
    shape = 16, size = 1.5, alpha = 0.5
  ) +
  # Cd censored:
  geom_segment(
    data = . %>% filter(censored_Cd == "left", censored_Co == "none"),
    aes(xend = 0, yend = value_Co),
    col = "grey40"
  ) +
  # both censored:
  geom_polygon(
    data = \(x) x %>%
      filter(censored_Cd == "left", censored_Co == "left") |>
      with(tibble(
        x = c(0, rep(max(value_Cd), 2), 0),
        y = c(0, 0, rep(max(value_Co), 2))
      )),
    aes(x, y),
    alpha = 0.3
  ) +
  geom_label(
    data = \(x) filter(x, value_Cd > max_x | value_Co > max_y) |>
      mutate(
        value_Cd_trunc = pmin(value_Cd, max_x),
        value_Co_trunc = pmin(value_Co, max_y),
      ),
    aes(value_Cd_trunc, value_Co_trunc, label = paste0("(", round(value_Cd, 2), ",", round(value_Co, 1), ")")),
    label.size = 0, alpha = 0.7, size = 2.5,
    hjust = "inward", vjust = "inward",
  ) +
  geom_ribbon(
    data = predictions,
    aes(y = .epred, ymin = .lower, ymax = .upper, fill = model),
    alpha = 0.3
  ) +
  geom_line(
    data = predictions,
    aes(y = .epred, col = model)
  ) +
  labs(
    x = "[Cd] (&mu;g g<sup>-1</sup> dry wt.)",
    y = "[Co] (&mu;g g<sup>-1</sup> dry wt.)",
    col = NULL,
    fill = NULL
  ) +
  scale_color_manual(values = colour_palette[c(4, 1)]) +
  scale_fill_manual(values = colour_palette[c(4, 1)]) +
  coord_cartesian(xlim = c(0, max_x), ylim = c(0, max_y))

# save --------------------------------------------------------------------

ggsave(
  here("figures/figure-censored-predictor.png"),
  plot_regression,
  width = 3.33, height = 3,
  dev = "png", dpi = 600
)

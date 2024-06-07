
# run this from "08-figures.R"

# inputs ------------------------------------------------------------------

source(here("R/02-ppca.R"))

# plot inputs -------------------------------------------------------------

# extract the principal component scores:

z_estimated <- model_ppca$draws("z") |>
  apply(2, mean) |>
  matrix(standata_cens$N, standata_cens$M, byrow = TRUE) |>
  `colnames<-`(paste0("PC", seq_len(standata_cens$M))) |>
  as_tibble()

# extract transformation matrix W:

w_estimated <- model_ppca$draws("w") |>
  apply(2, mean) |>
  matrix(nrow = standata_cens$D, ncol = standata_cens$M)

w_orthonormalized <- w_estimated |>
  svd() |>
  with(u) |>
  `colnames<-`(paste0("PC", seq_len(standata_cens$M))) |>
  `rownames<-`(unique(biosolids$element)) |>
  as_tibble(rownames = "element")

# plot --------------------------------------------------------------------

plot_ppca <- z_estimated |>
  mutate(site = paste("Site", biosolids_wide$site)) |>
  ggplot(aes(PC1, PC2, col = site)) +
  # scale_color_manual(values = colour_palette[c(1, 3, 5)]) +
  scale_color_manual(values = colour_palette[c(1, 4, 7)]) +
  # annotate extreme points instead of plotting them:
  geom_label(
    data = \(x) x |>
      filter(PC1 >= 2 | PC2 >= 3 | PC1 <= -2.5) |>
      mutate(
        PC1_trunc = pmin(PC1, 1.8),
        PC2_trunc = pmin(PC2, 3),
        PC1_trunc = if_else(PC1_trunc < -1, pmax(-1.8), PC1_trunc)
      ),
    aes(
      x = PC1_trunc, y = PC2_trunc,
      label = paste0("(", round(PC1, 1), ", ", round(PC2, 1), ")"),
      fontface = "bold"
    ),
    label.size = 0, alpha = 0.7, size = 2.5,
    hjust = "inward", vjust = "inward",
    show.legend = FALSE
  ) +
  geom_point(
    data = . %>%
      filter(PC1 < 2, PC2 < 3, PC1 > -2.5)
  ) +
  labs(
    x = "First latent variable (<i>z<sub>k=1</sub></i>)",
    y = "Second latent variable (<i>z<sub>k=2</sub></i>)",
    col = NULL
  )

plot_loadings <- w_orthonormalized |>
  pivot_longer(-element) |>
  ggplot(aes(element, value, col = name, group = name)) +
  scale_color_manual(
    values = colour_palette[c(1, 4)],
    labels = c("First principal axis", "Second principal axis")
  ) +
  geom_line() +
  # geom_point() +
  labs(x = NULL, y = "Loading", col = NULL) +
  theme(
    legend.direction = "horizontal",
    legend.margin = margin(r = 25)
  )

# compare to pca ----------------------------------------------------------

# compare the ppca plot to conventional pca:

# pca <- prcomp(pca_in)
#
# pca$x[,1:2] |>
#   as_tibble() |>
#   ggplot(aes(PC1, PC2)) +
#   geom_point(aes(col = factor(biosolids_wide$site))) +
#   xlim(c(-50, 150)) +
#   ylim(c(-50, 150))
#
# pca$rotation[,1]

# combine -----------------------------------------------------------------

plot_ppca_combined <- wrap_plots(plot_ppca, plot_loadings, ncol = 1, heights = c(3.5, 1)) +
  plot_annotation(tag_levels = "a")

# save --------------------------------------------------------------------

ggsave(
  here("figures/figure-ppca.png"),
  plot_ppca_combined,
  width = 3.33, height = 5.5,
  dev = "png", dpi = 600
)

# Fixed effect

make_fixed_effect_panel <- function(df_one) {
  stopifnot(nrow(df_one) == 1)
  ggplot(df_one, aes(x = mean, y = 1)) +
    geom_errorbarh(aes(xmin = q025, xmax = q975), height = 0.05) +
    geom_point(size = 3) +
    geom_vline(xintercept = 0, linetype = 2, color ="red") +
    # give a little horizontal padding around the CI
    coord_cartesian(xlim = range(c(df_one$q025, df_one$q975)) + c(-1, 1) * 0.02 * diff(range(c(df_one$q025, df_one$q975)))) +
    scale_y_continuous(NULL, breaks = NULL) +
    labs(
      title = df_one$model_short,
      x = "Posterior mean (95% CrI)",
      y = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 19),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    )
}
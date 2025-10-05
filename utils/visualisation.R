
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

# arrow plot for growth rates over budget and coverage ####

arrow_plot <- function(df,
                       y_var,                        # e.g. "growth_rate_coverage_MCV1"
                       color_var = "income_group",   # or "Region"
                       title = NULL,
                       show_equal_ratio = FALSE) {

  # build working data (x = budget growth, y = chosen coverage growth rate)
  df_work <- df %>%
    transmute(
      iso3,
      x = .data[["growth_rate_budget_L1"]],
      y = .data[[y_var]],
      color_by = .data[[color_var]]
    ) %>%
    filter(!is.na(x), !is.na(y))

  # small outward nudge for label along arrow direction (data-dependent)
  rng_x <- diff(range(df_work$x, na.rm = TRUE))
  rng_y <- diff(range(df_work$y, na.rm = TRUE))
  pad_x <- 0.02 * rng_x
  pad_y <- 0.02 * rng_y

  df_lab <- df_work %>%
    mutate(
      len = sqrt(x^2 + y^2),
      ux = ifelse(len > 0, x / len, 0),
      uy = ifelse(len > 0, y / len, 0),
      label_x = x + ux * pad_x,
      label_y = y + uy * pad_y
    )

  # default title
  if (is.null(title)) {
    pretty_name <- gsub("^growth_rate_coverage_", "", y_var)
    title <- paste0(pretty_name, " coverage and budget growth rates")
  }

  p <- ggplot(df_lab, aes(x = x, y = y, color = color_by)) +
    geom_segment(aes(x = 0, y = 0, xend = x, yend = y),
                 arrow = arrow(length = unit(4, "pt"), type = "closed"),
                 linewidth = 0.5, lineend = "round") +
    geom_text(aes(x = label_x, y = label_y, label = iso3),
              size = 2, show.legend = FALSE) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(
      title = title,
      x = "Growth rate of immunisation budget (L1)",
      y = "Growth rate of coverage",
      color = gsub("_", " ", color_var)
    ) +
    theme_minimal(base_size = 10)

  if (isTRUE(show_equal_ratio)) {
    p <- p + coord_equal()
  }

  return(p)
}

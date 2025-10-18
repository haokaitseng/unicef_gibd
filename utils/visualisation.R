
# Fixed effect ####

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

# prediction budget and coverage dual axis plot ####
func_prediction_country_panel <- function(dat) {
  stopifnot(length(unique(dat$iso3)) == 1)

  # country-specific scale so coverage (0–1) maps to left axis
  sf <- max(dat$budget_L1, na.rm = TRUE)
  if (!is.finite(sf) || sf <= 0) sf <- 1  # safety fallback

  # labels: 2025 -> "90 (88–92)", other years -> "90"
  dat_lab <- dat %>%
    dplyr::mutate(
      cov_pct  = round(coverage_mean * 100, 2),
      low_pct  = round(coverage_low  * 100),
      high_pct = round(coverage_high * 100),
      cov_lab  = ifelse(
        year == 2025 & !is.na(low_pct) & !is.na(high_pct),
        paste0(cov_pct, " (", low_pct, "–", high_pct, ")"),
        as.character(cov_pct)
      )
    )

  # 95% CrI shadow only for 2025 (works even with a single x)
  cri_2025 <- dat %>%
    dplyr::filter(year == 2025, !is.na(coverage_low), !is.na(coverage_high)) %>%
    dplyr::transmute(
      xmin = year - 0.35,
      xmax = year + 0.35,
      ymin = coverage_low  * sf,
      ymax = coverage_high * sf,
      year = year,
      Legend_Key = "Coverage (95% CrI)"
    )

  # headroom for labels
  y_top <- max(c(dat$budget_L1,
                 dat$coverage_mean * sf,
                 dat$coverage_high * sf), na.rm = TRUE)
  if (!is.finite(y_top)) y_top <- max(dat$budget_L1, na.rm = TRUE)
  y_pad <- 0.12 * y_top

  ggplot(dat, aes(x = year)) +

    # geom_linerange(
    #   data = cri_2025,
    #   aes(x = year, ymin = ymin, ymax = ymax),
    #   inherit.aes = FALSE,
    #   color = "red",
    #   linewidth = 0.8
    # ) +
    # Budget bars
    geom_col(aes(y = budget_L1), fill = "grey85") +
    # Coverage line & points (scaled to left axis)
    geom_line(aes(y = coverage_mean * sf), color = "blue3", linewidth = 1) +
    geom_point(aes(y = coverage_mean * sf), color = "blue3", size = 2.2) +
    # 95% CrI shadow for 2025
    geom_rect(
      data = cri_2025,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Legend_Key),
      inherit.aes = FALSE,
      fill = "red", alpha = 0.15
    ) +
    # Coverage labels (2025 shows 95% CrI)
    geom_text(
      data = dat_lab,
      aes(y = coverage_mean * sf, label = cov_lab),
      color = "blue3", size = 5, vjust = -0.6
    ) +
    # Axes (ggplot ≥3.5 uses `transform`)
    scale_y_continuous(
      name = "Immunisation budget (US$M)",
      labels = scales::label_number(scale = 1e-6),
      sec.axis = sec_axis(
        transform = ~ . / sf,
        name      = "DTPCV1 Coverage (%)",
        labels    = scales::label_percent(accuracy = 1)
      )
    ) +
    scale_x_continuous(breaks = sort(unique(dat$year))) +
    scale_fill_manual(values = c("Coverage (95% CrI)" = "red"))+
    coord_cartesian(ylim = c(0, y_top + y_pad)) +
    labs(title = paste0(unique(dat$country_name))) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.title.y.right = element_text(color = "blue3"),
      axis.text.y.right  = element_text(color = "blue3"),
      plot.title = element_text(size = 22, face = "bold")
    )
}

purrr::walk(list.files("R", full.names = TRUE), source)

dir.create("figs", showWarnings = FALSE)
z <- qnorm(0.975)

cue <- read_tab("cue_effects.csv") |>
  dplyr::filter(measure == "correct") |>
  dplyr::mutate(
    lower = difference - z * std_error, upper = difference + z * std_error,
    study = factor(study_labels[study], levels = study_labels),
    row = forcats::fct_rev(factor(item_labels[item], levels = item_labels)),
    pooled = item == "pooled"
  )

cue_plot <- ggplot2::ggplot(cue, ggplot2::aes(difference, row, colour = pooled)) +
  geom_zero() +
  geom_estimate() +
  ggplot2::scale_colour_manual(values = c(`FALSE` = "grey20", `TRUE` = "#08519C"), guide = "none") +
  ggplot2::facet_wrap(~study, ncol = 1, scales = "free_y", space = "free_y") +
  ggplot2::scale_x_continuous(labels = \(x) paste0(100 * x), limits = c(-0.65, 0.15), expand = c(0, 0)) +
  ggplot2::labs(x = "Photo minus name: share identifying the office correctly (percentage points)", y = NULL) +
  theme_evidence() +
  ggplot2::theme(strip.text = ggplot2::element_text(hjust = 0))
save_evidence(cue_plot, "figs/cue", width = 6.5, height = 5.4)

measure_labels <- c(
  scale_known = "Confidence scale: certain and ranked first",
  mc_corrected = "Multiple choice, corrected for guessing", mc_correct = "Multiple choice"
)
measure_colours <- c(mc_correct = "grey55", mc_corrected = "grey20", scale_known = "#08519C")
formats <- read_tab("mc_vs_scale.csv") |>
  dplyr::transmute(
    study, item,
    mc_correct_estimate = mc_correct, mc_correct_se = mc_se,
    mc_corrected_estimate = mc_corrected, mc_corrected_se = mc_corrected_se,
    scale_known_estimate = scale_known, scale_known_se = scale_se
  ) |>
  tidyr::pivot_longer(-c(study, item), names_to = c("measure", ".value"), names_pattern = "(.*)_(estimate|se)") |>
  dplyr::mutate(
    lower = pmax(estimate - z * se, 0), upper = pmin(estimate + z * se, 1),
    study = factor(study_labels[study], levels = study_labels),
    row = forcats::fct_rev(factor(item_labels[item], levels = item_labels)),
    measure = factor(measure, levels = names(measure_labels))
  )

format_plot <- ggplot2::ggplot(formats, ggplot2::aes(estimate, row, colour = measure)) +
  geom_estimate(position = ggplot2::position_dodge(width = 0.7)) +
  ggplot2::facet_wrap(~study, ncol = 1, scales = "free_y", space = "free_y") +
  ggplot2::scale_colour_manual(values = measure_colours, labels = measure_labels, name = NULL) +
  ggplot2::scale_x_continuous(
    labels = scales::label_percent(), limits = c(0, 1),
    expand = ggplot2::expansion(mult = c(0, 0.03))
  ) +
  ggplot2::labs(x = "Share of respondents counted as knowing the answer", y = NULL) +
  theme_evidence() +
  ggplot2::guides(colour = ggplot2::guide_legend(reverse = TRUE)) +
  ggplot2::theme(
    strip.text = ggplot2::element_text(hjust = 0),
    legend.position = "top", legend.justification = "left", legend.direction = "vertical"
  )
save_evidence(format_plot, "figs/formats", width = 6.5, height = 7)

# Table 1: design features of the knowledge items in the ANES 2012 and 2016 time series.
# The hand coding lives in docs/knowledge_items.csv (rules in docs/knowledge_items_rules.md). derive_features()
# re-derives every text-based feature from the recorded wording so the tests can hold the hand coding to the rules.

flag_cols <- c(
  "explicit_dk", "dk_probe", "dyt", "wwys", "wipo", "dke_preamble", "dkd_preamble", "happen_to_know",
  "self_placement_dk_filter"
)
option_levels <- c("2", "3", "4", "5", "7", "11", "open")

read_items <- function(path = file.path("docs", "knowledge_items.csv")) {
  readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()), na = character()) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(flag_cols), as.integer)) |>
    assertr::verify(assertr::has_all_names("survey", "variable", "question", "response_options", "correct_answer")) |>
    assertr::assert(assertr::in_set("ANES 2012", "ANES 2016"), "survey") |>
    assertr::assert(assertr::in_set(0L, 1L), dplyr::all_of(flag_cols)) |>
    assertr::assert(assertr::in_set(option_levels), "n_options") |>
    assertr::assert(\(x) nchar(x) > 0, "question", "response_options", "correct_answer", "answer_basis") |>
    assertr::assert_rows(assertr::col_concat, assertr::is_uniq, "survey", "variable")
}

has <- function(x, pattern) as.integer(stringr::str_detect(x, stringr::regex(pattern, ignore_case = TRUE)))

# Only text read or shown to the respondent (preamble, question, options) counts, except for the probe, which
# is an interviewer instruction by nature.
derive_features <- function(items) {
  said <- paste(items$preamble, items$question)
  dk_words <- "don'?t know|not sure|haven'?t (you )?thought|no opinion"
  items |>
    dplyr::transmute(
      survey = .data$survey,
      variable = .data$variable,
      explicit_dk = has(paste(.data$question, .data$response_options), dk_words),
      dk_probe = has(.data$interviewer_instructions, "best guess"),
      # "[Do / Assuming it's happening, do] you think" carries a bracket between "do" and "you".
      dyt = has(said, "\\bdo\\]? you think"),
      wwys = has(said, "would you say"),
      wipo = has(said, "personal opinion"),
      n_options = dplyr::if_else(
        stringr::str_starts(.data$response_options, stringr::fixed("(open")),
        "open",
        as.character(stringr::str_count(.data$response_options, stringr::fixed("|")) + 1L)
      ),
      dke_preamble = has(said, paste0(dk_words, "|(ok|okay|fine) (if|to) (say|not)")),
      dkd_preamble = has(said, "best guess|even if you('re| are) (not sure|unsure)|take a guess|please guess"),
      happen_to_know = has(said, "happen to know")
    )
}

# Closed-form Wilson score interval; stats::prop.test(correct = FALSE) gives the same bounds but warns on the
# zero and small counts this table is full of.
wilson <- function(k, n, z = stats::qnorm(0.975)) {
  p <- k / n
  centre <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  half <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  list(lower = pmax(0, centre - half), upper = pmin(1, centre + half))
}

feature_indicators <- function(items) {
  opts <- purrr::map(option_levels, \(lvl) as.integer(items$n_options == lvl)) |>
    rlang::set_names(paste0("options_", option_levels))
  tibble::tibble(
    survey = items$survey,
    explicit_dk = items$explicit_dk,
    dk_probe = items$dk_probe,
    srew = as.integer(items$dyt | items$wwys | items$wipo),
    dyt = items$dyt,
    wwys = items$wwys,
    wipo = items$wipo,
    !!!opts,
    dke_preamble = items$dke_preamble,
    dkd_preamble = items$dkd_preamble,
    happen_to_know = items$happen_to_know,
    self_placement_dk_filter = items$self_placement_dk_filter
  )
}

feature_table <- function(items) {
  ind <- feature_indicators(items)
  long <- dplyr::bind_rows(ind, dplyr::mutate(ind, survey = "Combined")) |>
    tidyr::pivot_longer(-"survey", names_to = "feature", values_to = "has_feature")
  long |>
    dplyr::summarise(k = sum(.data$has_feature), n = dplyr::n(), .by = c("feature", "survey")) |>
    dplyr::mutate(
      share = .data$k / .data$n,
      lower = wilson(.data$k, .data$n)$lower,
      upper = wilson(.data$k, .data$n)$upper,
      survey = factor(.data$survey, levels = c("Combined", "ANES 2012", "ANES 2016")),
      feature = factor(.data$feature, levels = setdiff(names(ind), "survey"))
    ) |>
    dplyr::arrange(.data$feature, .data$survey) |>
    dplyr::mutate(dplyr::across(c("feature", "survey"), as.character))
}

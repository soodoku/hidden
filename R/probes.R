# Don't-know probes in the ANES office-recognition items and the NAES candidate-issue items.
#
# Every item is reduced to one row per respondent with two facts: whether the interviewer probed (the survey's own
# "probe used" flag) and the final answer (correct, incorrect, or nonsubstantive = don't know, refused, no answer).
# Probes were given only to respondents who first said they did not know, so
#   correct before probing = correct and not probed
#   correct after probing  = correct
#   DK before probing      = probed, or not probed and nonsubstantive (interviewers skipped some probes)
#   DK after probing       = nonsubstantive.
# Nobody can move from correct to incorrect, so the paired before/after comparison has one discordant cell.

anes08_keys <- list(
  strict = list(pelosi = c(1, 2, 4, 11), cheney = c(1, 4, 11), brown = c(1, 2, 4, 11), roberts = c(1, 2, 4, 11))
)
anes08_nonsubstantive <- c(95, 96, 97, 98, 99)

naes04_keys <- list(
  verified = list(repeal_wealthy = c(1, 3, 5, 6, 7), repeal_all = c(2, 4, 8, 9)),
  codebook = list(repeal_wealthy = 5, repeal_all = c(8, 9))
)

read_probe_extract <- function(name, dir = file.path("data", "raw")) {
  readr::read_csv(file.path(dir, paste0(name, ".csv")), show_col_types = FALSE, guess_max = 1e5)
}

outcome_from_codes <- function(x, correct, nonsubstantive) {
  dplyr::case_when(
    is.na(x) | x %in% nonsubstantive ~ "nonsubstantive",
    x %in% correct ~ "correct",
    .default = "incorrect"
  )
}

anes_long <- function(dir = file.path("data", "raw")) {
  a00 <- read_probe_extract("anes2000_office_probe", dir) |>
    assertr::verify(length(case_id) == 1555) |>
    dplyr::filter(.data$k2_form == 1) |>
    tidyr::pivot_longer(
      tidyr::matches("_(exp|probe)$"),
      names_to = c("item", ".value"), names_pattern = "(.*)_(exp|probe)"
    ) |>
    assertr::assert(assertr::in_set(0, 1, 5), "probe") |>
    assertr::assert(assertr::in_set(1, 5, 8, 9), "exp") |>
    dplyr::filter(.data$probe %in% c(1, 5)) |>
    dplyr::transmute(
      survey = "ANES 2000", .data$item, key = "anes", .data$case_id, probed = .data$probe == 1,
      outcome = outcome_from_codes(.data$exp, 1, c(8, 9))
    )

  a04 <- read_probe_extract("anes2004_office_probe", dir) |>
    assertr::verify(length(case_id) == 1066) |>
    dplyr::rename_with(\(x) paste0(x, "_answer"), c("hastert", "cheney", "blair", "rehnquist")) |>
    tidyr::pivot_longer(
      tidyr::matches("_(answer|probe)$"),
      names_to = c("item", ".value"), names_pattern = "(.*)_(answer|probe)"
    ) |>
    assertr::assert(assertr::in_set(1, 5), "probe") |>
    assertr::assert(assertr::in_set(1, 5, 8, 9), "answer") |>
    dplyr::transmute(
      survey = "ANES 2004", .data$item, key = "anes", .data$case_id, probed = .data$probe == 1,
      outcome = outcome_from_codes(.data$answer, 1, c(8, 9))
    )

  raw08 <- read_probe_extract("anes2008_office_probe", dir) |>
    assertr::verify(length(case_id) == 2102) |>
    assertr::assert(assertr::in_set(1, 5), tidyr::ends_with("_probe"))
  figures <- c("pelosi", "cheney", "brown", "roberts")
  a08 <- purrr::map(figures, function(fig) {
    codes <- as.matrix(raw08[paste0(fig, "_code", 1:5)])
    substantive <- !is.na(codes) & !(codes %in% anes08_nonsubstantive)
    hit <- matrix(codes %in% anes08_keys$strict[[fig]], nrow = nrow(codes))
    level1 <- raw08[[paste0(fig, "_level1")]]
    tibble::tibble(
      survey = "ANES 2008", item = fig, case_id = raw08$case_id, probed = raw08[[paste0(fig, "_probe")]] == 1,
      any_substantive = rowSums(substantive) > 0,
      strict = rowSums(hit) > 0,
      anes_level1 = !is.na(level1) & level1 == 1
    )
  }) |>
    dplyr::bind_rows() |>
    tidyr::pivot_longer(c("strict", "anes_level1"), names_to = "key", values_to = "is_correct") |>
    dplyr::transmute(
      .data$survey, .data$item, .data$key, .data$case_id, .data$probed,
      outcome = dplyr::case_when(
        .data$is_correct ~ "correct",
        .data$any_substantive ~ "incorrect",
        .default = "nonsubstantive"
      )
    )

  dplyr::bind_rows(a00, a04, a08)
}

# Multi-mention items: correct means at least one name and every name inside the key.
score_mentions <- function(mentions, key) {
  purrr::map_chr(mentions, function(m) {
    m <- m[!is.na(m)]
    if (length(m) == 0 || any(m %in% c(998, 999))) {
      return("nonsubstantive")
    }
    if (all(m %in% key)) "correct" else "incorrect"
  })
}

naes_long <- function(dir = file.path("data", "raw")) {
  n04 <- read_probe_extract("naes2004_tax_probe", dir) |>
    assertr::assert(assertr::in_set(0, 1, NA), "ccb26", "ccb28", "ccb30", "ccb32")
  single04 <- function(item, answer, probe, correct) {
    d <- dplyr::filter(n04, !is.na(.data[[probe]]))
    tibble::tibble(
      survey = "NAES 2004", item = item, key = "codebook", case_id = d$rkey, probed = d[[probe]] == 1,
      outcome = outcome_from_codes(d[[answer]], correct, c(998, 999))
    )
  }
  multi04 <- function(item, stem, probe) {
    d <- dplyr::filter(n04, !is.na(.data[[probe]]))
    mentions <- purrr::pmap(d[paste0(stem, "_", 1:5)], c)
    purrr::imap(naes04_keys, \(keys, key_name) {
      outcome <- score_mentions(mentions, keys[[item]])
      tibble::tibble(
        survey = "NAES 2004", item = item, key = key_name, case_id = d$rkey, probed = d[[probe]] == 1,
        outcome = outcome
      )
    }) |>
      dplyr::bind_rows()
  }

  n08 <- read_probe_extract("naes2008_tax_probe", dir) |>
    assertr::assert(assertr::in_set(0, 1, NA), tidyr::matches("^cbb0[4-9]a$"))
  single08 <- function(item, stem, correct) {
    d <- dplyr::filter(n08, !is.na(.data[[paste0(stem, "a")]]))
    tibble::tibble(
      survey = "NAES 2008", item = item, key = "codebook", case_id = d$rkey, probed = d[[paste0(stem, "a")]] == 1,
      outcome = outcome_from_codes(d[[stem]], correct, c(998, 999))
    )
  }
  d04 <- dplyr::filter(n08, !is.na(.data$cbb04a))
  named04 <- purrr::pmap(d04[c("cbb04ra", "cbb04rb", "cbb04rc", "cbb04rd")], function(...) {
    x <- c(...)
    if (all(x %in% c(998, 999))) 998 else c(giuliani = 1, huckabee = 2, mccain = 3, romney = 4)[x == 1]
  })
  cbb04 <- tibble::tibble(
    survey = "NAES 2008", item = "r_opposed_cuts_1", key = "codebook", case_id = d04$rkey, probed = d04$cbb04a == 1,
    outcome = dplyr::if_else(lengths(named04) == 0, "incorrect", score_mentions(named04, 3))
  )
  d06 <- dplyr::filter(n08, !is.na(.data$cbb06a))
  cbb06 <- tibble::tibble(
    survey = "NAES 2008", item = "d_eliminate_some_1", key = "codebook", case_id = d06$rkey, probed = d06$cbb06a == 1,
    outcome = dplyr::if_else(d06$cbb06ra %in% c(998, 999), "nonsubstantive", NA_character_)
  )

  dplyr::bind_rows(
    single04("cut_permanent", "ccb25", "ccb26", 1),
    single04("kerry_income", "ccb27", "ccb28", 3),
    multi04("repeal_wealthy", "ccb29", "ccb30"),
    multi04("repeal_all", "ccb31", "ccb32"),
    cbb04,
    single08("r_opposed_cuts_2", "cbb05", 2),
    cbb06,
    single08("d_eliminate_some_2", "cbb07", 3),
    single08("eliminate_some", "cbb08", 2),
    single08("d_working_families", "cbb09", 2)
  )
}

prop_se <- function(p, n) sqrt(p * (1 - p) / n)

mcnemar_p <- function(converted) {
  if (is.na(converted)) {
    return(NA_real_)
  }
  if (converted == 0) {
    return(1)
  }
  stats::binom.test(converted, converted)$p.value
}

# The exact McNemar test uses only the discordant pairs: DKs who became correct against correct answers lost after
# probing. Probes went only to DKs, so no correct answer can be lost; p therefore only asks whether any probed DK
# became correct, and is not evidence that probing uncovers knowledge rather than guesses.
summarise_probe <- function(long) {
  long |>
    assertr::assert(assertr::in_set("correct", "incorrect", "nonsubstantive", NA), "outcome") |>
    dplyr::mutate(
      correct = .data$outcome == "correct",
      nonsub = dplyr::coalesce(.data$outcome == "nonsubstantive", FALSE)
    ) |>
    dplyr::summarise(
      n = dplyr::n(),
      n_probed = sum(.data$probed),
      unprobed_dk = sum(!.data$probed & .data$nonsub),
      correct_before = mean(.data$correct & !.data$probed),
      correct_after = mean(.data$correct),
      converted = sum(.data$correct & .data$probed),
      dk_before = mean(.data$probed | .data$nonsub),
      dk_after = mean(.data$nonsub),
      .by = c("survey", "item", "key")
    ) |>
    dplyr::mutate(
      diff = .data$correct_after - .data$correct_before,
      se_before = prop_se(.data$correct_before, .data$n),
      se_after = prop_se(.data$correct_after, .data$n),
      se_diff = prop_se(.data$diff, .data$n),
      p_paired = purrr::map_dbl(.data$converted, mcnemar_p),
      conversion_rate = .data$converted / .data$n_probed
    ) |>
    dplyr::select(
      "survey", "item", "key", "n", "correct_before", "se_before", "correct_after", "se_after", "diff", "se_diff",
      "p_paired", "dk_before", "dk_after", "n_probed", "unprobed_dk", "converted", "conversion_rate"
    )
}

attach_fielding <- function(tab) {
  items <- readr::read_csv(file.path("docs", "probe_items.csv"), show_col_types = FALSE)
  tab |>
    dplyr::left_join(dplyr::select(items, "survey", "item", "fielded"), by = c("survey", "item")) |>
    assertr::assert(assertr::not_na, "fielded")
}

# Averages are unweighted means over the four items under the main key, as in the draft.
anes_probe_table <- function(dir = file.path("data", "raw")) {
  items <- attach_fielding(summarise_probe(anes_long(dir)))
  averages <- items |>
    dplyr::filter(.data$key %in% c("anes", "strict")) |>
    dplyr::summarise(
      item = "average", key = dplyr::first(.data$key), n = dplyr::first(.data$n),
      dplyr::across(c("correct_before", "correct_after", "diff", "dk_before", "dk_after"), mean),
      fielded = dplyr::first(.data$fielded),
      .by = "survey"
    )
  dplyr::bind_rows(items, averages) |>
    dplyr::arrange(.data$survey, !.data$key %in% c("anes", "strict"), .data$item == "average")
}

naes_probe_table <- function(dir = file.path("data", "raw")) {
  attach_fielding(summarise_probe(naes_long(dir)))
}

write_probe_tables <- function(dir = file.path("data", "raw"), out = "tabs") {
  readr::write_csv(anes_probe_table(dir), file.path(out, "anes_probe.csv"), na = "")
  readr::write_csv(naes_probe_table(dir), file.path(out, "naes_probe.csv"), na = "")
}

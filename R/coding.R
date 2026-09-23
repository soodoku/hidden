read_open_codes <- \() read_strict_csv(file.path("docs", "open_codes.csv"))

normalize_answer <- function(x) {
  x |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[’`]", "'") |>
    stringr::str_squish() |>
    dplyr::coalesce("")
}

# Every open-ended answer is coded the same way in every sample. For an office,
# naming the office is correct and the right institution without the office (a
# senator rather than the majority leader) is partial. A blank or an explicit
# "don't know" is dk; anything else is incorrect.
code_open <- function(answer, item, year, codes = read_open_codes()) {
  text <- normalize_answer(answer)
  rules <- dplyr::filter(codes, .data$item == .env$item, .data$year %in% c(as.character(.env$year), "any"))
  if (!any(rules$code == "correct")) stop("No coding rule for ", item, " in ", year)
  hit <- function(code) {
    rule <- dplyr::filter(rules, .data$code == .env$code)
    if (nrow(rule) == 0) {
      return(rep(FALSE, length(text)))
    }
    matched <- stringr::str_detect(text, rule$pattern)
    if (!is.na(rule$exclude)) {
      matched <- matched & !stringr::str_detect(text, rule$exclude)
    }
    matched
  }
  dk <- codes$pattern[codes$code == "dk"]
  dplyr::case_when(
    hit("correct") ~ "correct",
    hit("partial") ~ "partial",
    stringr::str_detect(text, dk) ~ "dk",
    .default = "incorrect"
  )
}

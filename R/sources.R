raw_path <- \(name) file.path(raw_dir, name)

verify_sources <- function() {
  found <- purrr::map_chr(names(raw_files), \(f) digest::digest(file = raw_path(f), algo = "sha256"))
  bad <- names(raw_files)[found != raw_files]
  if (length(bad) > 0) stop("Hash mismatch: ", paste(bad, collapse = ", "))
  invisible(TRUE)
}

read_strict_csv <- function(path) {
  data <- readr::read_csv(path, col_types = readr::cols(.default = "c"), na = c("", "NA"))
  if (nrow(readr::problems(data)) > 0) stop("Parsing problems in ", path)
  data
}

partisanship <- function(party, lean) {
  dplyr::case_when(
    party == "Democrat" ~ "Democrat",
    party == "Republican" ~ "Republican",
    lean == "Democrat" ~ "Democrat",
    lean == "Republican" ~ "Republican",
    !is.na(party) ~ "Independent",
    .default = NA_character_
  )
}

# Ratings were typed into boxes in 2010. Answers of 20 to 100 in steps of 10
# read as the 0-100 scale used elsewhere in the questionnaire and are divided
# by 10; other out-of-range answers are missing.
parse_rating <- function(x) {
  value <- suppressWarnings(as.numeric(x))
  value <- dplyr::if_else(value %in% seq(20, 100, 10), value / 10, value)
  dplyr::if_else(value %in% 0:10, value, NA_real_)
}

# Alumni and staff answered the same instrument. IDs without the panel prefix
# belong to the survey team's test runs. Respondents who opened the survey more
# than once keep their first complete attempt; partial completes are dropped.
read_panel_2010 <- function(file, id_marker, study) {
  read_strict_csv(raw_path(file)) |>
    dplyr::filter(stringr::str_detect(userid, id_marker), !is.na(photo.nonphoto.pk)) |>
    dplyr::mutate(started = lubridate::mdy_hm(date)) |>
    dplyr::arrange(userid, status != "Complete", started) |>
    dplyr::distinct(userid, .keep_all = TRUE) |>
    dplyr::filter(status == "Complete") |>
    dplyr::mutate(
      study = study,
      respondent = userid,
      party = partisanship(
        pid1,
        dplyr::case_when(
          stringr::str_detect(pid2.other, "Democratic") ~ "Democrat",
          stringr::str_detect(pid2.other, "Republican") ~ "Republican"
        )
      ),
      cue = dplyr::if_else(photo.nonphoto.pk == "photo", "photo", "name"),
      question_form = open.closed,
      scale_arm = dplyr::recode(correct.or.conf, percent = "scale", regular = "mc"),
      dk_arm = dplyr::recode(dkencouraging.dk.discouraging, `Branch A` = "encouraging", `Branch B` = "discouraging")
    ) |>
    assertr::assert(assertr::is_uniq, respondent) |>
    assertr::assert(assertr::in_set("photo", "name"), cue)
}

read_alumni <- \() read_panel_2010("alumni_2010.csv", "^FY", "alumni")
read_staff <- \() read_panel_2010("staff_2010.csv", "^srep", "staff")

# Qualtrics exports carry two extra header rows under the column names. The
# study sampled U.S. workers; the few who consented from abroad (by IP country)
# were never assigned to an arm.
read_mturk <- function() {
  path <- raw_path("mturk_march_2017.csv")
  names <- names(readr::read_csv(path, n_max = 0, show_col_types = FALSE))
  readr::read_csv(path, skip = 3, col_names = names, col_types = readr::cols(.default = "c")) |>
    dplyr::filter(Finished == "True", consent == "I agree", ccode == "US", !is.na(visual)) |>
    dplyr::mutate(
      study = "mturk",
      respondent = ResponseId,
      party = partisanship(
        pid,
        dplyr::case_when(
          as.numeric(pid_strength_1) < 5 ~ "Democrat",
          as.numeric(pid_strength_1) > 5 ~ "Republican"
        )
      ),
      cue = dplyr::if_else(visual == "photo", "photo", "name")
    ) |>
    assertr::assert(assertr::is_uniq, respondent) |>
    assertr::assert(assertr::in_set("photo", "text"), visual) |>
    assertr::assert(assertr::in_set("closed", "scale"), reticence_guessing) |>
    assertr::assert(assertr::in_set("closed", "asses"), fugitive_visual) |>
    assertr::assert(assertr::in_set("open", "closed"), probe)
}

# The panel files hold what respondents reported when they joined the panel.
read_panel_demographics <- function(file, id_column, study) {
  read_strict_csv(raw_path(file)) |>
    dplyr::rename(respondent = dplyr::all_of(id_column)) |>
    dplyr::distinct(respondent, .keep_all = TRUE) |>
    dplyr::transmute(
      study = study, respondent,
      birth_year = as.numeric(year),
      gender, ethnic, educ
    )
}

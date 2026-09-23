# Writes the respondent-level extracts behind the ANES and NAES don't-know probe tables.
#
# Usage:
#   Rscript --no-init-file scripts/extract_public_polls.R <old_repo> <anes2008_office_codes.sav>
#
# <old_repo> is the original project directory holding data/anes and data/naes.
# <anes2008_office_codes.sav> is "Political knowledge - All codes2.sav" from ANES2008TS_OfficeRecognition.zip,
# published by ANES at https://electionstudies.org/wp-content/uploads/2018/09/ANES2008TS_OfficeRecognition.zip
# (archived copy: https://web.archive.org/web/20201021182858/<same URL>).
#
# The ANES files are public releases and their extracts are committed. The NAES extracts are written next to them
# but are git-ignored: the Annenberg Public Policy Center's data-access terms forbid posting the data "in whole or
# part" without its written permission (https://www.annenbergpublicpolicycenter.org/data-access/).

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2) {
  stop("Usage: extract_public_polls.R <old_repo> <anes2008_office_codes.sav>")
}
old_repo <- args[[1]]
or08_path <- args[[2]]
out_dir <- file.path("data", "raw")

load_one <- function(path) {
  env <- new.env()
  obj <- load(path, envir = env)
  stopifnot(length(obj) == 1)
  env[[obj]]
}

# The old .Rdata copies carry the authors' derived columns next to the ANES/NAES originals. Only original release
# variables are taken, except the NAES 2004 probe flags, whose original names (cCB26, cCB28, cCB30, cCB32) were
# replaced in the old file; their 0/1 coding and fielding windows match the codebook entries.
mmdd_date <- function(year, mmdd) {
  mmdd <- trimws(mmdd)
  ok <- grepl("^[0-9]{4}$", mmdd) & mmdd != "9999" & mmdd != "0000"
  out <- rep(NA_character_, length(mmdd))
  out[ok] <- format(as.Date(paste0(year, mmdd[ok]), "%Y%m%d"))
  out
}

anes00 <- load_one(file.path(old_repo, "data", "anes", "nes00.rdata")) |>
  dplyr::filter(.data$prepost) |>
  dplyr::transmute(
    case_id = .data$v000001,
    k2_form = .data$v000127b,
    rand_failure = .data$v000262,
    post_date = mmdd_date(2000, .data$v000130),
    lott_std = .data$v001446a, lott_exp = .data$v001446b, lott_probe = .data$v001448,
    rehnquist_std = .data$v001449a, rehnquist_exp = .data$v001449b, rehnquist_probe = .data$v001451,
    blair_std = .data$v001452a, blair_exp = .data$v001452b, blair_probe = .data$v001454,
    reno_std = .data$v001455a, reno_exp = .data$v001455b, reno_probe = .data$v001457
  )

anes04 <- load_one(file.path(old_repo, "data", "anes", "nes04.Rdata")) |>
  dplyr::filter(!is.na(.data$v045162)) |>
  dplyr::transmute(
    case_id = .data$v040001,
    post_date = mmdd_date(2004, .data$v044004),
    hastert = .data$v045162, hastert_probe = .data$v045162a,
    cheney = .data$v045163, cheney_probe = .data$v045163a,
    blair = .data$v045164, blair_probe = .data$v045164a,
    rehnquist = .data$v045165, rehnquist_probe = .data$v045165a
  )

# Verbatim answers are left out: the tables need only ANES's codes, and the old repo's know.open.end.08.csv predates
# ANES's December 2012 redaction (one Roberts answer still carries a personal detail ANES later removed).
or08 <- haven::read_sav(or08_path) |>
  haven::zap_labels() |>
  dplyr::rename(case_id = "ID") |>
  dplyr::rename_with(tolower, -"case_id")

anes08 <- load_one(file.path(old_repo, "data", "anes", "nes08h.Rdata")) |>
  dplyr::filter(.data$v085120a != -2) |>
  dplyr::transmute(
    case_id = .data$v080001,
    post_date = mmdd_date(2008, .data$v084001c),
    pelosi_probe = .data$v085120a, cheney_probe = .data$v085121a,
    brown_probe = .data$v085122a, roberts_probe = .data$v085123a
  ) |>
  dplyr::left_join(or08, by = "case_id")
stopifnot(nrow(anes08) == 2102, nrow(or08) == 2102, all(or08$case_id %in% anes08$case_id))

naes04 <- load_one(file.path(old_repo, "data", "naes", "naes04.rdata")) |>
  dplyr::filter(!is.na(.data$cutdk) | !is.na(.data$kerdk) | !is.na(.data$o2ldk) | !is.na(.data$repdk)) |>
  dplyr::transmute(
    rkey = .data$ckey,
    int_date = format(as.Date(as.character(.data$cdatestr), "%Y%m%d")),
    ccb25 = .data$ccb25, ccb26 = .data$cutdk,
    ccb27 = .data$ccb27, ccb28 = .data$kerdk,
    ccb29_1 = .data$ccb29_1, ccb29_2 = .data$ccb29_2, ccb29_3 = .data$ccb29_3,
    ccb29_4 = .data$ccb29_4, ccb29_5 = .data$ccb29_5, ccb30 = .data$o2ldk,
    ccb31_1 = .data$ccb31_1, ccb31_2 = .data$ccb31_2, ccb31_3 = .data$ccb31_3,
    ccb31_4 = .data$ccb31_4, ccb31_5 = .data$ccb31_5, ccb32 = .data$repdk
  )

# The old NAES 2008 file stores labelled factors; codes are restored from the NAES08-Phone codebook.
# CBb06rB (Edwards) and CBb06rC (Obama) are absent from that file (two CBb04 columns sit in their place), so CBb06
# cannot be scored.
probe_code <- function(x) {
  dplyr::case_when(
    is.na(x) ~ NA_integer_,
    startsWith(as.character(x), "respondent gave answer") ~ 0L,
    startsWith(as.character(x), "respondent said don't know") ~ 1L
  )
}
yes_no_code <- function(x) {
  unname(c(yes = 1L, no = 2L, "don't know" = 998L, "no answer" = 999L)[as.character(x)])
}
choice_code <- function(x, options) {
  codes <- stats::setNames(c(seq_along(options), 998L, 999L), c(options, "don't know", "no answer"))
  unname(codes[as.character(x)])
}

naes08 <- load_one(file.path(old_repo, "data", "naes", "naes08.rdata")) |>
  dplyr::filter(dplyr::if_any(c("cbb04a", "cbb05a", "cbb06a", "cbb07a", "cbb08a", "cbb09a"), \(x) !is.na(x))) |>
  dplyr::transmute(
    rkey = .data$rkey,
    int_date = format(as.Date(.data$cdatestr, "%Y%m%d")),
    cbb04ra = yes_no_code(.data$cbb04ra), cbb04rb = yes_no_code(.data$cbb04rb),
    cbb04rc = yes_no_code(.data$cbb04rc), cbb04rd = yes_no_code(.data$cbb04rd),
    cbb04a = probe_code(.data$cbb04a),
    cbb05 = choice_code(.data$cbb05, c("huckabee", "mccain", "both", "neither")), cbb05a = probe_code(.data$cbb05a),
    cbb06ra = yes_no_code(.data$cbb06ra), cbb06a = probe_code(.data$cbb06a),
    cbb07 = choice_code(.data$cbb07, c("clinton", "obama", "both", "neither")), cbb07a = probe_code(.data$cbb07a),
    cbb08 = choice_code(.data$cbb08, c("mccain", "obama", "both", "neither")), cbb08a = probe_code(.data$cbb08a),
    cbb09 = choice_code(.data$cbb09, c("clinton", "obama", "both", "neither")), cbb09a = probe_code(.data$cbb09a)
  )

extracts <- list(
  anes2000_office_probe = anes00,
  anes2004_office_probe = anes04,
  anes2008_office_probe = anes08,
  naes2004_tax_probe = naes04,
  naes2008_tax_probe = naes08
)

paths <- file.path(out_dir, paste0(names(extracts), ".csv"))
purrr::walk2(extracts, paths, \(d, p) readr::write_csv(d, p, na = ""))

hashes <- vapply(paths, \(p) digest::digest(file = p, algo = "sha256"), character(1))
writeLines(paste0(hashes, "  ", basename(paths)), file.path(out_dir, "probe_extracts.sha256"))
print(tibble::tibble(file = basename(paths), rows = vapply(extracts, nrow, integer(1)), sha256 = unname(hashes)))

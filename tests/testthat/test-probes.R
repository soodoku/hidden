root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
old_wd <- setwd(root)
withr::defer(setwd(old_wd), testthat::teardown_env())
source(file.path("R", "probes.R"), local = TRUE)

have_naes <- file.exists(file.path("data", "raw", "naes2004_tax_probe.csv"))

pick <- function(tab, survey, item, key, col) {
  tab[[col]][tab$survey == survey & tab$item == item & tab$key == key]
}

# Rounding to the draft's three decimals.
expect_rounds_to <- function(x, target, tol = 0.0005) expect_lte(abs(x - target), tol + 1e-9)

test_that("extracts match the hashes recorded when they were written", {
  lines <- readLines(file.path("data", "raw", "probe_extracts.sha256"))
  sums <- stats::setNames(sub("  .*", "", lines), sub(".*  ", "", lines))
  for (f in names(sums)) {
    path <- file.path("data", "raw", f)
    if (!file.exists(path)) next
    expect_equal(digest::digest(file = path, algo = "sha256"), sums[[f]], label = f)
  }
})

test_that("ANES 2000 extract reproduces the codebook marginals (release 2005-Oct-06)", {
  a <- read_probe_extract("anes2000_office_probe")
  tab <- \(x) as.vector(table(factor(x[x != 0], levels = c(1, 5, 8, 9))))
  expect_equal(tab(a$lott_exp), c(50, 167, 311, 2))
  expect_equal(tab(a$rehnquist_exp), c(66, 189, 274, 1))
  expect_equal(tab(a$blair_exp), c(189, 69, 271, 1))
  expect_equal(tab(a$reno_exp), c(296, 108, 125, 1))
  expect_equal(tab(a$lott_probe)[1:2], c(241, 287))
  expect_equal(tab(a$rehnquist_probe)[1:2], c(220, 308))
  expect_equal(tab(a$blair_probe)[1:2], c(218, 311))
  expect_equal(tab(a$reno_probe)[1:2], c(130, 399))
  expect_equal(as.vector(table(a$k2_form)), c(1025, 530))
})

test_that("ANES 2004 extract reproduces the codebook marginals (release 2005AUG16)", {
  a <- read_probe_extract("anes2004_office_probe")
  tab <- \(x) as.vector(table(factor(x, levels = c(1, 5, 8))))
  expect_equal(tab(a$hastert), c(117, 145, 804))
  expect_equal(tab(a$cheney), c(917, 63, 86))
  expect_equal(tab(a$blair), c(692, 134, 240))
  expect_equal(tab(a$rehnquist), c(332, 252, 482))
  expect_equal(tab(a$hastert_probe)[1:2], c(557, 509))
  expect_equal(tab(a$cheney_probe)[1:2], c(112, 954))
  expect_equal(tab(a$blair_probe)[1:2], c(220, 846))
  expect_equal(tab(a$rehnquist_probe)[1:2], c(346, 720))
})

test_that("ANES 2008 codes cover the answers counted in the ANES coding report", {
  a <- read_probe_extract("anes2008_office_probe")
  counts <- vapply(c("brown", "cheney", "pelosi", "roberts"), \(f) sum(!is.na(a[[paste0(f, "_code1")]])), numeric(1))
  expect_equal(unname(counts), c(2098, 2095, 2096, 2093))
})

test_that("ANES proportions correct match the draft where the draft used the ANES codes (2000, 2004)", {
  tab <- anes_probe_table()
  draft <- tibble::tribble(
    ~survey, ~item, ~before, ~after, ~probed,
    "ANES 2000", "lott", .091, .095, .456,
    "ANES 2000", "rehnquist", .121, .125, .417,
    "ANES 2000", "blair", .333, .357, .412,
    "ANES 2000", "reno", .524, .560, .246,
    "ANES 2004", "hastert", .098, .110, .523,
    "ANES 2004", "cheney", .831, .860, .105,
    "ANES 2004", "blair", .612, .649, .206,
    "ANES 2004", "rehnquist", .295, .311, .325
  )
  for (i in seq_len(nrow(draft))) {
    d <- draft[i, ]
    expect_rounds_to(pick(tab, d$survey, d$item, "anes", "correct_before"), d$before)
    expect_rounds_to(pick(tab, d$survey, d$item, "anes", "correct_after"), d$after)
    # The draft's "DKs" column is the share probed, which omits DKs the interviewer did not probe.
    share <- pick(tab, d$survey, d$item, "anes", "n_probed") / pick(tab, d$survey, d$item, "anes", "n")
    expect_rounds_to(share, d$probed)
  }
})

test_that("before and after are nested: probing only adds correct answers and removes DKs", {
  tab <- dplyr::filter(anes_probe_table(), .data$item != "average")
  expect_true(all(tab$correct_after >= tab$correct_before))
  expect_true(all(tab$dk_after <= tab$dk_before))
  expect_equal(tab$diff, tab$converted / tab$n)
})

test_that("ANES 2008 main key matches the documented ANES master codes", {
  items <- readr::read_csv(file.path("docs", "probe_items.csv"), show_col_types = FALSE)
  documented <- dplyr::filter(items, .data$survey == "ANES 2008")
  for (fig in documented$item) {
    codes <- as.numeric(strsplit(documented$key_codes[documented$item == fig], ",")[[1]])
    expect_equal(anes08_keys$strict[[fig]], codes, label = fig)
  }
})

test_that("NAES 2004 and 2008 match the draft where the draft's coding holds", {
  skip_if_not(have_naes, "NAES extracts are not distributed")
  tab <- naes_probe_table()
  draft <- tibble::tribble(
    ~survey, ~item, ~before, ~after, ~probed, ~n,
    "NAES 2004", "cut_permanent", .651, .654, .102, 21141,
    "NAES 2004", "kerry_income", .335, .339, .244, 31940,
    "NAES 2008", "r_opposed_cuts_1", .300, .307, .377, 5200,
    "NAES 2008", "r_opposed_cuts_2", .315, .320, .248, 6824,
    "NAES 2008", "d_eliminate_some_2", .409, .411, .168, 20895,
    "NAES 2008", "eliminate_some", .651, .654, .135, 28325,
    "NAES 2008", "d_working_families", .193, .200, .338, 14150
  )
  for (i in seq_len(nrow(draft))) {
    d <- draft[i, ]
    expect_equal(pick(tab, d$survey, d$item, "codebook", "n"), d$n)
    # The draft prints 13774 / 21141 = .65153 as .651.
    expect_rounds_to(pick(tab, d$survey, d$item, "codebook", "correct_before"), d$before, tol = 0.0006)
    expect_rounds_to(pick(tab, d$survey, d$item, "codebook", "correct_after"), d$after)
    expect_rounds_to(pick(tab, d$survey, d$item, "codebook", "n_probed") / d$n, d$probed)
  }
})

test_that("NAES 2008 CBb06 is reported without a correct rate", {
  skip_if_not(have_naes, "NAES extracts are not distributed")
  tab <- naes_probe_table()
  expect_true(is.na(pick(tab, "NAES 2008", "d_eliminate_some_1", "codebook", "correct_after")))
  expect_equal(pick(tab, "NAES 2008", "d_eliminate_some_1", "codebook", "n"), 3817)
})

test_that("multi-mention scoring needs every named candidate in the key", {
  m <- list(5, c(5, 3), c(5, 2), 998, c(2, 4))
  expect_equal(score_mentions(m, c(1, 3, 5, 6, 7)), c("correct", "correct", "incorrect", "nonsubstantive", "incorrect"))
})

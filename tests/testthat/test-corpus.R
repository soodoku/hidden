root <- testthat::test_path("..", "..")
source(file.path(root, "R", "corpus.R"))
items <- read_items(file.path(root, "docs", "knowledge_items.csv"))

test_that("the corpus has the audited number of items per survey", {
  expect_equal(sum(items$survey == "ANES 2012"), 59)
  expect_equal(sum(items$survey == "ANES 2016"), 43)
})

test_that("items without a documentable correct answer stay out", {
  excluded <- c("ENVIR_GWGOOD", "HLTHLAW_NUM", "HLTHLAW_AMCOST", "CSES5_ECON", "ECON_ECPAST", "RETRO_PRESECON")
  expect_false(any(items$variable %in% excluded))
})

test_that("every text rule reproduces the hand coding", {
  derived <- derive_features(items)
  for (col in setdiff(names(derived), c("survey", "variable"))) {
    off <- items$variable[items[[col]] != derived[[col]]]
    expect(length(off) == 0, paste0(col, " differs from its rule for: ", paste(off, collapse = ", ")))
  }
})

test_that("the rules fire on wording that should trigger them", {
  planted <- items[1, ]
  planted$question <- "Would you say, in your personal opinion, do you think X? If you don't know, just say so."
  planted$interviewer_instructions <- "{PROBE DK WITH 'WHAT IS YOUR BEST GUESS?'}"
  planted$response_options <- "(open: text entry)"
  d <- derive_features(planted)
  expect_equal(unname(unlist(d[c("dyt", "wwys", "wipo", "dke_preamble", "dk_probe", "explicit_dk")])), rep(1L, 6))
  expect_equal(d$n_options, "open")
})

test_that("the self-placement DK filter precedes exactly the 7-point placements", {
  expected <- as.integer(items$topic == "placement" & items$n_options == "7")
  expect_equal(items$self_placement_dk_filter, expected)
})

test_that("Wilson bounds match prop.test without continuity correction", {
  ref <- suppressWarnings(stats::prop.test(9, 102, correct = FALSE)$conf.int)
  expect_equal(unname(unlist(wilson(9, 102))), as.numeric(ref))
  expect_equal(wilson(0, 59)$lower, 0)
})

test_that("the table is internally consistent", {
  tab <- feature_table(items)
  expect_true(all(tab$lower <= tab$share & tab$share <= tab$upper))
  expect_equal(unique(tab$n[tab$survey == "Combined"]), nrow(items))
  opts <- tab[startsWith(tab$feature, "options_"), ]
  expect_equal(as.vector(tapply(opts$share, opts$survey, sum)), rep(1, 3))
})

root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
old_wd <- setwd(root)
withr::defer(setwd(old_wd), testthat::teardown_env())
for (f in list.files("R", full.names = TRUE)) source(f, local = TRUE)

panel <- dplyr::bind_rows(read_alumni(), read_staff())
mturk <- read_mturk()

test_that("samples have the documented sizes", {
  expect_equal(sum(panel$study == "alumni"), 339)
  expect_equal(sum(panel$study == "staff"), 173)
  expect_equal(nrow(mturk), 1059)
})

test_that("every respondent answers each identification item once and codes partition responses", {
  ids <- identification(panel, id_items_2010, 2010)
  expect_equal(nrow(ids), nrow(panel) * nrow(id_items_2010))
  shares <- identification_shares(ids)
  expect_equal(shares$correct + shares$partial + shares$incorrect + shares$dk, rep(1, nrow(shares)))
})

# A second route: where the new rules and the draft's keyword rules should
# agree, the numbers must match the draft's tables.
test_that("lenient coding reproduces the draft's Sarkozy figures for alumni", {
  shares <- identification_shares(identification(read_alumni(), id_items_2010, 2010))
  lenient <- \(cue) with(shares[shares$item == "sarkozy" & shares$cue == cue, ], correct + partial)
  expect_equal(round(lenient("name"), 3), 0.848)
  expect_equal(round(lenient("photo"), 3), 0.310)
})

test_that("multiple-choice shares reproduce the draft for Robert Gates among alumni", {
  gates <- mc_scale_summary(mc_vs_scale_2010(read_alumni())) |> dplyr::filter(item == "gates")
  expect_equal(round(gates$mc_correct, 3), 0.500)
  expect_equal(round(gates$mc_corrected, 3), 0.481)
})

test_that("the 2010 multiple-choice probe went only to blank open-ended answers", {
  probe <- mc_probe_2010(panel)
  expect_true(all(is.na(probe$answer[!is.na(probe$mc_outcome)])))
})

test_that("reasons are tabulated only for respondents who were asked", {
  r <- reasons(mturk)
  expect_equal(sum(r$item == "bush_deficit"), sum(mturk$probe == "closed"))
  expect_equal(sum(r$item == "travel_ban"), sum(mturk$probe == "closed" & mturk$reticence_guessing == "closed"))
})

root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
old_wd <- setwd(root)
withr::defer(setwd(old_wd), testthat::teardown_env())
for (f in list.files("R", full.names = TRUE)) source(f, local = TRUE)

codes <- read_open_codes()
expect_codes <- function(answers, item, year, expected) {
  expect_equal(code_open(answers, item, year, codes), expected, label = paste(item, answers, collapse = " / "))
}

test_that("offices are correct, the institution alone partial, other offices incorrect", {
  expect_codes(
    c("Senate Majority Leader", "senator from nevada", "house majority leader", "senator or representative"),
    "reid", 2010, c("correct", "partial", "incorrect", "partial")
  )
  expect_codes(
    c("senate majority whip", "senate minority leader", "Majority leader of the U.S. Senate"),
    "mcconnell", 2017, c("partial", "partial", "correct")
  )
  expect_codes(
    c("house minority leader (and former speaker)", "speaker of the house", "senate minority leader"),
    "pelosi", 2017, c("correct", "partial", "incorrect")
  )
  expect_codes(c("cheif justice", "supreme court justice", "senator"), "roberts", 2017, c(
    "correct", "partial",
    "incorrect"
  ))
})

test_that("hedges that include the right office count, misspellings count", {
  expect_codes(c("president or prime minister of france", "prime minister of france"), "sarkozy", 2010, c(
    "correct",
    "partial"
  ))
  expect_codes(c("russian presidant", "russian prime minister", "president of ruussia"), "putin", 2017, c(
    "correct",
    "partial", "correct"
  ))
  expect_codes(c("german chancelor", "german president", "chancellor"), "merkel", 2017, c(
    "correct", "partial",
    "correct"
  ))
  expect_codes(c("head of home security", "secretary of education"), "napolitano", 2010, c("correct", "partial"))
})

test_that("blanks and professed ignorance are dk", {
  expect_codes(c(NA, "", "?", "I don't know", "no idea"), "merkel", 2010, rep("dk", 5))
})

test_that("general-knowledge answers follow the documented rules", {
  expect_codes(c("germany or russia", "USSR", "japan"), "ww2", 2010, c("correct", "correct", "incorrect"))
  expect_codes(
    c("heart attack", "cardiovascular disease", "hypertension", "breast cancer"), "women_death", 2010,
    c("correct", "correct", "incorrect", "incorrect")
  )
})

test_that("an item without a rule fails loudly", {
  expect_error(code_open("x", "biden", 2010, codes), "No coding rule")
})

test_that("the guessing correction removes the expected lucky hits", {
  # With four options, 30 right and 30 wrong out of 100 imply 10 lucky right answers.
  outcome <- rep(c("correct", "incorrect", "dk"), c(30, 30, 40))
  expect_equal(mean(corrected_score(outcome, 4)), 0.2)
  # Pure random guessing among k options has an expected corrected score of zero.
  expect_equal(corrected_score("correct", 5) * 1 / 5 + corrected_score("incorrect", 5) * 4 / 5, 0)
})

test_that("scale knowledge requires certainty and a unique top rating", {
  ratings <- tibble::tibble(right = c(10, 10, 8, NA), w1 = c(3, 10, 2, 5), w2 = c(NA, 0, 1, 1))
  out <- scale_outcomes(ratings, "right", c("w1", "w2"))
  expect_equal(out$known, c(TRUE, FALSE, FALSE, FALSE))
  expect_equal(out$top, c(TRUE, FALSE, TRUE, FALSE))
})

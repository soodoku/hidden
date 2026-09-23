root <- normalizePath(file.path(testthat::test_path(), "..", ".."))

read_all <- \(f) {
  readr::read_csv(f, col_types = readr::cols(.default = "c"), show_col_types = FALSE, name_repair = "unique_quiet")
}

test_that("the released data carry no direct identifiers", {
  files <- list.files(file.path(root, "data", "raw"), pattern = "\\.csv$", full.names = TRUE)
  for (f in files) {
    cells <- f |>
      read_all() |>
      # Browser version strings (e.g. 37.0.0.0) look like IP addresses.
      dplyr::select(-dplyr::matches("Version$")) |>
      unlist(use.names = FALSE) |>
      stats::na.omit()
    has <- \(pattern) any(stringr::str_detect(cells, pattern))
    expect_false(has("(?<![\\d.])(?:\\d{1,3}\\.){3}\\d{1,3}(?![\\d.])"), label = paste(basename(f), "IP address"))
    expect_false(has("[\\w.+-]+@[\\w-]+\\.[A-Za-z]{2,}"), label = paste(basename(f), "email"))
  }
  blanked <- c(
    "ip.address", "ipaddress", "lat", "long", "zip", "city", "postal", "postal_code", "IPAddress",
    "LocationLatitude", "LocationLongitude", "born.ctry", "born.city", "born.us.city", "occ", "occ2",
    "major", "ethnic2", "affiliate.oth2", "other.race2"
  )
  for (f in files) {
    data <- read_all(f)
    for (column in intersect(blanked, names(data))) {
      values <- data[[column]]
      if (grepl("mturk", f)) values <- values[-(1:2)]
      expect_true(all(is.na(values)), label = paste(basename(f), column, "is blank"))
    }
  }
})

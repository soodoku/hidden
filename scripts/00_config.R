project_root <- rprojroot::find_root(rprojroot::has_file("DESCRIPTION"))
project_file <- function(...) file.path(project_root, ...)
raw_dir <- project_file("data", "raw")
derived_dir <- project_file("data", "derived")
table_dir <- project_file("tabs")
figure_dir <- project_file("figs")
prepared_data_file <- file.path(derived_dir, "prepared_data.rds")
naes_summary_file <- project_file("docs", "naes_probe.csv")
figure_sizes <- list(cue = c(width = 6.5, height = 5.4), formats = c(width = 6.5, height = 7))
figure_dpi <- 180
reference_colour <- "grey60"
cue_colours <- c(`FALSE` = "grey20", `TRUE` = "#08519C")
table_style <- list(font_size = "normalsize", column_padding = "6pt", row_stretch = 1)

raw_files <- c(
  alumni_2010.csv = "7baf68c339ab36d99eac7ff018c6c4e1179b13443dd8450f1d0d1c6b13ab6c82",
  alumni_2010_demographics.csv = "2d8df024fa03a009dada14b1c2d7b5cee4f26457936a485dfa6fd687e65d501a",
  staff_2010.csv = "891f04e9cfe768d0b666a3133a23da05478b90c4a6653285fe197d263e7fb5c2",
  staff_2010_demographics.csv = "1ccc085332361fcd405fa0d1823e1f7ebe5779e09542833086af3c1cc2896a18",
  mturk_march_2017.csv = "845864d3861868f27a781aa3ed5875e77b5d8587f216eff22930416521808f02"
)

study_labels <- c(alumni = "Alumni, 2010", staff = "Staff, 2010", mturk = "MTurk, 2017")

item_labels <- c(
  sarkozy = "Nicolas Sarkozy", napolitano = "Janet Napolitano", reid = "Harry Reid",
  merkel = "Angela Merkel", mcconnell = "Mitch McConnell", schumer = "Chuck Schumer",
  putin = "Vladimir Putin", roberts = "John Roberts", pelosi = "Nancy Pelosi",
  ww2 = "Most World War II casualties", women_death = "Leading cause of death, U.S. women",
  reagan = "Reagan, 1981--88", clinton = "Clinton, 1993--2000", bush = "G. W. Bush, 2001--08",
  gates = "Robert Gates (photo)", breyer = "Stephen Breyer (photo)",
  aca_payroll = "ACA raises Medicare payroll tax", aca_providers = "ACA limits Medicare provider payments",
  travel_ban = "Travel ban bars entry", merkel_name = "Merkel's office, by name",
  merkel_photo = "Merkel's office, by photo", mcconnell_name = "McConnell's office, by name",
  mcconnell_photo = "McConnell's office, by photo", bush_deficit = "Deficit rose under G. W. Bush",
  pooled = "Pooled"
)

probe_labels <- c(
  lott = "Trent Lott", rehnquist = "William Rehnquist", blair = "Tony Blair", reno = "Janet Reno",
  hastert = "Dennis Hastert", cheney = "Dick Cheney", pelosi = "Nancy Pelosi", brown = "Gordon Brown",
  roberts = "John Roberts",
  cut_permanent = "Who favors making the tax cuts permanent",
  kerry_income = "Income above which Kerry would repeal the cuts",
  repeal_wealthy = "Who would repeal the cuts for the wealthy",
  repeal_all = "Who would repeal all the cuts",
  r_opposed_cuts_1 = "Republican who opposed the cuts (4 names)",
  r_opposed_cuts_2 = "Republican who opposed the cuts (2 names)",
  d_eliminate_some_2 = "Democrat who would end cuts above an income",
  eliminate_some = "Candidate who would end cuts above an income",
  d_working_families = "Democrat promising a working-family tax cut"
)

measure_labels <- c(
  scale_known = "Confidence scale: certain and ranked first",
  mc_corrected = "Multiple choice, corrected for guessing", mc_correct = "Multiple choice"
)
measure_colours <- c(mc_correct = "grey55", mc_corrected = "grey20", scale_known = "#08519C")
theme_paper <- function() {
  ggplot2::theme_minimal(base_size = 11, base_family = "sans") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"),
      plot.caption = ggplot2::element_text(hjust = 0),
      plot.title.position = "plot"
    )
}

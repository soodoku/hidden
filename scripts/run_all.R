purrr::walk(list.files("R", full.names = TRUE), source)

verify_sources()
panel <- dplyr::bind_rows(read_alumni(), read_staff())
mturk <- read_mturk()

dir.create("tabs", showWarnings = FALSE)
write_output <- \(x, name) readr::write_csv(x, file.path("tabs", name), na = "")

ids <- dplyr::bind_rows(identification(panel, id_items_2010, 2010), identification(mturk, id_items_2017, 2017))
ids |>
  dplyr::count(study, item, cue, code, answer = normalize_answer(answer), name = "responses") |>
  dplyr::arrange(item, code, dplyr::desc(responses)) |>
  write_output("open_answers.csv")
write_output(identification_shares(ids), "identification_shares.csv")
write_output(cue_effects(ids), "cue_effects.csv")

forms <- open_closed(panel)
dplyr::bind_rows(
  forms |>
    dplyr::summarise(
      n = dplyr::n(), correct = mean(correct), dk = mean(dk), corrected = mean(corrected),
      .by = c(study, item, form)
    ),
  purrr::map(c("correct", "corrected"), \(m) {
    dplyr::bind_rows(
      arm_contrast(forms, m, "form", c("open", "closed")),
      pooled_contrast(forms, m, "form", c("open", "closed"))
    ) |>
      dplyr::mutate(measure = m)
  }) |>
    purrr::list_rbind()
) |>
  write_output("open_closed.csv")

dplyr::bind_rows(probe_gains(mc_probe_2010(panel)), probe_gains(mc_probe_2017(mturk))) |>
  write_output("probe_gains.csv")

deficits <- dk_orientation(panel)
dplyr::bind_rows(
  deficits |>
    dplyr::summarise(
      n = dplyr::n(), correct = mean(correct), dk = mean(dk), corrected = mean(corrected),
      .by = c(study, item, dk_arm)
    ),
  purrr::map(c("correct", "dk", "corrected"), \(m) {
    dplyr::bind_rows(
      arm_contrast(deficits, m, "dk_arm", c("encouraging", "discouraging")),
      pooled_contrast(deficits, m, "dk_arm", c("encouraging", "discouraging"))
    ) |>
      dplyr::mutate(measure = m)
  }) |>
    purrr::list_rbind()
) |>
  write_output("dk_orientation.csv")

dplyr::bind_rows(mc_vs_scale_2010(panel), mc_vs_scale_2017(mturk)) |>
  mc_scale_summary() |>
  write_output("mc_vs_scale.csv")

write_output(reason_shares(reasons(mturk)), "reasons.csv")
write_output(feature_table(read_items()), "knowledge_item_features.csv")

# The NAES files may not be redistributed, so the committed summary in docs/
# stands in for them when they are absent.
write_output(anes_probe_table(), "anes_probe.csv")
if (file.exists(raw_path("naes2004_tax_probe.csv"))) {
  readr::write_csv(naes_probe_table(), file.path("docs", "naes_probe.csv"), na = "")
}

dplyr::bind_rows(
  dplyr::count(panel, study, arm = "all"),
  dplyr::count(mturk, study, arm = "all")
) |>
  write_output("samples.csv")

verify_sources()
have_naes <- file.exists(raw_path("naes2004_tax_probe.csv"))
prepared <- list(
  panel = dplyr::bind_rows(read_alumni(), read_staff()),
  mturk = read_mturk(),
  knowledge_items = read_items(),
  anes = anes_long(),
  naes = if (have_naes) naes_long() else NULL,
  naes_summary = if (!have_naes) readr::read_csv(naes_summary_file, show_col_types = FALSE) else NULL
)
saveRDS(prepared, prepared_data_file)

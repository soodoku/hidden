purrr::walk(list.files("R", full.names = TRUE), source)

p0 <- \(x) dplyr::if_else(is.na(x), "--", formatC(abs(round(100 * x)), format = "f", digits = 0))
signed <- \(x) dplyr::if_else(is.na(x), "--", sub("^-", "$-$", formatC(round(100 * x) + 0, format = "f", digits = 0)))
with_se <- \(est, se) dplyr::if_else(is.na(est), "--", paste0(signed(est), " (", p0(se), ")"))
label_item <- \(x) unname(item_labels[x])
label_study <- \(x) unname(study_labels[x])
blank_repeats <- \(x) dplyr::if_else(x == dplyr::lag(x, default = ""), "", x)

# Identification by name and by photo.
shares <- read_tab("identification_shares.csv")
effects <- read_tab("cue_effects.csv")
cue_rows <- effects |>
  dplyr::filter(measure %in% c("correct", "lenient")) |>
  dplyr::select(study, item, measure, difference, std_error) |>
  tidyr::pivot_wider(names_from = measure, values_from = c(difference, std_error)) |>
  dplyr::left_join(
    shares |>
      dplyr::select(study, item, cue, correct, n) |>
      tidyr::pivot_wider(names_from = cue, values_from = c(correct, n)),
    by = c("study", "item")
  ) |>
  dplyr::mutate(study = factor(study, names(study_labels)), item = factor(item, names(item_labels))) |>
  dplyr::arrange(study, item) |>
  dplyr::transmute(
    Sample = blank_repeats(label_study(as.character(study))),
    Figure = label_item(as.character(item)),
    Name = p0(correct_name), Photo = p0(correct_photo),
    Difference = with_se(difference_correct, std_error_correct),
    Lenient = with_se(difference_lenient, std_error_lenient)
  )
write_table(cue_rows, "tabs/cue.tex", "llrrrr", c(
  "Sample", "Public figure", "Name", "Photo", "Photo $-$ name",
  "Lenient"
))

# Open-ended versus multiple choice.
forms <- read_tab("open_closed.csv")
form_levels <- forms |>
  dplyr::filter(!is.na(form)) |>
  dplyr::select(study, item, form, correct, dk, corrected, n) |>
  tidyr::pivot_wider(names_from = form, values_from = c(correct, dk, corrected, n))
form_diff <- forms |>
  dplyr::filter(is.na(form)) |>
  dplyr::select(study, item, measure, difference, std_error) |>
  tidyr::pivot_wider(names_from = measure, values_from = c(difference, std_error))
form_rows <- dplyr::full_join(form_levels, form_diff, by = c("study", "item")) |>
  dplyr::mutate(study = factor(study, names(study_labels)), item = factor(item, names(item_labels))) |>
  dplyr::arrange(study, item) |>
  dplyr::transmute(
    Sample = blank_repeats(label_study(as.character(study))),
    Item = label_item(as.character(item)),
    Open = p0(correct_open), Closed = p0(correct_closed), Corrected = p0(corrected_closed),
    Raw = with_se(difference_correct, std_error_correct),
    Adjusted = with_se(difference_corrected, std_error_corrected)
  )
write_table(
  form_rows, "tabs/open_closed.tex", "llrrrrr",
  c("Sample", "Item", "Open", "MC", "MC corr.", "MC $-$ open", "Corr. $-$ open")
)

# Multiple-choice follow-ups to open-ended identification items.
probes <- read_tab("probe_gains.csv") |>
  dplyr::mutate(
    study = factor(study, names(study_labels)), item = factor(item, names(item_labels)),
    cue = factor(cue, c("name", "photo"))
  ) |>
  dplyr::arrange(study, item, cue) |>
  dplyr::transmute(
    Sample = blank_repeats(label_study(as.character(study))),
    Figure = blank_repeats(label_item(as.character(item))),
    Cue = as.character(cue),
    n = as.character(n),
    Open = p0(open_correct), Probed = p0(probed),
    Gain = with_se(gain, gain_se), Corrected = with_se(gain_corrected, gain_corrected_se),
    Combined = p0(combined_corrected)
  )
write_table(
  probes, "tabs/probes.tex", "lllrrrrrr",
  c("Sample", "Figure", "Cue", "$n$", "Open", "Asked MC", "Gain", "Corr. gain", "Open + corr.")
)

# DK-encouraging versus DK-discouraging instructions.
dk <- read_tab("dk_orientation.csv")
dk_levels <- dk |>
  dplyr::filter(!is.na(dk_arm)) |>
  dplyr::select(study, item, dk_arm, correct, dk) |>
  tidyr::pivot_wider(names_from = dk_arm, values_from = c(correct, dk))
dk_diff <- dk |>
  dplyr::filter(is.na(dk_arm)) |>
  dplyr::select(study, item, measure, difference, std_error) |>
  tidyr::pivot_wider(names_from = measure, values_from = c(difference, std_error))
dk_rows <- dplyr::full_join(dk_levels, dk_diff, by = c("study", "item")) |>
  dplyr::mutate(study = factor(study, names(study_labels)), item = factor(item, names(item_labels))) |>
  dplyr::arrange(study, item) |>
  dplyr::transmute(
    Sample = blank_repeats(label_study(as.character(study))),
    Item = label_item(as.character(item)),
    CorrectEnc = p0(correct_encouraging), CorrectDis = p0(correct_discouraging),
    DkEnc = p0(dk_encouraging), DkDis = p0(dk_discouraging),
    Correct = with_se(difference_correct, std_error_correct),
    Dk = with_se(difference_dk, std_error_dk),
    Corrected = with_se(difference_corrected, std_error_corrected)
  )
write_table(
  dk_rows, "tabs/dk.tex", "llrrrrrrr",
  c(
    "Sample", "Deficit under", "\\multicolumn{2}{c}{Correct}", "\\multicolumn{2}{c}{DK}",
    "\\multicolumn{3}{c}{Discouraging $-$ encouraging}\\\\ & & Enc. & Disc. & Enc. & Disc. & Correct & DK & Corr."
  )
)

# Multiple choice versus a confidence scale.
formats <- read_tab("mc_vs_scale.csv") |>
  dplyr::mutate(study = factor(study, names(study_labels)), item = factor(item, names(item_labels))) |>
  dplyr::arrange(study, item) |>
  dplyr::transmute(
    Sample = blank_repeats(label_study(as.character(study))),
    Item = label_item(as.character(item)),
    Mc = p0(mc_correct), McCorr = p0(mc_corrected),
    Certain = p0(certain), Known = p0(scale_known),
    Gap = with_se(gap_corrected, gap_corrected_se)
  )
write_table(
  formats, "tabs/formats.tex", "llrrrrr",
  c("Sample", "Item", "MC", "MC corr.", "Certain", "Certain, first", "MC corr. $-$ scale")
)

# Reasons given for multiple-choice answers.
reasons <- read_tab("reasons.csv") |>
  dplyr::mutate(item = factor(item, c("bush_deficit", "aca_providers", "travel_ban")), outcome = factor(
    outcome,
    c("correct", "incorrect")
  )) |>
  dplyr::arrange(item, outcome)
reason_order <- c(
  "Read, saw, or heard it", "Inferred it", "Guessed", "Felt good to think it", "Asked someone",
  "Looked it up"
)
reason_rows <- reasons |>
  dplyr::select(item, outcome, responses, reason, share) |>
  tidyr::pivot_wider(names_from = reason, values_from = share) |>
  dplyr::transmute(
    Item = blank_repeats(label_item(as.character(item))),
    Answer = as.character(outcome), n = as.character(responses),
    dplyr::across(dplyr::all_of(reason_order), p0)
  )
write_table(
  reason_rows, "tabs/reasons.tex", "llrrrrrrr",
  c("Item", "Answer", "$n$", "Heard it", "Inferred", "Guessed", "Felt good", "Asked", "Looked up")
)

# Design features of the ANES knowledge items.
features <- read_tab("knowledge_item_features.csv")
feature_labels <- c(
  explicit_dk = "Explicit DK option", dk_probe = "Interviewer probes DKs",
  srew = "Opinion-inviting wording", dyt = "\\quad ``Do you think''", wwys = "\\quad ``Would you say''",
  wipo = "\\quad ``Personal opinion''", options_2 = "2 options", options_3 = "3 options", options_4 = "4 options",
  options_5 = "5 options", options_7 = "7 options (placement scales)", options_11 = "11 options",
  options_open = "Open-ended",
  dke_preamble = "DK-encouraging preamble", dkd_preamble = "DK-discouraging preamble"
)
feature_rows <- features |>
  dplyr::filter(feature %in% names(feature_labels)) |>
  dplyr::mutate(cell = dplyr::if_else(
    survey == "Combined",
    paste0(p0(share), " [", p0(lower), ", ", p0(upper), "]"),
    p0(share)
  )) |>
  dplyr::select(feature, survey, cell) |>
  tidyr::pivot_wider(names_from = survey, values_from = cell) |>
  dplyr::mutate(feature = factor(feature, names(feature_labels))) |>
  dplyr::arrange(feature) |>
  dplyr::transmute(Feature = unname(feature_labels[as.character(feature)]), Combined, `ANES 2012`, `ANES 2016`)
write_table(
  feature_rows, "tabs/features.tex", "lrrr",
  c("Feature", paste0("Combined ($n=", nrow(read_items()), "$)"), "2012", "2016")
)

# Repeat asks of don't-know answers in the ANES and NAES.
public_probes <- dplyr::bind_rows(
  read_tab("anes_probe.csv") |> dplyr::filter(key %in% c("anes", "strict"), item != "average"),
  readr::read_csv(file.path("docs", "naes_probe.csv"), show_col_types = FALSE) |>
    dplyr::filter(key %in% c("verified", "codebook"), !is.na(correct_before)) |>
    dplyr::filter(!(item %in% c("repeal_wealthy", "repeal_all") & key == "codebook"))
)
one_decimal <- \(x) formatC(100 * x, format = "f", digits = 1)
public_rows <- public_probes |>
  dplyr::transmute(
    Survey = blank_repeats(survey),
    Item = unname(probe_labels[item]),
    n = count(n),
    Before = p0(correct_before), After = p0(correct_after),
    Gain = paste0(one_decimal(diff), " (", one_decimal(se_diff), ")"),
    DK = p0(dk_before), Converted = p0(conversion_rate)
  )
write_table(
  public_rows, "tabs/public_probes.tex", "llrrrrrr",
  c("Survey", "Item", "$n$", "Before", "After", "Gain", "DK before", "DKs converted")
)

# Numbers quoted in the text.
cue_pooled <- \(s, m) effects[effects$study == s & effects$item == "pooled" & effects$measure == m, ]
pooled_cols <- \(m) paste0(c("difference_", "std_error_"), m)
form_pooled <- \(s, m) form_diff[form_diff$study == s & form_diff$item == "pooled", pooled_cols(m)]
dk_pooled <- \(s, m) dk_diff[dk_diff$study == s & dk_diff$item == "pooled", pooled_cols(m)]
share_of <- \(s, i, c, col) shares[[col]][shares$study == s & shares$item == i & shares$cue == c]
gap_tab <- read_tab("mc_vs_scale.csv")
reason_tab <- read_tab("reasons.csv")
reason_share <- \(i, o, r) reason_tab$share[reason_tab$item == i & reason_tab$outcome == o & reason_tab$reason == r]
probe_tab <- read_tab("probe_gains.csv")
feature <- \(f, s = "Combined") features$share[features$feature == f & features$survey == s]
samples <- read_tab("samples.csv")
partial_share <- read_tab("identification_shares.csv") |>
  dplyr::summarise(partial = sum(partial * n) / sum(n), .by = study)
point_se <- \(x) c(signed(x[[1]]), p0(x[[2]]))

values <- c(
  nAlumni = samples$n[samples$study == "alumni"], nStaff = samples$n[samples$study == "staff"],
  nMturk = count(samples$n[samples$study == "mturk"]),
  cueAlumni = signed(cue_pooled("alumni", "correct")$difference), cueAlumniSe = p0(cue_pooled(
    "alumni",
    "correct"
  )$std_error),
  cueStaff = signed(cue_pooled("staff", "correct")$difference), cueStaffSe = p0(cue_pooled(
    "staff",
    "correct"
  )$std_error),
  cueMturk = signed(cue_pooled("mturk", "correct")$difference), cueMturkSe = p0(cue_pooled(
    "mturk",
    "correct"
  )$std_error),
  cueLenientAlumni = signed(cue_pooled("alumni", "lenient")$difference),
  cueLenientStaff = signed(cue_pooled("staff", "lenient")$difference),
  cueLenientMturk = signed(cue_pooled("mturk", "lenient")$difference),
  cueWrongAlumni = signed(cue_pooled("alumni", "incorrect")$difference),
  cueWrongStaff = signed(cue_pooled("staff", "incorrect")$difference),
  cueWrongMturk = signed(cue_pooled("mturk", "incorrect")$difference),
  cueDkAlumni = signed(cue_pooled("alumni", "dk")$difference),
  cueDkStaff = signed(cue_pooled("staff", "dk")$difference),
  cueDkMturk = signed(cue_pooled("mturk", "dk")$difference),
  sarkozyName = p0(share_of("alumni", "sarkozy", "name", "correct")),
  sarkozyPhoto = p0(share_of("alumni", "sarkozy", "photo", "correct")),
  robertsName = p0(share_of("mturk", "roberts", "name", "correct")),
  robertsPhoto = p0(share_of("mturk", "roberts", "photo", "correct")),
  partialAlumni = p0(partial_share$partial[partial_share$study == "alumni"]),
  partialStaff = p0(partial_share$partial[partial_share$study == "staff"]),
  partialMturk = p0(partial_share$partial[partial_share$study == "mturk"]),
  formAlumni = point_se(form_pooled("alumni", "correct"))[1], formAlumniSe = point_se(form_pooled(
    "alumni",
    "correct"
  ))[2],
  formStaff = point_se(form_pooled("staff", "correct"))[1], formStaffSe = point_se(form_pooled(
    "staff",
    "correct"
  ))[2],
  formCorrAlumni = point_se(form_pooled("alumni", "corrected"))[1],
  formCorrStaff = point_se(form_pooled("staff", "corrected"))[1],
  dkCorrectAlumni = point_se(dk_pooled("alumni", "correct"))[1], dkCorrectAlumniSe = point_se(dk_pooled(
    "alumni",
    "correct"
  ))[2],
  dkCorrectStaff = point_se(dk_pooled("staff", "correct"))[1], dkCorrectStaffSe = point_se(dk_pooled(
    "staff",
    "correct"
  ))[2],
  dkDkAlumni = point_se(dk_pooled("alumni", "dk"))[1], dkDkStaff = point_se(dk_pooled("staff", "dk"))[1],
  probeGainMax = p0(max(probe_tab$gain_corrected[probe_tab$study != "mturk"])),
  probeGainMin = p0(min(probe_tab$gain_corrected[probe_tab$study != "mturk"])),
  probeMturkMin = p0(min(probe_tab$gain_corrected[probe_tab$study == "mturk"])),
  probeMturkMax = p0(max(probe_tab$gain_corrected[probe_tab$study == "mturk"])),
  gapPanel = p0(mean(gap_tab$gap_corrected[gap_tab$study != "mturk"])),
  gapMturkPolicy = p0(mean(gap_tab$gap_corrected[gap_tab$study == "mturk" & gap_tab$item %in% c(
    "aca_payroll",
    "aca_providers", "travel_ban"
  )])),
  travelMc = p0(gap_tab$mc_correct[gap_tab$item == "travel_ban"]),
  travelCertain = p0(gap_tab$certain[gap_tab$item == "travel_ban"]),
  travelKnown = p0(gap_tab$scale_known[gap_tab$item == "travel_ban"]),
  heardBush = p0(reason_share("bush_deficit", "correct", "Read, saw, or heard it")),
  heardAca = p0(reason_share("aca_providers", "correct", "Read, saw, or heard it")),
  heardBan = p0(reason_share("travel_ban", "correct", "Read, saw, or heard it")),
  heardBushWrong = p0(reason_share("bush_deficit", "incorrect", "Read, saw, or heard it")),
  inferAca = p0(reason_share("aca_providers", "correct", "Inferred it")),
  guessAca = p0(reason_share("aca_providers", "correct", "Guessed")),
  lookedBush = p0(reason_share("bush_deficit", "correct", "Looked it up") + reason_share(
    "bush_deficit", "correct",
    "Asked someone"
  )),
  nItems = sum(features$n[features$feature == "explicit_dk" & features$survey != "Combined"]),
  nItemsTwelve = features$n[features$feature == "explicit_dk" & features$survey == "ANES 2012"],
  nItemsSixteen = features$n[features$feature == "explicit_dk" & features$survey == "ANES 2016"],
  featProbe = p0(feature("dk_probe")), featOpen = p0(feature("options_open")),
  featOpinion = p0(feature("srew")), featSeven = p0(feature("options_7")),
  featSelfFilter = p0(feature("self_placement_dk_filter")),
  anesGainMax = p0(max(public_probes$diff[startsWith(public_probes$survey, "ANES")])),
  anesGainMin = p0(min(public_probes$diff[startsWith(public_probes$survey, "ANES")])),
  naesGainMax = p0(max(public_probes$diff[startsWith(public_probes$survey, "NAES")])),
  anesConvertMax = p0(max(public_probes$conversion_rate[startsWith(public_probes$survey, "ANES")])),
  anesConvertMedian = p0(median(public_probes$conversion_rate[startsWith(public_probes$survey, "ANES")])),
  naesConvertMax = p0(max(public_probes$conversion_rate[startsWith(public_probes$survey, "NAES")]))
)
write_macros(values, "tabs/macros.tex")

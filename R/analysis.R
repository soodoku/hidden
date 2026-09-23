# A respondent who picks one of k substantive options at random is right with
# probability 1 / k, so each wrong pick implies 1 / (k - 1) lucky right ones.
# The score is 1 for a right answer, -1 / (k - 1) for a wrong one, 0 for DK.
corrected_score <- function(outcome, k) {
  dplyr::case_when(outcome == "correct" ~ 1, outcome == "incorrect" ~ -1 / (k - 1), .default = 0)
}

outcome_of <- function(response, correct, dk) {
  dplyr::case_when(
    is.na(response) | response %in% dk ~ "dk",
    response %in% correct ~ "correct",
    .default = "incorrect"
  )
}

# Each respondent sees one arm, so arms are independent samples; a pooled
# contrast across items keeps item fixed effects and clusters by respondent.
arm_contrast <- function(data, outcome, arm, levels) {
  data |>
    dplyr::filter(.data[[arm]] %in% levels) |>
    dplyr::summarise(
      p = mean(.data[[outcome]]), v = var(.data[[outcome]]), n = dplyr::n(),
      .by = c(study, item, dplyr::all_of(arm))
    ) |>
    tidyr::pivot_wider(names_from = dplyr::all_of(arm), values_from = c(p, v, n)) |>
    dplyr::transmute(
      study, item,
      !!levels[1] := .data[[paste0("p_", levels[1])]],
      !!levels[2] := .data[[paste0("p_", levels[2])]],
      difference = .data[[paste0("p_", levels[2])]] - .data[[paste0("p_", levels[1])]],
      std_error = sqrt(
        .data[[paste0("v_", levels[1])]] / .data[[paste0("n_", levels[1])]] +
          .data[[paste0("v_", levels[2])]] / .data[[paste0("n_", levels[2])]]
      ),
      !!paste0("n_", levels[1]) := .data[[paste0("n_", levels[1])]],
      !!paste0("n_", levels[2]) := .data[[paste0("n_", levels[2])]]
    )
}

pooled_contrast <- function(data, outcome, arm, levels) {
  data |>
    dplyr::filter(.data[[arm]] %in% levels) |>
    dplyr::mutate(treated = .data[[arm]] == levels[2]) |>
    dplyr::group_by(study) |>
    dplyr::group_modify(\(d, key) {
      form <- if (dplyr::n_distinct(d$item) > 1) {
        reformulate(
          c("treated", "item"),
          outcome
        )
      } else {
        reformulate("treated", outcome)
      }
      fit <- lm(form, data = d)
      se <- sqrt(diag(sandwich::vcovCL(fit, cluster = d$respondent, type = "HC1")))
      tibble::tibble(
        item = "pooled", difference = unname(coef(fit)["treatedTRUE"]),
        std_error = unname(se["treatedTRUE"]),
        respondents = dplyr::n_distinct(d$respondent)
      )
    }) |>
    dplyr::ungroup()
}

id_items_2010 <- tibble::tribble(
  ~item, ~name, ~photo,
  "sarkozy", "ns", "ns2",
  "napolitano", "jn", "jn2",
  "reid", "hr", "hr2",
  "merkel", "am1", "am2",
  "mcconnell", "mm1", "mm2"
)

id_items_2017 <- tibble::tribble(
  ~item, ~name, ~photo,
  "mcconnell", "vt_mmc", "vp_mmc",
  "schumer", "vt_cs", "vp_js",
  "merkel", "vt_am", "vp_am",
  "putin", "vt_vp", "vp_vp",
  "roberts", "vt_jr", "vp_jr",
  "pelosi", "vt_np", "vp_np"
)

# One row per respondent and public figure: the answer to the name or photo
# version, whichever the respondent was randomly assigned.
identification <- function(data, items, year) {
  purrr::pmap(items, \(item, name, photo) {
    data |>
      dplyr::transmute(
        study, respondent, cue,
        item = item,
        answer = dplyr::if_else(cue == "photo", .data[[photo]], .data[[name]])
      )
  }) |>
    purrr::list_rbind() |>
    dplyr::mutate(code = code_open(answer, dplyr::first(item), year), .by = item) |>
    dplyr::mutate(
      correct = code == "correct",
      lenient = code %in% c("correct", "partial"),
      dk = code == "dk",
      incorrect = code == "incorrect"
    )
}

identification_shares <- function(ids) {
  ids |>
    dplyr::summarise(
      n = dplyr::n(),
      correct = mean(correct), partial = mean(code == "partial"),
      incorrect = mean(incorrect), dk = mean(dk),
      .by = c(study, item, cue)
    )
}

cue_effects <- function(ids) {
  c("correct", "lenient", "incorrect", "dk") |>
    purrr::map(\(m) {
      dplyr::bind_rows(
        arm_contrast(ids, m, "cue", c("name", "photo")),
        pooled_contrast(ids, m, "cue", c("name", "photo"))
      ) |>
        dplyr::mutate(measure = m)
    }) |>
    purrr::list_rbind()
}

# Which country suffered the most World War II casualties, and the leading
# cause of death among U.S. women, each asked open-ended or as multiple choice.
open_closed <- function(panel) {
  items <- tibble::tribble(
    ~item, ~open, ~closed, ~key, ~k,
    "ww2", "ww2.cas2", "ww2.cas", "[Erstwhile] Soviet Union", 5,
    "women_death", "fem.death2", "fem.death", "Heart Disease", 4
  )
  purrr::pmap(items, \(item, open, closed, key, k) {
    this_item <- item
    panel |>
      dplyr::filter(question_form %in% c("open", "closed")) |>
      dplyr::transmute(
        study, respondent,
        form = question_form, item = item,
        outcome = dplyr::if_else(
          form == "open",
          code_open(.data[[open]], this_item, 2010),
          outcome_of(.data[[closed]], key, c("Couldn't say", "Don't Know"))
        ),
        corrected = dplyr::if_else(form == "open", as.numeric(outcome == "correct"), corrected_score(outcome, k))
      )
  }) |>
    purrr::list_rbind() |>
    dplyr::mutate(correct = outcome == "correct", dk = outcome == "dk")
}

# 2010: only respondents who left the open-ended item blank were shown the
# multiple-choice version. 2017: everyone in the multiple-choice arm saw it,
# right after the open-ended block.
mc_probe_2010 <- function(panel) {
  items <- tibble::tribble(
    ~item, ~name_mc, ~photo_mc, ~key,
    "merkel", "am11", "am21", "Chancellor of Germany",
    "mcconnell", "mm11", "mm21", "Senate Minority Leader"
  )
  ids <- identification(panel, dplyr::filter(id_items_2010, item %in% items$item), 2010)
  purrr::pmap(items, \(item, name_mc, photo_mc, key) {
    panel |>
      dplyr::transmute(
        study, respondent, cue,
        item = item,
        mc = dplyr::if_else(cue == "photo", .data[[photo_mc]], .data[[name_mc]]),
        mc_outcome = dplyr::if_else(is.na(mc), NA_character_, outcome_of(mc, key, "Don't Know"))
      )
  }) |>
    purrr::list_rbind() |>
    dplyr::left_join(dplyr::select(ids, study, respondent, item, answer, code), by = c(
      "study", "respondent",
      "item"
    )) |>
    assertr::verify(is.na(mc_outcome) | is.na(answer))
}

mc_probe_2017 <- function(mturk) {
  items <- tibble::tribble(
    ~item, ~name_mc, ~photo_mc, ~key,
    "merkel", "ftc_am", "fvc_am", "Chancellor of Germany",
    "mcconnell", "ftc_mmc", "fvc_mmc", "U.S. Senate Majority Leader"
  )
  ids <- identification(mturk, dplyr::filter(id_items_2017, item %in% items$item), 2017)
  closed <- dplyr::filter(mturk, fugitive_visual == "closed")
  purrr::pmap(items, \(item, name_mc, photo_mc, key) {
    closed |>
      dplyr::transmute(
        study, respondent, cue,
        item = item,
        mc = dplyr::if_else(cue == "photo", .data[[photo_mc]], .data[[name_mc]]),
        mc_outcome = outcome_of(mc, key, "Don't know")
      )
  }) |>
    purrr::list_rbind() |>
    dplyr::left_join(dplyr::select(ids, study, respondent, item, answer, code), by = c("study", "respondent", "item"))
}

# Knowledge the open-ended item missed: respondents who did not answer it
# correctly but chose the right option afterward. Every item has four
# substantive options.
probe_gains <- function(probe) {
  probe |>
    dplyr::mutate(
      missed = code != "correct",
      gain = missed & mc_outcome %in% "correct",
      gain_corrected = dplyr::if_else(missed & !is.na(mc_outcome), corrected_score(mc_outcome, 4), 0),
      probed = !is.na(mc_outcome) & missed
    ) |>
    dplyr::summarise(
      n = dplyr::n(),
      open_correct = mean(code == "correct"),
      probed = mean(probed),
      probe_correct_rate = sum(gain) / sum(probed),
      gain_se = sd(gain) / sqrt(n),
      gain_corrected_se = sd(gain_corrected) / sqrt(n),
      gain = mean(gain),
      gain_corrected = mean(gain_corrected),
      .by = c(study, item, cue)
    ) |>
    dplyr::mutate(combined_corrected = open_correct + gain_corrected)
}

deficit_items <- tibble::tribble(
  ~item, ~encouraging, ~discouraging, ~key,
  "reagan", "reagan", "reagan2", "Increased",
  "clinton", "clinton", "clinton2", "Decreased",
  "bush", "wbush", "wbush2", "Increased"
)

# The Obama item ("between 2009 and 2010 ... the federal deficit") is keyed
# Increased, but the deficit fell from $1.41 trillion in fiscal 2009 to $1.29
# trillion in fiscal 2010, so it is not scored.
dk_orientation <- function(panel) {
  purrr::pmap(deficit_items, \(item, encouraging, discouraging, key) {
    panel |>
      dplyr::filter(dk_arm %in% c("encouraging", "discouraging")) |>
      dplyr::transmute(
        study, respondent, dk_arm,
        item = item,
        response = dplyr::if_else(dk_arm == "encouraging", .data[[encouraging]], .data[[discouraging]]),
        outcome = outcome_of(response, key, "Don't Know"),
        correct = outcome == "correct",
        dk = outcome == "dk",
        corrected = corrected_score(outcome, 3)
      )
  }) |>
    purrr::list_rbind()
}

mc_scale_items_2010 <- tibble::tribble(
  ~item, ~mc, ~key, ~dk, ~right, ~wrong,
  "gates", "rg", "Robert Gates", "Don't Know", "rg.rg", "rg.ad,rg.jc,rg.ak",
  "breyer", "sb", "Stephen Breyer", "Couldn't Say", "sb.sb", "sb.jb,sb.ks,sb.tv",
  "aca_payroll", "healthcare.pk1", "Increases the Medicare payroll tax for upper‐income Americans",
  "Couldn't say", "upper.class", "illegal,single.payer,mammograms",
  "aca_providers", "healthbill", "Limits future increases in payments to Medicare providers",
  "Couldn't say", "future.increase", "death.panel,medicare,cuts.benefits"
)

# On the 0-10 scale, knowing means being certain the right option is true (a
# 10) and rating it above every wrong option; a respondent who also gives a
# wrong option 10 does not know which is right. A skipped rating counts as no
# certainty, as a skipped multiple-choice item counts as DK.
scale_outcomes <- function(data, right, wrong) {
  ratings <- dplyr::select(data, dplyr::all_of(c(right, wrong)))
  right_rating <- dplyr::pull(ratings, 1)
  top_wrong <- do.call(pmax, c(unname(as.list(ratings[, -1])), na.rm = TRUE))
  top <- dplyr::coalesce(right_rating > dplyr::coalesce(top_wrong, -1), FALSE)
  certain <- dplyr::coalesce(right_rating == 10, FALSE)
  tibble::tibble(known = top & certain, certain = certain, top = top)
}

mc_vs_scale_2010 <- function(panel) {
  purrr::pmap(mc_scale_items_2010, \(item, mc, key, dk, right, wrong) {
    wrong <- strsplit(wrong, ",")[[1]]
    mc_arm <- panel |>
      dplyr::filter(scale_arm == "mc") |>
      dplyr::transmute(
        study, respondent,
        arm = "mc", item = item,
        outcome = outcome_of(.data[[mc]], key, dk),
        known = outcome == "correct",
        corrected = corrected_score(outcome, 4)
      )
    scale_arm <- panel |>
      dplyr::filter(scale_arm == "scale") |>
      dplyr::mutate(dplyr::across(dplyr::all_of(c(right, wrong)), parse_rating))
    scale_arm <- dplyr::bind_cols(
      dplyr::select(scale_arm, study, respondent),
      scale_outcomes(scale_arm, right, wrong)
    ) |>
      dplyr::transmute(study, respondent, arm = "scale", item = item, known, certain, top)
    dplyr::bind_rows(mc_arm, scale_arm)
  }) |>
    purrr::list_rbind()
}

parse_percent_scale <- function(x) {
  value <- suppressWarnings(as.numeric(x))
  dplyr::if_else(value >= 0 & value <= 100, value / 10, NA_real_)
}

# In 2017 the policy items used a 0-10 scale; the office items, which followed
# the open-ended block for the same public figure, used 0-100.
mc_vs_scale_2017 <- function(mturk) {
  policy <- tibble::tribble(
    ~item, ~open_probe, ~closed_probe, ~key, ~right, ~wrong,
    "aca_payroll", "rgc_o_aca", "rgc_c_aca", "Increase the Medicare payroll tax for upper-income Americans",
    "rg_s_aca_3", "rg_s_aca_1,rg_s_aca_2,rg_s_aca_4",
    "aca_providers", "rgc_o_aca2", "rgc_c_aca2", "Limit future increases in payments to Medicare providers",
    "rg_s_aca2_3", "rg_s_aca2_1,rg_s_aca2_2,rg_s_aca2_4",
    "travel_ban", "rgc_o_dt", "rgc_c_dt", "Temporarily ban immigrants from several majority-Muslim countries",
    "rg_s_dt_4", "rg_s_dt_1,rg_s_dt_2,rg_s_dt_3"
  )
  policy_rows <- purrr::pmap(policy, \(item, open_probe, closed_probe, key, right, wrong) {
    wrong <- strsplit(wrong, ",")[[1]]
    mc_arm <- mturk |>
      dplyr::filter(reticence_guessing == "closed") |>
      dplyr::transmute(
        study, respondent, cue,
        arm = "mc", item = item,
        outcome = outcome_of(dplyr::coalesce(.data[[open_probe]], .data[[closed_probe]]), key, "Don’t know"),
        known = outcome == "correct",
        corrected = corrected_score(outcome, 4)
      )
    scale_arm <- dplyr::filter(mturk, reticence_guessing == "scale") |>
      dplyr::mutate(dplyr::across(dplyr::all_of(c(right, wrong)), parse_rating))
    scale_arm <- dplyr::bind_cols(
      dplyr::select(scale_arm, study, respondent, cue),
      scale_outcomes(scale_arm, right, wrong)
    ) |>
      dplyr::transmute(study, respondent, cue, arm = "scale", item = item, known, certain, top)
    dplyr::bind_rows(mc_arm, scale_arm)
  })
  office <- tibble::tribble(
    ~item, ~stem, ~key,
    "merkel", "am", "Chancellor of Germany",
    "mcconnell", "mmc", "U.S. Senate Majority Leader"
  )
  office_rows <- purrr::pmap(office, \(item, stem, key) {
    mc_arm <- mturk |>
      dplyr::filter(fugitive_visual == "closed") |>
      dplyr::transmute(
        study, respondent, cue,
        arm = "mc", item = paste(item, cue, sep = "_"),
        response = dplyr::if_else(cue == "photo", .data[[paste0("fvc_", stem)]], .data[[paste0("ftc_", stem)]]),
        outcome = outcome_of(response, key, "Don't know"),
        known = outcome == "correct",
        corrected = corrected_score(outcome, 4)
      ) |>
      dplyr::select(-response)
    scale_arm <- dplyr::filter(mturk, fugitive_visual == "asses")
    ratings <- purrr::map(1:4, \(i) {
      parse_percent_scale(dplyr::if_else(
        scale_arm$cue == "photo",
        scale_arm[[paste0("fvs_", stem, "_", i)]],
        scale_arm[[paste0("fts_", stem, "_", i)]]
      ))
    }) |>
      rlang::set_names(paste0("option_", 1:4)) |>
      tibble::as_tibble()
    scale_arm <- dplyr::bind_cols(
      dplyr::select(scale_arm, study, respondent, cue),
      scale_outcomes(ratings, "option_3", paste0("option_", c(1, 2, 4)))
    ) |>
      dplyr::transmute(study, respondent, cue, arm = "scale", item = paste(item, cue, sep = "_"), known, certain, top)
    dplyr::bind_rows(mc_arm, scale_arm)
  })
  dplyr::bind_rows(policy_rows, office_rows)
}

mc_scale_summary <- function(rows) {
  mc <- rows |>
    dplyr::filter(arm == "mc") |>
    dplyr::summarise(
      n_mc = dplyr::n(), mc_correct = mean(known), mc_corrected = mean(corrected),
      mc_se = sd(known) / sqrt(n_mc), mc_corrected_se = sd(corrected) / sqrt(n_mc),
      .by = c(study, item)
    )
  scale <- rows |>
    dplyr::filter(arm == "scale") |>
    dplyr::summarise(
      n_scale = dplyr::n(), scale_known = mean(known), certain = mean(certain), top = mean(top),
      scale_se = sd(known) / sqrt(n_scale),
      .by = c(study, item)
    )
  dplyr::left_join(mc, scale, by = c("study", "item")) |>
    dplyr::mutate(
      gap = mc_correct - scale_known, gap_se = sqrt(mc_se^2 + scale_se^2),
      gap_corrected = mc_corrected - scale_known, gap_corrected_se = sqrt(mc_corrected_se^2 + scale_se^2)
    )
}

reason_labels <- c(
  "I’ve read, seen, or heard that" = "Read, saw, or heard it",
  "It makes sense, in view of other things I know" = "Inferred it",
  "I just thought I’d take a shot" = "Guessed",
  "It makes me feel good to think that" = "Felt good to think it",
  "I asked someone I know" = "Asked someone",
  "I looked it up" = "Looked it up"
)

# Respondents in the closed-probe arm said why they chose their answer. The
# Obama deficit item is left out for the reason given at dk_orientation(); the
# second greenhouse-gas item was dropped by the authors before analysis.
travel_ban_key <- "Temporarily ban immigrants from several majority-Muslim countries"

reasons <- function(mturk) {
  items <- tibble::tribble(
    ~item, ~response, ~reason, ~key, ~arm,
    "bush_deficit", "bush_c", "bush_c_p", "Increased", "all",
    "aca_providers", "rgc_c_aca2", "rgc_c_aca2_p", "Limit future increases in payments to Medicare providers", "closed",
    "travel_ban", "rgc_c_dt", "rgc_c_dt_p", travel_ban_key, "closed"
  )
  purrr::pmap(items, \(item, response, reason, key, arm) {
    mturk |>
      dplyr::filter(probe == "closed", arm == "all" | reticence_guessing == arm) |>
      dplyr::transmute(
        study, respondent,
        item = item,
        outcome = outcome_of(.data[[response]], key, c("Don’t know", "Don’t Know")),
        reason = unname(reason_labels[.data[[reason]]])
      )
  }) |>
    purrr::list_rbind() |>
    assertr::assert(assertr::in_set(c(unname(reason_labels), NA)), reason)
}

reason_shares <- function(reason_rows) {
  reason_rows |>
    dplyr::filter(outcome != "dk", !is.na(reason)) |>
    dplyr::count(item, outcome, reason) |>
    dplyr::mutate(share = n / sum(n), responses = sum(n), .by = c(item, outcome)) |>
    tidyr::complete(item, outcome, reason = unname(reason_labels), fill = list(n = 0L, share = 0)) |>
    dplyr::mutate(responses = max(responses, na.rm = TRUE), .by = c(item, outcome))
}

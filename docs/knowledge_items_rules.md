# Knowledge-item coding rules (Table 1)

Unit: one questionnaire item in one survey. Each item appears once, even though it was fielded in both face-to-face
(FTF) and web modes. Source: the ANES 2012 and 2016 Time Series questionnaires in `data/raw/anes_questionnaires/`. The
`variable` column holds the questionnaire's specification name, not the release variable. `R/corpus.R` re-derives
every feature marked *(rule)* from the recorded text, and `tests/testthat/test-corpus.R` fails if the derived value
differs from the hand coding.

## Which items count

An item is included if both conditions hold:

1. It asks about a state of the world. That covers a person's office, a party's or candidate's position, a law's text
   or realized effect, an official statistic, a constitutional rule or a documented event. It does not cover the
   respondent's own preference, evaluation or forecast.
2. One answer is correct at fielding time according to a documentable source. The source is recorded in
   `answer_basis`: an official record or statistic, the text of the law, party or candidate platforms, or a scientific
   assessment such as the IPCC reports.

Placements of candidates and parties on the issue, ideology and left-right scales count. As in the old files, a
placement is scored correct when the target is on its party's side of the scale midpoint. Scoring the Democrat to the
left of the Republican is the common alternative. This choice affects correctness but none of the Table 1 features.

Excluded as evaluations, forecasts, normative questions or contested answers:

- 2012: `ENVIR_GWGOOD` (whether warming would be good or bad), `HLTHLAW_NUM` ("will it have increased" once fully
  implemented, a forecast), `ECON_ECPAST`, `NONMAIN_ADMINBIAS`, `MORMON_MORCHRIST`, `HLTHLAW_QUAL`, `PTYDIFF_*`.
- 2016: `CSES5_ECON` and `ECON_ECPAST` (the state of the economy is an evaluation), `RETRO_PRESECON`,
  `ECONMOBIL_INEQRED`, `HLTHLAW_AMCOST` (effect on costs is disputed), `HLTHLAW_QUALREV`, `HLTHLAW_RCOST`.
- Self-placements and follow-up items that measure strength or certainty.

## Features

Only text read or shown to the respondent counts: `preamble`, `question` and `response_options`. `{...}` marks
interviewer instructions. `preamble` holds only a display-only introduction read immediately before the item.

- `explicit_dk` *(rule)*: the question or a displayed option offers "don't know", "not sure", "haven't thought" or "no
  opinion". A volunteered DK code (FTF) or skipping an item (web) does not count. The codebooks record DK as "FTF only",
  and no candidate or party placement has a web "haven't thought" code.
- `dk_probe` *(rule)*: an instruction to answer a DK with a request for a best guess. It appears only in FTF
  interviewer instructions (the office-recognition items). The web instrument has no probe, so these items are coded 1
  for the item as fielded.
- `dyt`, `wwys`, `wipo` *(rule)*: the spoken text contains "do you think" (including "[Do / ...do] you think"), "would
  you say" or "(what is your) personal opinion". The substantive-response-encouraging wording in Table 1 is any of the
  three.
- `n_options` *(rule)*: the number of substantive options read or displayed, or `open` for text or numeric entry. A
  volunteered option that is neither read nor shown (2012 FTF abortion "Other {VOL}") does not count. Scales count
  every point: 7 or 11.
- `dke_preamble` *(rule)*: the item's own preamble or question legitimizes not knowing ("don't know", "not sure",
  "haven't thought", "no opinion", "ok to say"). No item qualifies.
- `dkd_preamble` *(rule)*: the preamble or question urges a guess before any answer ("best guess", "even if you are not
  sure", "take a guess"). No item qualifies. The office-recognition "best guess" comes only after a DK and is a probe.
- `happen_to_know` *(rule)*: "Do you happen to know". This phrase softens DK but is not a DK-encouraging preamble. It is
  reported separately.
- `self_placement_dk_filter` (hand coded): on the 7-point placements, the self-placement just before the target asks
  "Where would you place yourself on this scale, or haven't you thought much about this?", but the target item itself
  says `{DO NOT PROBE DK}` and offers no DK. The old coding counted this as a DK-encouraging preamble. Here it is kept as
  a separate flag so that either reading can be tabulated.

`correct_answer` and `answer_basis` document the knowledge criterion. `conditional` records items asked only of some
respondents. `old_coding` records every disagreement with the 2018 coding in `hidden/data/academic_polls/*_clean.xls*`.

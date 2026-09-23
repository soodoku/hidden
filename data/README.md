# Data

| File | What it is | Source |
|---|---|---|
| `raw/alumni_2010.csv`, `raw/alumni_2010_demographics.csv` | Online survey of an alumni panel of a private university in the western U.S., September–October 2010, and the panel's intake demographics | `soodoku/hidden`, `data/arep/` |
| `raw/staff_2010.csv`, `raw/staff_2010_demographics.csv` | The same survey of the university's staff panel, September–October 2010 | `soodoku/hidden`, `data/srep/` |
| `raw/mturk_march_2017.csv` | Qualtrics export of an Amazon Mechanical Turk survey of U.S. workers, 27 March 2017 | `soodoku/hidden`, `data/mturk/` |
| `raw/anes2000_office_probe.csv`, `raw/anes2004_office_probe.csv`, `raw/anes2008_office_probe.csv` | Derived extracts of the ANES Time Series public releases: case ID, interview date, probe flags and office-recognition codes | ANES 2000, 2004, 2008 Time Series (electionstudies.org); 2008 codes from the ANES office-recognition release |
| `raw/naes2004_tax_probe.csv`, `raw/naes2008_tax_probe.csv` | Not in the repository. The NAES terms forbid posting the data in whole or part; `docs/naes_probe.csv` holds the item-level results | Annenberg Public Policy Center, by request |
| `raw/anes_questionnaires/` | ANES 2012 and 2016 questionnaires used to code the item corpus | electionstudies.org; see `SOURCE.md` |

`R/sources.R` checks the survey files against SHA-256 hashes before any
analysis; `raw/probe_extracts.sha256` holds the hashes of the poll extracts.
The ANES extracts leave out the verbatim answers.

## Anonymization

The survey files were anonymized before release.
- In every file, IP addresses, latitude and longitude, city, and ZIP or postal codes are blank.
- In the 2010 files, panel IDs are replaced with random pseudonyms that keep the `FY` (alumni) and `srep` (staff) prefixes the survey software used. The mapping to the original IDs was not kept.
- Free-text birthplace, occupation, college major and ethnicity fields are also blank.

No other cell was changed. The panels' contact lists are not part of this
repository.

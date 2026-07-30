# STATS_ATTRIBUTE_AGREEMENT

Attribute Agreement Analysis (AAA) extension for IBM SPSS Statistics.  
Implements AIAG MSA 4th Edition methods for evaluating measurement system agreement on categorical (nominal or ordinal) data — Pass/Fail, defect ratings, visual classifications, and similar attribute studies.

---

## Features

- **Agreement statistics** — Overall percent agreement with Wilson score 95% CI, within-appraiser (repeatability), between-appraiser (reproducibility, pairwise)
- **Kappa-family** — Pairwise Cohen's kappa (with SE, z-test, p-value, Landis & Koch interpretation), Fleiss' kappa (overall + per-category with SE/z/p), PABAK
- **Concordance** — Kendall's W coefficient of concordance (with tie correction)
- **Standard comparison** — Each appraiser vs. reference standard: percent agreement + confusion matrices per appraiser
- **AIAG effectiveness metrics** — Effectiveness, Sensitivity, Specificity, PPV, NPV, Miss Rate, False Alarm Rate — per category, pooled across all appraisers
- **Output tables** — Summary, Within-Appraiser Agreement, Between-Appraiser Agreement, Kappa Statistics, Fleiss' Kappa, PABAK, Kendall's W, Each Appraiser vs. Standard, Effectiveness / Miss Rate / False Alarm Rate, Repeatability & Reproducibility Summary (AIAG MSA-4 labels), Confusion Matrices, Disagreement Source Analysis
- **Charts** — Between-Appraiser Heatmap (with interactive color-theme picker), Within-Appraiser Bar, Each Appraiser vs. Standard Bar, Confusion Matrix Plot, Category Distribution Pie (with counts and %), Key Metrics Panel, Kappa Forest Plot, Appraiser Disagreement Network
- **Interactive HTML report** — Plotly-based report opens in browser on every run; includes executive summary banner with AIAG verdict (Acceptable / Marginal / Unacceptable), all charts as interactive plots, heatmap color-theme switcher, and sample-by-sample run-order chart
- **Generate Study Worksheet** — builds a blank rating sheet for any appraiser/sample/trial configuration; optionally writes it directly to the active SPSS dataset (`CREATEVARS=YES`)
- **No extra R packages required** — runs on base R only

---

## Requirements

- IBM SPSS Statistics 28 or later
- R 4.1 or later
- No additional R packages (base R only)

---

## Installation

### Via Extension Hub *(recommended)*

1. Open IBM SPSS Statistics
2. Navigate to **Extensions → Extension Hub**
3. Search for **STATS ATTRIBUTE AGREEMENT** and click **Install**
4. Restart SPSS — the procedure appears under **Analyze → Quality Control → Attribute Agreement Analysis**

### Manual installation

1. Download `STATS_ATTRIBUTE_AGREEMENT.spe`
2. Navigate to **Extensions → Extension Bundles → Install Local Extension Bundle**
3. Select the downloaded `.spe` file and click **Open**
4. Restart SPSS

---

## Usage

### Dialog

**Analyze → Quality Control → Attribute Agreement Analysis**

Assign your rater/trial variables, set the number of trials per appraiser, optionally assign a Sample ID and Reference Standard variable, then click **Run**.

### Syntax

```spss
STATS ATTRIBUTE AGREEMENT
  VARS=Alice_T1 Alice_T2 Bob_T1 Bob_T2 Carol_T1 Carol_T2
  NTRIALS=2
  SAMPLEID='SampleID'
  REFVAR='Standard'
  CATEGORIES='Pass,Fail,Review'
  /OPTIONS KAPPA=YES WITHIN=YES BETWEEN=YES CONFUSION=YES VSSTANDARD=YES
           KENDALL=YES FLEISS=YES PABAK=NO EFFECTIVENESS=YES
  /OUTPUT  SUMMARY=YES DISAGREE=YES MAXLIST=20 RRSUMMARY=YES SHOWINTERP=YES
  /CHARTS  HEATMAP=YES WITHINBAR=YES VSSTDBAR=YES CONFUSIONPLOT=YES
           CATPIE=YES METRICSPANEL=YES KAPPAFOREST=YES DISAGREENETWORK=YES
  /GENERATE GENDATA=NO
  /STUDYINFO STUDY='Visual Inspection Study' BY='A. Saraswathy' DATE='2026-07-28'.
```

#### Key parameters

| Parameter | Purpose |
|---|---|
| `VARS=` | Rater/trial variable list — all appraisers × all trials, in appraiser-major order |
| `NTRIALS=` | Number of trial columns **per appraiser** (e.g. `2` for T1/T2) |
| `SAMPLEID=` | Optional variable containing sample identifiers |
| `REFVAR=` | Optional reference standard variable (enables vs-standard and confusion matrix outputs) |
| `CATEGORIES=` | Optional explicit category list (e.g. `'Pass,Fail,Review'`); auto-derived from data if omitted |

#### Key subcommands

| Subcommand | Purpose |
|---|---|
| `/OPTIONS KAPPA=YES` | Pairwise Cohen's kappa with SE, z-test, p-value |
| `/OPTIONS FLEISS=YES` | Fleiss' kappa overall and per-category |
| `/OPTIONS KENDALL=YES` | Kendall's W concordance coefficient |
| `/OPTIONS EFFECTIVENESS=YES` | AIAG effectiveness, sensitivity, specificity, PPV, NPV per category (requires REFVAR) |
| `/OUTPUT SHOWINTERP=YES` | Add Landis & Koch interpretation tier to kappa tables |
| `/OUTPUT RRSUMMARY=YES` | AIAG MSA-4 Repeatability & Reproducibility summary row |
| `/GENERATE GENDATA=YES` | Generate blank study worksheet instead of running analysis |
| `/GENERATE CREATEVARS=YES` | Write the generated worksheet into the active SPSS dataset |

Full syntax reference is available in the extension help file (`STATS_ATTRIBUTE_AGREEMENT.htm`) — click **?** in any dialog.

---

## AIAG acceptance thresholds

| Overall Agreement | Verdict |
|---|---|
| ≥ 90% | **Acceptable** |
| 80 – 89% | **Marginal** — use with caution; investigate sources of disagreement |
| < 80% | **Unacceptable** — review appraiser training or operational definitions |

These thresholds are applied automatically in both the SPSS output summary and the HTML report executive banner.

---

## Data layout

Variables must be listed in **appraiser-major, trial-minor** order and the count must equal `n_appraisers × NTRIALS`:

```
Alice_T1  Alice_T2  Bob_T1  Bob_T2  Carol_T1  Carol_T2   ← NTRIALS=2, 3 appraisers
```

Appraiser names are derived automatically from variable name stems (e.g. `Alice_T1` → `Alice`). If stripping produces non-unique names the fallback is `Appraiser1`, `Appraiser2`, …

Category values are matched case-insensitively — `Pass`, `PASS`, and `pass` are all treated as the same category.

---

## Statistical references

- AIAG *Measurement Systems Analysis Reference Manual*, 4th Edition (2010)
- Cohen, J. (1960). A coefficient of agreement for nominal scales. *Educational and Psychological Measurement*, 20(1), 37–46
- Fleiss, J. L. (1971). Measuring nominal scale agreement among many raters. *Psychological Bulletin*, 76(5), 378–382
- Kendall, M. G. (1962). *Rank Correlation Methods* (3rd ed.). Charles Griffin
- Landis, J. R., & Koch, G. G. (1977). The measurement of observer agreement for categorical data. *Biometrics*, 33(1), 159–174
- Wilson, E. B. (1927). Probable inference, the law of succession, and statistical inference. *Journal of the American Statistical Association*, 22(158), 209–212

---

## License

GPL ≥ 2.0

---

## Contributors

- Aruna Saraswathy

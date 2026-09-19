# huncMarkovNCD

Cohort Markov cost-effectiveness model of **zelravine versus usual care for Nocturnal Cortical Dysrhythmia (NCD)**, packaged as an installable R package with an interactive Shiny app.

> **Fictional example.** The disease, therapy, sponsor ("Marovex Therapeutics Inc."), trial data and all input values are invented for methods illustration. Only the background mortality table (CDC WONDER, 2020) is real public data. Nothing here describes a real product or submission.

## Model at a glance

| | |
|---|---|
| Structure | 1-year decision tree (week-12 severity split) followed by an annual Markov model |
| States | Remission, Mild, Moderate, Severe; the same four after one-time re-treatment (R); Death |
| Population / horizon | Cohort aged 45, to age 100 (55 annual cycles) |
| Perspective / currency | US payer, 2024 USD |
| Discounting | 3% costs and QALYs; cycle 1 undiscounted; no half-cycle correction |
| Uncertainty | PSA: Dirichlet (severity), Gamma (state costs), 1 - Gamma (utilities), log-normal (mortality RR) |

## Installation

```r
# install.packages("remotes")
remotes::install_github("HealthyUncertainty/huncMarkovNCD")
```

Optional packages for the Shiny app and ICER table: `shiny`, `DT`, `dampack`.

## Quick start

```r
library(huncMarkovNCD)

# Deterministic base case
det <- run_model(return_trace = TRUE)
det$results_summary

# Probabilistic analysis (reproduces the report with seed 2024)
psa <- run_model(n_sim = 1000, seed = 2024)
summarise_psa(psa$psa_data)

create_icer_table_advanced(psa$psa_data)
plot_ce_plane_advanced(psa$psa_data, reference = "Usual_care", wtp = 100000)
plot_ceac_advanced(psa$psa_data)

# Year-1 valuation scenario (report Table 4.4)
scan_year1()

# Interactive app
launch_app()
```

## The report

The manufacturer's base case report that the model implements is included in the repository:
[`inst/report/ncd_cost_effectiveness_report.docx`](inst/report/ncd_cost_effectiveness_report.docx). After installation:

```r
system.file("report", "ncd_cost_effectiveness_report.docx", package = "huncMarkovNCD")
```

## Validation

`tests/validation/validate_against_report.R` checks the package against report Tables 4.1-4.6 (results in `tests/validation/validation_results.md`), and the `testthat` suite repeats the key checks.

| Result | Report | Package |
|---|---|---|
| Usual care cost / QALYs | $141,524 / 13.9586 | $141,524 / 13.9586 |
| Zelravine cost / QALYs | $196,234 / 14.7556 | $196,234 / 14.7556 |
| ICER | $68,644 per QALY | $68,644 per QALY |
| PSA mean cost (UC / zelravine), seed 2024, 1,000 draws | $141,969 / $196,919 | $141,969 / $196,919 |
| P(cost-effective) at $50k / $100k / $150k | 35% / 63% / 74% | 35% / 63% / 74% |

PSA draws follow the random-number order of the original scripts, so the package output is numerically identical to the source model for the same seed.

## Notes and limitations

- Severity distributions in the Shiny app are editable means; PSA sampling uses fixed trial-count Dirichlet alphas (`get_dirichlet_alphas()`).
- Utilities are sampled independently in the PSA, so about 1.9% of draws violate the Remission > Mild > Moderate > Severe ordering (as in the report). They are not constrained.
- The year-1 blend switch and the baseline severity distribution are exposed in R (`year1_blend`, `year1_switch_week`, `scan_year1()`), not in the Shiny app.
- `parameter_audit.html` documents every parameter, its source and its PSA distribution. `renv.lock` pins the dependencies (regenerate with `dev/regenerate_renv_lock.R` after installing `dampack`).

## Credits and licence

- **Source model and report:** fictional manufacturer's base case, September 2026, generated with Claude AI for methods illustration (no named original authors).
- **Background mortality:** CDC WONDER, Underlying Cause of Death 1999-2020, year 2020 (public data).
- **Packaging:** Ian Cromwell (healthyuncertainty@gmail.com), using the [HEPackageR](https://github.com/HealthyUncertainty/hepackager) skill for Claude AI.
- **Licence:** MIT.

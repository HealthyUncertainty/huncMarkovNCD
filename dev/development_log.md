# Development log: huncMarkovNCD

**Prompt (2026-09-19):** "Package this model using the Skill, and include the report in the repository" (uploaded `model_ncd.R`, `model_parameters.R`, `ncd_cost_effectiveness_report.docx`).

| Step | Action | Challenge / lesson | Result |
|---|---|---|---|
| Intake | Classified as Type D-lite (2 files, `source()` chain, PSA draws generated inside the params file); mapped 22 PSA-table parameters + settings; no licence or named authors in source (fictional; report generated with Claude AI) | Ian's answers covered the package name only; licence/author/Dirichlet-UX defaults were assumed and stated | Package name `huncMarkovNCD`; pairwise Shiny template |
| Parameters | `get_default_parameters()`, `get_parameter_registry()`, `get_dirichlet_alphas()`; audit HTML (9 reported SE, 24 fixed/Dirichlet) | Template audit counted fixed/Dirichlet as "missing SE"; added a grey "n/a" status. Base-case means (0.24...) differ from alpha/N (14/58), so means were kept to preserve the report | `parameter_audit.html` |
| PSA | Vectorised draws in registry order, then Dirichlet zel/uc/base | `MCMCpack::rdirichlet()` uses a different RNG order and cannot reproduce the report; used the source's normalised-Gamma method. Log-normal SE is on the log scale (not template moment matching) | Package PSA identical to original for seed 2024 |
| Model | Ported trace and outcomes from globals to a params-driven design; `scan_year1()` kept as a function | Year-1 blend toggle and baseline distribution are not in the Shiny template, so left as R-only features | Deterministic results identical to the original (tol 1e-12) |
| Shiny | Filled pairwise template; Dirichlet panels with live sum check | Template names (`psa_results`, `create_icer_table_dampack`) did not match the other templates; reconciled to `psa_data` / `create_icer_table_advanced`. Guarded PSA plots against 1-row deterministic data | `testServer` smoke test passes (det + PSA) |
| QA | `devtools::check()`; validation vs report; renv | CRAN blocked in the sandbox: `dampack` not installable, so the ICER table used the fallback here and `renv.lock` does not pin dampack (script provided to regenerate) | 0 errors, 0 warnings, 1 environment-only note (dampack); 31/31 report checks PASS; 128+ tests |
| Report | Copied `.docx` to `inst/report/`, linked in README | none | Ships with the package |

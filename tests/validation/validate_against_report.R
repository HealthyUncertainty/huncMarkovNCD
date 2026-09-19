# Validation of huncMarkovNCD against the manufacturer's report (Tables 4.1-4.6)
# and against the ORIGINAL model scripts (model_ncd.R + model_parameters.R).
# Run from the package root:  Rscript tests/validation/validate_against_report.R

suppressMessages(devtools::load_all("."))

chk <- function(label, model, report, digits = 0) {
  data.frame(Output = label, Report = report, Package = round(model, digits),
             Match = ifelse(isTRUE(all.equal(round(model, digits), report)), "PASS", "FAIL"),
             stringsAsFactors = FALSE)
}

d   <- run_model()$results_summary
uc  <- d[d$treatment == "Usual_care", ]
zel <- d[d$treatment == "Zelravine", ]
icer <- (zel$total_costs - uc$total_costs) / (zel$total_qalys - uc$total_qalys)

t41 <- rbind(
  chk("Usual care cost",  uc$total_costs, 141524), chk("Zelravine cost", zel$total_costs, 196234),
  chk("Usual care LYs",   uc$total_lys, 20.3459, 4), chk("Zelravine LYs", zel$total_lys, 20.4497, 4),
  chk("Usual care QALYs", uc$total_qalys, 13.9586, 4), chk("Zelravine QALYs", zel$total_qalys, 14.7556, 4),
  chk("Zelravine course cost (incl. re-treatment)", zel$cost_intervention, 67877),
  chk("Zelravine health-state cost", zel$cost_state, 128357),
  chk("Re-treatments per patient", zel$n_retx, 0.219, 3),
  chk("Undiscounted LE, usual care", uc$ly_undiscounted, 32.70, 2),
  chk("Undiscounted LE, zelravine", zel$ly_undiscounted, 32.97, 2),
  chk("ICER ($/QALY)", icer, 68644))

y <- scan_year1()
t44 <- rbind(chk("Year-1 ICER, w=0", y$ICER[1], 68644), chk("Year-1 ICER, w=6", y$ICER[2], 69091),
             chk("Year-1 ICER, w=12", y$ICER[3], 69543), chk("Year-1 ICER, w=26", y$ICER[4], 70614),
             chk("Year-1 ICER, w=52", y$ICER[5], 72675))

psa <- run_model(n_sim = 1000, seed = 2024)
s <- summarise_psa(psa$psa_data)
t45 <- rbind(
  chk("PSA mean cost, usual care", psa$results_summary$total_costs[1], 141969),
  chk("PSA mean cost, zelravine",  psa$results_summary$total_costs[2], 196919),
  chk("PSA mean QALYs, usual care", psa$results_summary$total_qalys[1], 13.9218, 4),
  chk("PSA mean QALYs, zelravine",  psa$results_summary$total_qalys[2], 14.7113, 4),
  chk("PSA incremental cost", s$incremental$inc_cost, 54951),
  chk("PSA ICER", s$incremental$icer, 69601),
  chk("Incremental cost 95% interval, lower", s$incremental$inc_cost_lo, 34927),
  chk("Incremental cost 95% interval, upper", s$incremental$inc_cost_hi, 74748),
  chk("Incremental QALYs 95% interval, lower", s$incremental$inc_qaly_lo, -0.40, 2),
  chk("Incremental QALYs 95% interval, upper", s$incremental$inc_qaly_hi, 2.08, 2),
  chk("P(cost-effective) at $50,000 (%)", s$prob_ce$prob[1] * 100, 35),
  chk("P(cost-effective) at $100,000 (%)", s$prob_ce$prob[2] * 100, 63),
  chk("P(cost-effective) at $150,000 (%)", s$prob_ce$prob[3] * 100, 74),
  chk("Utility-ordering violations (%)", s$pct_utility_order_violations, 1.9, 1))

res <- rbind(t41, t44, t45)
print(res, row.names = FALSE)

# Exact comparison with the original scripts, if they are supplied
orig <- Sys.getenv("NCD_ORIGINAL_RDS", "")
if (nzchar(orig) && file.exists(orig)) {
  ref <- readRDS(orig)
  cat("\nExact match to original scripts:\n")
  print(all.equal(psa$psa_data[, 2:5], ref$psa[, 1:4], check.attributes = FALSE, tolerance = 1e-10))
}

cat(sprintf("\n%d of %d checks PASS\n", sum(res$Match == "PASS"), nrow(res)))
out <- file.path("tests", "validation", "validation_results.md")
lines <- c("# Validation results: huncMarkovNCD vs manufacturer's report", "",
           paste0("Run date: ", Sys.Date()), "",
           "| Output | Report | Package | Result |", "|---|---|---|---|",
           sprintf("| %s | %s | %s | %s |", res$Output, format(res$Report, big.mark = ",", scientific = FALSE),
                   format(res$Package, big.mark = ",", scientific = FALSE), res$Match), "",
           sprintf("**%d of %d checks PASS.**", sum(res$Match == "PASS"), nrow(res)))
writeLines(lines, out)
if (any(res$Match != "PASS")) quit(status = 1)

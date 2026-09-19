test_that("deterministic base case reproduces report Table 4.1", {
  r <- run_model()$results_summary
  uc  <- r[r$treatment == "Usual_care", ]
  zel <- r[r$treatment == "Zelravine", ]
  expect_equal(round(uc$total_costs), 141524)
  expect_equal(round(zel$total_costs), 196234)
  expect_equal(round(uc$total_lys, 4), 20.3459)
  expect_equal(round(zel$total_lys, 4), 20.4497)
  expect_equal(round(uc$total_qalys, 4), 13.9586)
  expect_equal(round(zel$total_qalys, 4), 14.7556)
})

test_that("cost breakdown and ICER reproduce report Tables 4.2-4.3", {
  r <- run_model()$results_summary
  zel <- r[r$treatment == "Zelravine", ]
  expect_equal(round(zel$cost_intervention), 67877)
  expect_equal(round(zel$cost_state), 128357)
  expect_equal(round(zel$n_retx, 3), 0.219)
  expect_equal(round(r$ly_undiscounted, 2), c(32.70, 32.97))
  icer <- (r$total_costs[2] - r$total_costs[1]) / (r$total_qalys[2] - r$total_qalys[1])
  expect_equal(round(icer), 68644)
})

test_that("Markov trace conserves the cohort and has 55 cycles", {
  tr <- run_model(return_trace = TRUE)$trace_data
  expect_equal(nrow(tr$Zelravine), 55)
  expect_true(all(abs(rowSums(tr$Zelravine[, -1]) - 1) < 1e-8))
  expect_true(all(abs(rowSums(tr$Usual_care[, -1]) - 1) < 1e-8))
  # no re-treated occupancy without re-treatment
  expect_equal(sum(tr$Usual_care[, c("Remission_R", "Mild_R", "Moderate_R", "Severe_R")]), 0)
  expect_equal(unname(tr$Zelravine[1, "age"]), 45)
})

test_that("year-1 scenario reproduces report Table 4.4", {
  y <- scan_year1()
  expect_equal(round(y$ICER), c(68644, 69091, 69543, 70614, 72675))
  expect_equal(round(y$UC_cost), c(141524, 141706, 141888, 142312, 143099))
  expect_equal(round(y$ZEL_QALY, 4), c(14.7556, 14.7418, 14.7280, 14.6959, 14.6362))
})

test_that("changing inputs moves results in the expected direction", {
  base <- run_model()$results_summary
  p <- get_default_parameters(); p$c_intervention <- 0
  cheaper <- run_model(p)$results_summary
  expect_lt(cheaper$total_costs[2], base$total_costs[2])
  expect_equal(cheaper$total_costs[1], base$total_costs[1])
  p <- get_default_parameters(); p$p_retx <- 0
  expect_equal(run_model(p)$results_summary$n_retx[2], 0)
})

test_that("PSA is reproducible and has the expected structure", {
  a <- run_model(n_sim = 10, seed = 1)
  b <- run_model(n_sim = 10, seed = 1)
  expect_equal(a$psa_data, b$psa_data)
  expect_equal(nrow(a$psa_data), 10)
  expect_true(all(c("Usual_care_cost", "Usual_care_qaly", "Zelravine_cost", "Zelravine_qaly") %in% names(a$psa_data)))
})

test_that("PSA reproduces report Tables 4.5-4.6 (seed 2024, 1,000 draws)", {
  skip_on_cran()
  psa <- run_model(n_sim = 1000, seed = 2024)
  m <- psa$results_summary
  expect_equal(round(m$total_costs), c(141969, 196919))
  expect_equal(round(m$total_qalys, 4), c(13.9218, 14.7113))
  s <- summarise_psa(psa$psa_data)
  expect_equal(round(s$incremental$inc_cost), 54951)
  expect_equal(round(s$incremental$icer), 69601)
  expect_equal(round(s$incremental$inc_cost_lo), 34927)
  expect_equal(round(s$incremental$inc_cost_hi), 74748)
  expect_equal(round(s$incremental$inc_qaly_lo, 2), -0.40)
  expect_equal(round(s$incremental$inc_qaly_hi, 2), 2.08)
  expect_equal(round(s$prob_ce$prob * 100), c(35, 63, 74))
  expect_equal(round(s$pct_utility_order_violations, 1), 1.9)
})

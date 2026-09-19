test_that("wide_to_long_cea handles deterministic and PSA data", {
  det <- wide_to_long_cea(run_model()$psa_data)
  expect_equal(nrow(det), 2)
  psa <- wide_to_long_cea(run_model(n_sim = 5, seed = 1)$psa_data)
  expect_equal(nrow(psa), 10)
  expect_true("sim" %in% names(psa))
})

test_that("ICER table shows zelravine on the frontier", {
  icer <- create_icer_table_advanced(run_model()$psa_data)
  expect_equal(nrow(icer), 2)
  expect_true(all(c("Strategy", "Cost", "Effect", "Inc_Cost", "Inc_Effect", "ICER", "Status") %in% names(icer)))
  expect_equal(round(icer$ICER[icer$Strategy == "Zelravine"]), 68644)
})

test_that("plots return ggplot objects", {
  psa <- run_model(n_sim = 20, seed = 2024, return_trace = TRUE)
  expect_s3_class(plot_ce_plane_advanced(psa$psa_data, "Usual_care", 100000), "ggplot")
  expect_s3_class(plot_ceac_advanced(psa$psa_data), "ggplot")
  expect_s3_class(plot_frontier_advanced(psa$psa_data), "ggplot")
  expect_s3_class(plot_trace(psa$trace_data), "ggplot")
})

test_that("registry defaults match get_default_parameters()", {
  p   <- get_default_parameters()
  reg <- get_parameter_registry()
  for (i in seq_len(nrow(reg))) {
    nm <- reg$name[i]
    expect_true(nm %in% names(p), info = nm)
    expect_equal(as.numeric(p[[nm]]), reg$default_value[i], info = nm)
  }
  expect_false(any(duplicated(reg$name)))
})

test_that("default severity distributions sum to 1", {
  p <- get_default_parameters()
  expect_equal(sum(unlist(p[c("p_post_zel_rem", "p_post_zel_mild", "p_post_zel_mod", "p_post_zel_sev")])), 1)
  expect_equal(sum(unlist(p[c("p_post_uc_rem", "p_post_uc_mild", "p_post_uc_mod", "p_post_uc_sev")])), 1)
  expect_equal(sum(unlist(p[c("p_base_rem", "p_base_mild", "p_base_mod", "p_base_sev")])), 1)
})

test_that("Dirichlet alphas are whole-person counts summing to trial N", {
  a <- get_dirichlet_alphas()
  expect_equal(sum(a$zelravine), 58)
  expect_equal(sum(a$usual_care), 55)
  expect_equal(sum(a$baseline), 113)
  expect_equal(unname(a$baseline[1]), 0)
})

test_that("registry SE values follow the report's interval rules", {
  reg <- get_parameter_registry()
  se <- stats::setNames(reg$se, reg$name)
  expect_equal(unname(se["u_sev"]), (0.60 - 0.36) / 3.92)
  expect_equal(unname(se["c_mod"]), (9600 - 5200) / 3.92)
  expect_equal(unname(se["rr_mort"]), 0.1034, tolerance = 1e-3)
})

test_that("validate_parameters catches bad inputs", {
  p <- get_default_parameters()
  expect_silent(validate_parameters(p))
  bad <- p; bad$end_age <- 40
  expect_error(validate_parameters(bad), "end_age")
  bad <- p; bad$u_rem <- 1.5
  expect_error(validate_parameters(bad), "u_rem")
  bad <- p; bad$year1_switch_week <- 60
  expect_error(validate_parameters(bad), "year1_switch_week")
  bad <- p; bad$discount_rate_costs <- NULL
  expect_error(validate_parameters(bad), "Missing required")
})

test_that("mortality table covers ages 18-100", {
  tab <- get_mortality_table()
  expect_equal(tab$age, 18:100)
  expect_equal(tab$m_all_per100k[tab$age == 100], 15210.9)
})

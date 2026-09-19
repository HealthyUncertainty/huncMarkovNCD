# Parameter management for the zelravine vs usual care NCD model.
# All inputs are FICTIONAL and were invented for methods illustration.
# Only the background mortality table (CDC WONDER 2020) is real public data.

#' Get Default Model Parameters
#'
#' Returns the base-case parameters of the fictional zelravine versus usual
#' care cost-effectiveness model for Nocturnal Cortical Dysrhythmia (NCD).
#' Source: manufacturer's base case report (fictional), Tables 3.1-3.4.
#'
#' @return Named list of model parameters
#' @export
#' @examples
#' params <- get_default_parameters()
#' params$start_age
#' params$rr_mort
get_default_parameters <- function() {
  list(
    # ---- General / structural ----
    cycle_length_years  = 1,
    start_age           = 45,     # [ASSUMPTION] cohort mean age, both arms
    end_age             = 100,    # [ASSUMPTION] terminal age
    discount_rate_costs = 0.03,
    discount_rate_qalys = 0.03,
    n_retx_cycles       = 3,      # re-treatment window (years)
    year1_blend         = FALSE,  # year-1 scenario switch (FALSE = base case)
    year1_switch_week   = 12,     # weeks 0-52; used only when year1_blend = TRUE
    wtp_threshold       = 100000,

    # ---- Severity distributions (proportions; Dirichlet in PSA) ----
    p_post_zel_rem  = 0.24, p_post_zel_mild = 0.30,
    p_post_zel_mod  = 0.28, p_post_zel_sev  = 0.18,
    p_post_uc_rem   = 0.15, p_post_uc_mild  = 0.28,
    p_post_uc_mod   = 0.32, p_post_uc_sev   = 0.25,

    # ---- Re-treatment and mortality ----
    p_retx  = 0.08,
    rr_mort = 1.35,

    # ---- Utilities ----
    u_rem = 0.88, u_mild = 0.79, u_mod = 0.66, u_sev = 0.48,

    # ---- Costs (2024 USD) ----
    c_rem = 3100, c_mild = 5200, c_mod = 7400, c_sev = 10800,
    c_intervention = 60000,
    c_retx         = 38000,

    # ---- Baseline (pre-treatment) distribution; year-1 scenario only ----
    p_base_rem = 0, p_base_mild = 0.12, p_base_mod = 0.46, p_base_sev = 0.42
  )
}

#' Get Dirichlet Alpha Vectors
#'
#' Whole-person trial counts (largest-remainder rounding of proportion x N)
#' used as Dirichlet concentration parameters in the PSA. A zero count fixes
#' that component at 0. Source: report Table 3.1.
#'
#' @return Named list with one alpha vector per Dirichlet group
#'   (\code{zelravine}, \code{usual_care}, \code{baseline}); vector names are
#'   parameter names.
#' @export
#' @examples
#' get_dirichlet_alphas()$zelravine
get_dirichlet_alphas <- function() {
  list(
    zelravine  = c(p_post_zel_rem = 14, p_post_zel_mild = 17,
                   p_post_zel_mod = 16, p_post_zel_sev  = 11),   # N = 58
    usual_care = c(p_post_uc_rem  = 8,  p_post_uc_mild  = 15,
                   p_post_uc_mod  = 18, p_post_uc_sev   = 14),   # N = 55
    baseline   = c(p_base_rem     = 0,  p_base_mild     = 14,
                   p_base_mod     = 52, p_base_sev      = 47)    # N = 113
  )
}

#' Get Report Validation Targets
#'
#' Base-case results of the manufacturer's report (Table 4, discounted, per
#' patient) used to validate the model, as coded in the original model script.
#'
#' @return Named list with one element per arm
#' @export
#' @examples
#' get_validation_targets()$Zelravine$qaly
get_validation_targets <- function() {
  list(
    Usual_care = list(ly = 20.35, cost = 142000, qaly = 13.96,
                      cost_intervention = 0, cost_other = 142000,
                      source = "Table 4"),
    Zelravine  = list(ly = 20.45, cost = 196000, qaly = 14.76,
                      cost_intervention = 68000, cost_other = 128000,
                      source = "Table 4")
  )
}

# SE from a stated 95% interval
se_from_interval <- function(lo, hi) (hi - lo) / (2 * 1.96)

#' Get Parameter Registry
#'
#' Returns a data.frame describing every model parameter: default value,
#' category, type, bounds, standard error and PSA distribution. Used by the
#' Shiny app to build input widgets and by the PSA module to choose sampling
#' distributions. Row order matches the original model script and must not be
#' changed: the PSA random-number sequence depends on it.
#'
#' Distribution codes: \code{gamma}; \code{one_minus_gamma} (utility = 1 -
#' Gamma draw of the disutility); \code{lognormal} (SE is on the log scale);
#' \code{dirichlet} (alphas from \code{get_dirichlet_alphas()}); \code{fixed}.
#'
#' @return A data.frame with columns: name, display_name, default_value,
#'   category, type, min, max, step, se, distribution, dist_note
#' @export
#' @examples
#' reg <- get_parameter_registry()
#' head(reg)
get_parameter_registry <- function() {

  p <- function(name, display_name, default_value, category, type,
                min = NA, max = NA, step = NA,
                se = NA, distribution = NA, dist_note = "") {
    data.frame(
      name = name, display_name = display_name, default_value = default_value,
      category = category, type = type, min = min, max = max, step = step,
      se = se, distribution = distribution, dist_note = dist_note,
      stringsAsFactors = FALSE
    )
  }

  dir_note <- "Dirichlet; alpha = trial counts (Table 3.1), fixed for PSA"
  int_note <- "SE derived from reported 95% interval: (upper - lower) / 3.92"

  rbind(
    # ---- General / structural (fixed in PSA) ----
    p("cycle_length_years", "Cycle length (years)", 1, "General", "other",
      0.25, 5, 0.25, distribution = "fixed", dist_note = "Structural; model designed for 1-year cycles"),
    p("start_age", "Cohort start age", 45, "General", "count", 18, 99, 1,
      distribution = "fixed", dist_note = "Fixed; [ASSUMPTION] cohort mean age"),
    p("end_age", "Terminal age", 100, "General", "count", 19, 100, 1,
      distribution = "fixed", dist_note = "Fixed; [ASSUMPTION] terminal age"),
    p("discount_rate_costs", "Discount rate: costs", 0.03, "General", "probability",
      0, 1, 0.005, distribution = "fixed", dist_note = "Fixed"),
    p("discount_rate_qalys", "Discount rate: QALYs", 0.03, "General", "probability",
      0, 1, 0.005, distribution = "fixed", dist_note = "Fixed"),
    p("wtp_threshold", "WTP threshold ($/QALY)", 100000, "General", "cost",
      0, NA, 10000, distribution = "fixed", dist_note = "Fixed; report threshold"),

    # ---- Severity distributions: zelravine post-treatment (year 1) ----
    p("p_post_zel_rem",  "Zelravine: Remission", 0.24, "Severity Distributions", "proportion",
      0, 1, 0.01, distribution = "dirichlet", dist_note = dir_note),
    p("p_post_zel_mild", "Zelravine: Mild",      0.30, "Severity Distributions", "proportion",
      0, 1, 0.01, distribution = "dirichlet", dist_note = dir_note),
    p("p_post_zel_mod",  "Zelravine: Moderate",  0.28, "Severity Distributions", "proportion",
      0, 1, 0.01, distribution = "dirichlet", dist_note = dir_note),
    p("p_post_zel_sev",  "Zelravine: Severe",    0.18, "Severity Distributions", "proportion",
      0, 1, 0.01, distribution = "dirichlet", dist_note = dir_note),

    # ---- Severity distributions: usual care after year 1 ----
    p("p_post_uc_rem",  "Usual care: Remission", 0.15, "Severity Distributions", "proportion",
      0, 1, 0.01, distribution = "dirichlet", dist_note = dir_note),
    p("p_post_uc_mild", "Usual care: Mild",      0.28, "Severity Distributions", "proportion",
      0, 1, 0.01, distribution = "dirichlet", dist_note = dir_note),
    p("p_post_uc_mod",  "Usual care: Moderate",  0.32, "Severity Distributions", "proportion",
      0, 1, 0.01, distribution = "dirichlet", dist_note = dir_note),
    p("p_post_uc_sev",  "Usual care: Severe",    0.25, "Severity Distributions", "proportion",
      0, 1, 0.01, distribution = "dirichlet", dist_note = dir_note),

    # ---- Re-treatment and mortality ----
    p("p_retx", "Annual re-treatment probability", 0.08, "Re-treatment & Mortality",
      "probability", 0, 1, 0.01, distribution = "fixed",
      dist_note = "Fixed in source PSA (report Table 3.4)"),
    p("rr_mort", "Mortality RR (Mild/Moderate/Severe)", 1.35, "Re-treatment & Mortality",
      "multiplier", 1, NA, 0.05,
      se = (log(1.65) - log(1.10)) / (2 * 1.96), distribution = "lognormal",
      dist_note = "RR 1.35 (95% CI 1.10-1.65); SE on log scale = (ln 1.65 - ln 1.10) / 3.92"),

    # ---- Utilities ----
    p("u_rem",  "Utility: Remission", 0.88, "Utilities", "utility", 0, 1, 0.01,
      se = se_from_interval(0.82, 0.93), distribution = "one_minus_gamma",
      dist_note = paste0(int_note, " (0.82-0.93); 1 - Gamma(1 - mean, SE)")),
    p("u_mild", "Utility: Mild",      0.79, "Utilities", "utility", 0, 1, 0.01,
      se = se_from_interval(0.73, 0.85), distribution = "one_minus_gamma",
      dist_note = paste0(int_note, " (0.73-0.85); 1 - Gamma(1 - mean, SE)")),
    p("u_mod",  "Utility: Moderate",  0.66, "Utilities", "utility", 0, 1, 0.01,
      se = se_from_interval(0.60, 0.72), distribution = "one_minus_gamma",
      dist_note = paste0(int_note, " (0.60-0.72); 1 - Gamma(1 - mean, SE)")),
    p("u_sev",  "Utility: Severe",    0.48, "Utilities", "utility", 0, 1, 0.01,
      se = se_from_interval(0.36, 0.60), distribution = "one_minus_gamma",
      dist_note = paste0(int_note, " (0.36-0.60); 1 - Gamma(1 - mean, SE)")),

    # ---- State costs ----
    p("c_rem",  "Annual cost: Remission", 3100,  "Costs - State", "cost", 0, NA, 100,
      se = se_from_interval(2200, 4000), distribution = "gamma",
      dist_note = paste0(int_note, " ($2,200-$4,000)")),
    p("c_mild", "Annual cost: Mild",      5200,  "Costs - State", "cost", 0, NA, 100,
      se = se_from_interval(3600, 6800), distribution = "gamma",
      dist_note = paste0(int_note, " ($3,600-$6,800)")),
    p("c_mod",  "Annual cost: Moderate",  7400,  "Costs - State", "cost", 0, NA, 100,
      se = se_from_interval(5200, 9600), distribution = "gamma",
      dist_note = paste0(int_note, " ($5,200-$9,600)")),
    p("c_sev",  "Annual cost: Severe",    10800, "Costs - State", "cost", 0, NA, 100,
      se = se_from_interval(7500, 14100), distribution = "gamma",
      dist_note = paste0(int_note, " ($7,500-$14,100)")),

    # ---- Treatment course costs ----
    p("c_intervention", "Zelravine initial course", 60000, "Costs - Treatment", "cost",
      0, NA, 1000, distribution = "fixed", dist_note = "Fixed in source PSA (Table 3.3a)"),
    p("c_retx", "Zelravine re-treatment course", 38000, "Costs - Treatment", "cost",
      0, NA, 1000, distribution = "fixed", dist_note = "Fixed in source PSA (Table 3.3a)"),

    # ---- Baseline distribution (year-1 scenario only) ----
    p("p_base_rem",  "Baseline: Remission", 0,    "Severity Distributions", "proportion",
      0, 1, 0.01, distribution = "dirichlet", dist_note = dir_note),
    p("p_base_mild", "Baseline: Mild",      0.12, "Severity Distributions", "proportion",
      0, 1, 0.01, distribution = "dirichlet", dist_note = dir_note),
    p("p_base_mod",  "Baseline: Moderate",  0.46, "Severity Distributions", "proportion",
      0, 1, 0.01, distribution = "dirichlet", dist_note = dir_note),
    p("p_base_sev",  "Baseline: Severe",    0.42, "Severity Distributions", "proportion",
      0, 1, 0.01, distribution = "dirichlet", dist_note = dir_note),

    # ---- Year-1 scenario switches (not PSA) ----
    p("n_retx_cycles", "Re-treatment window (years)", 3, "Re-treatment & Mortality",
      "count", 0, 54, 1, distribution = "fixed",
      dist_note = "Fixed; transitions 1->2, 2->3, 3->4"),
    p("year1_blend", "Year-1 blend switch (0 = base case)", 0, "Scenario", "other",
      0, 1, 1, distribution = "fixed",
      dist_note = "Logical switch stored as FALSE; scenario only (see scan_year1())"),
    p("year1_switch_week", "Year-1 switch week", 12, "Scenario", "count",
      0, 52, 1, distribution = "fixed", dist_note = "Used only when year1_blend = TRUE")
  )
}

#' Validate Model Parameters
#'
#' Checks that all required parameters are present and within valid ranges.
#'
#' @param params Named list of model parameters
#' @return NULL (invisible). Throws an error if validation fails.
#' @export
#' @examples
#' params <- get_default_parameters()
#' validate_parameters(params)
validate_parameters <- function(params) {

  required <- c("cycle_length_years", "start_age", "end_age",
                "discount_rate_costs", "discount_rate_qalys", "n_retx_cycles",
                "year1_blend", "year1_switch_week")
  missing <- setdiff(required, names(params))
  if (length(missing) > 0) {
    stop("Missing required parameters: ", paste(missing, collapse = ", "),
         "\nUse get_default_parameters() to see all required parameters.",
         call. = FALSE)
  }

  if (params$end_age <= params$start_age)
    stop("end_age must be greater than start_age", call. = FALSE)
  if (params$discount_rate_costs < 0 || params$discount_rate_costs > 1)
    stop("discount_rate_costs must be between 0 and 1", call. = FALSE)
  if (params$discount_rate_qalys < 0 || params$discount_rate_qalys > 1)
    stop("discount_rate_qalys must be between 0 and 1", call. = FALSE)
  if (!is.logical(params$year1_blend))
    stop("year1_blend must be TRUE or FALSE", call. = FALSE)
  if (params$year1_switch_week < 0 || params$year1_switch_week > 52)
    stop("year1_switch_week must be between 0 and 52", call. = FALSE)

  reg <- get_parameter_registry()
  for (i in seq_len(nrow(reg))) {
    nm  <- reg$name[i]
    val <- params[[nm]]
    if (is.null(val)) next
    if (!is.na(reg$min[i]) && val < reg$min[i])
      stop(nm, " must be >= ", reg$min[i], call. = FALSE)
    if (!is.na(reg$max[i]) && val > reg$max[i])
      stop(nm, " must be <= ", reg$max[i], call. = FALSE)
  }

  # Each severity distribution must have a positive total
  groups <- list(
    c("p_post_zel_rem", "p_post_zel_mild", "p_post_zel_mod", "p_post_zel_sev"),
    c("p_post_uc_rem",  "p_post_uc_mild",  "p_post_uc_mod",  "p_post_uc_sev"),
    c("p_base_rem",     "p_base_mild",     "p_base_mod",     "p_base_sev"))
  for (g in groups) {
    if (sum(unlist(params[g])) <= 0)
      stop("Severity distribution ", g[1], " ... must sum to a positive value",
           call. = FALSE)
  }

  invisible(NULL)
}

# Dirichlet draws as normalised Gamma(alpha) draws. Column-wise generation is
# deliberate: it reproduces the random-number sequence of the source model.
rdirichlet_gamma <- function(n, alpha) {
  g <- sapply(alpha, function(a) stats::rgamma(n, a))
  if (n == 1) g <- matrix(g, nrow = 1)
  g / rowSums(g)
}

#' Generate PSA Draws (Internal)
#'
#' Draws all PSA parameter values for \code{n_sim} iterations at once, in the
#' same order as the source model script (registry row order, then Dirichlet
#' groups zelravine, usual care, baseline) so that a given seed reproduces the
#' published PSA. Parameters that are fixed keep their value in \code{params}.
#'
#' @param params Base parameter list
#' @param n_sim Number of iterations
#' @param se_overrides Optional named list of SE values overriding the registry
#' @return Data frame with \code{n_sim} rows and one column per sampled parameter
#' @importFrom stats rgamma rnorm
#' @noRd
generate_psa_draws <- function(params, n_sim, se_overrides = NULL) {

  reg   <- get_parameter_registry()
  draws <- list()

  for (i in seq_len(nrow(reg))) {
    nm   <- reg$name[i]
    dist <- reg$distribution[i]
    if (is.na(dist) || !dist %in% c("gamma", "one_minus_gamma", "lognormal")) next

    se <- if (!is.null(se_overrides) && !is.null(se_overrides[[nm]])) se_overrides[[nm]] else reg$se[i]
    m  <- params[[nm]]
    if (is.null(m) || is.na(se) || se <= 0) next

    draws[[nm]] <- switch(dist,
      gamma = if (m > 0) rgamma(n_sim, shape = m^2 / se^2, scale = se^2 / m) else NULL,
      one_minus_gamma = if (m < 1) {
        d <- 1 - m
        1 - rgamma(n_sim, shape = d^2 / se^2, scale = se^2 / d)
      } else NULL,
      lognormal = if (m > 0) exp(rnorm(n_sim, log(m), se)) else NULL
    )
  }

  for (alpha in get_dirichlet_alphas()) {
    d <- rdirichlet_gamma(n_sim, alpha)
    for (j in seq_along(alpha)) draws[[names(alpha)[j]]] <- d[, j]
  }

  as.data.frame(draws)
}

#' Generate PSA Parameters for One Iteration (Internal)
#'
#' @param params Base parameter list
#' @param draws Data frame from \code{generate_psa_draws()}
#' @param iteration Row of \code{draws} to use
#' @return Parameter list with sampled values substituted
#' @noRd
generate_psa_parameters <- function(params, draws, iteration) {
  psa_params <- params
  for (nm in names(draws)) psa_params[[nm]] <- draws[[nm]][iteration]
  psa_params
}

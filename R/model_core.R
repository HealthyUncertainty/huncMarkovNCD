# Core model logic: year-1 post-treatment distribution -> annual Markov to age 100.
# States: 4 severity states (naive) + 4 severity states after one-time re-treatment
# (_R) + Death. Re-treatment: zelravine arm only, from all alive naive states,
# cycles 1 to n_retx_cycles; re-treated patients are redistributed by the zelravine
# post-treatment split and cost c_retx on entry.
# Timing: start-of-cycle occupancy, no half-cycle correction; cycle 1 undiscounted.

sev_states <- c("Remission", "Mild", "Moderate", "Severe")

# Rate <-> probability conversions
rate_to_prob <- function(r, t) 1 - exp(-r * t)

get_state_names <- function() c(sev_states, paste0(sev_states, "_R"), "Death")

get_n_cycles <- function(params) {
  ceiling((params$end_age - params$start_age) / params$cycle_length_years)
}

# Annual death probability by severity (1 = Remission ... 4 = Severe).
# Remission: background mortality. Mild/Moderate/Severe: RR x background.
get_p_death <- function(age, params) {
  r <- get_background_rate(age)
  rate_to_prob(c(r, rep(params$rr_mort * r, 3)), params$cycle_length_years)
}

# Post-treatment severity distribution (normalised)
get_post_dist <- function(params, arm) {
  p <- if (arm == "Zelravine") {
    c(params$p_post_zel_rem, params$p_post_zel_mild, params$p_post_zel_mod, params$p_post_zel_sev)
  } else {
    c(params$p_post_uc_rem, params$p_post_uc_mild, params$p_post_uc_mod, params$p_post_uc_sev)
  }
  p / sum(p)
}

get_base_dist <- function(params) {
  c(params$p_base_rem, params$p_base_mild, params$p_base_mod, params$p_base_sev)
}

get_state_costs <- function(params) {
  c(rep(c(params$c_rem, params$c_mild, params$c_mod, params$c_sev), 2) *
      params$cycle_length_years, 0)
}

get_state_utilities <- function(params) {
  c(rep(c(params$u_rem, params$u_mild, params$u_mod, params$u_sev), 2), 0)
}

#' Generate Markov Trace (Internal)
#'
#' @param params Parameter list
#' @param arm "Usual_care" or "Zelravine"
#' @return List with \code{trace} (cycles x 9 state occupancy) and
#'   \code{retx_new} (patients entering re-treatment in each cycle)
#' @noRd
generate_trace <- function(params, arm) {
  state_names  <- get_state_names()
  n_states     <- length(state_names)
  n_cycles     <- get_n_cycles(params)
  sev_of_state <- rep(1:4, 2)

  trace    <- matrix(0, n_cycles, n_states, dimnames = list(NULL, state_names))
  retx_new <- numeric(n_cycles)
  p_retx   <- if (arm == "Zelravine") params$p_retx else 0
  post_zel <- get_post_dist(params, "Zelravine")
  trace[1, 1:4] <- get_post_dist(params, arm)

  for (t in seq_len(n_cycles - 1)) {
    pd  <- get_p_death(params$start_age + (t - 1) * params$cycle_length_years, params)
    cur <- trace[t, ]
    nxt <- numeric(n_states)
    for (i in 1:8) {
      sv   <- sev_of_state[i]
      surv <- cur[i] * (1 - pd[sv])
      nxt[9] <- nxt[9] + cur[i] * pd[sv]
      if (i <= 4 && t <= params$n_retx_cycles) {
        flow <- surv * p_retx
        nxt[i]   <- nxt[i] + surv - flow
        nxt[5:8] <- nxt[5:8] + flow * post_zel
        retx_new[t + 1] <- retx_new[t + 1] + flow
      } else {
        nxt[i] <- nxt[i] + surv
      }
    }
    trace[t + 1, ] <- nxt
    trace[t + 1, 9] <- trace[t + 1, 9] + cur[9]
  }

  stopifnot(all(abs(rowSums(trace) - 1) < 1e-8), all(trace >= -1e-12))
  list(trace = trace, retx_new = retx_new)
}

#' Calculate Outcomes from a Trace (Internal)
#'
#' Year-1 toggle: cycle-1 occupancy used for COSTS and QALYs only (life years,
#' mortality and transitions use the trace). Blend = baseline distribution for
#' weeks 0..switch, the arm's post-treatment distribution for switch..52.
#'
#' @param res Output of \code{generate_trace()}
#' @param params Parameter list
#' @param arm Arm name
#' @param blend Logical year-1 blend switch
#' @param switch_week Year-1 switch week (0-52)
#' @return List of discounted and undiscounted outcomes
#' @noRd
calculate_outcomes <- function(res, params, arm,
                               blend = params$year1_blend,
                               switch_week = params$year1_switch_week) {
  tr <- res$trace
  n_cycles <- nrow(tr)
  cyc <- (0:(n_cycles - 1)) * params$cycle_length_years
  disc_c <- (1 / (1 + params$discount_rate_costs))^cyc
  disc_q <- (1 / (1 + params$discount_rate_qalys))^cyc
  sc <- get_state_costs(params)
  su <- get_state_utilities(params)

  occ <- tr
  if (blend) {
    w_b <- switch_week / 52
    occ[1, 1:4] <- w_b * get_base_dist(params) + (1 - w_b) * tr[1, 1:4]
  }

  cost_state <- sum((occ %*% sc) * disc_c)
  cost_int <- if (arm == "Zelravine") {
    params$c_intervention * disc_c[1] + sum(res$retx_new * params$c_retx * disc_c)
  } else 0

  list(
    total_ly    = sum(rowSums(tr[, 1:8]) * params$cycle_length_years * disc_q),
    total_qaly  = sum((occ %*% su) * params$cycle_length_years * disc_q),
    total_cost  = cost_state + cost_int,
    cost_state  = cost_state,
    cost_int    = cost_int,
    ly_undisc   = sum(rowSums(tr[, 1:8]) * params$cycle_length_years),
    n_retx      = sum(res$retx_new)
  )
}

#' Scan Year-1 Valuation Scenario
#'
#' Absolute outcomes and ICER across year-1 switch weeks. In the scenario the
#' first \code{w} weeks of year 1 are valued on the baseline severity
#' distribution and the remaining weeks on the arm's post-treatment
#' distribution (costs and QALYs only; mortality, transitions and life years
#' are unchanged). \code{w = 0} is the base case.
#'
#' @param parameters Named list of parameters (default: \code{get_default_parameters()})
#' @param weeks Numeric vector of switch weeks (0-52)
#' @return Data frame with columns switch_week, UC_cost, UC_QALY, ZEL_cost,
#'   ZEL_QALY, ICER
#' @export
#' @examples
#' scan_year1()
scan_year1 <- function(parameters = NULL, weeks = c(0, 6, 12, 26, 52)) {
  p <- if (is.null(parameters)) get_default_parameters() else parameters
  validate_parameters(p)
  tr_uc  <- generate_trace(p, "Usual_care")
  tr_zel <- generate_trace(p, "Zelravine")
  do.call(rbind, lapply(weeks, function(w) {
    a <- calculate_outcomes(tr_uc,  p, "Usual_care", blend = TRUE, switch_week = w)
    b <- calculate_outcomes(tr_zel, p, "Zelravine",  blend = TRUE, switch_week = w)
    data.frame(switch_week = w,
               UC_cost = a$total_cost, UC_QALY = a$total_qaly,
               ZEL_cost = b$total_cost, ZEL_QALY = b$total_qaly,
               ICER = (b$total_cost - a$total_cost) / (b$total_qaly - a$total_qaly))
  }))
}

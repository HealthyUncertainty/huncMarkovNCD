# Main user-facing function for the NCD zelravine vs usual care model

model_arms <- c("Usual_care", "Zelravine")

#' Run the NCD Cost-Effectiveness Model
#'
#' Runs both arms (usual care and zelravine) of the fictional Nocturnal
#' Cortical Dysrhythmia model. \code{n_sim = 1} gives the deterministic
#' base case; \code{n_sim > 1} runs a probabilistic sensitivity analysis (PSA)
#' in which both arms share each parameter draw.
#'
#' PSA draws are generated in the same order as the source model script, so
#' \code{run_model(n_sim = 1000, seed = 2024)} reproduces the published PSA.
#' Dirichlet groups use fixed source alphas (\code{get_dirichlet_alphas()}),
#' so \code{se_overrides} apply only to the Gamma, utility and log-normal
#' parameters.
#'
#' @param parameters Named list of model parameters (optional; defaults to
#'   \code{get_default_parameters()})
#' @param n_sim Number of PSA iterations (default 1 for deterministic)
#' @param return_trace Logical, return the deterministic Markov traces?
#' @param seed Random seed for reproducibility (optional)
#' @param se_overrides Optional named list of SE values overriding the registry
#'   defaults in the PSA (log-scale SE for \code{rr_mort})
#'
#' @return List containing:
#'   \itemize{
#'     \item results_summary - Data frame of costs, QALYs and LYs per arm
#'       (deterministic values, or PSA means when \code{n_sim > 1})
#'     \item psa_data - Wide-format data frame (sim, Usual_care_cost,
#'       Usual_care_qaly, Zelravine_cost, Zelravine_qaly, u_order_ok)
#'     \item parameters - List of parameters used
#'     \item deterministic_summary - Deterministic results (PSA runs only)
#'     \item trace_data - Named list of state-occupancy matrices with an
#'       \code{age} column (if \code{return_trace = TRUE})
#'   }
#'
#' @export
#' @examples
#' # Deterministic base case
#' det <- run_model()
#' det$results_summary
#'
#' # Small probabilistic analysis
#' psa <- run_model(n_sim = 20, seed = 2024)
#' head(psa$psa_data)
run_model <- function(parameters = NULL,
                      n_sim = 1,
                      return_trace = FALSE,
                      seed = NULL,
                      se_overrides = NULL) {

  if (is.null(parameters)) parameters <- get_default_parameters()
  validate_parameters(parameters)
  if (!is.null(seed)) set.seed(seed)

  if (n_sim == 1) {
    results <- run_deterministic_model(parameters, return_trace)
  } else {
    results <- run_psa_model(parameters, n_sim, return_trace, se_overrides)
  }
  results
}

#' Trace matrices with age column (Internal)
#' @noRd
build_trace_data <- function(params, traces) {
  n_cycles <- get_n_cycles(params)
  ages <- params$start_age + (seq_len(n_cycles) - 1) * params$cycle_length_years
  lapply(traces, function(r) cbind(age = ages, r$trace))
}

#' Run Deterministic Model (Internal)
#' @noRd
run_deterministic_model <- function(params, return_trace) {
  traces <- lapply(stats::setNames(model_arms, model_arms),
                   function(a) generate_trace(params, a))
  out <- Map(function(r, a) calculate_outcomes(r, params, a), traces, model_arms)

  results_summary <- data.frame(
    treatment   = model_arms,
    total_costs = vapply(out, function(o) o$total_cost, numeric(1)),
    total_qalys = vapply(out, function(o) o$total_qaly, numeric(1)),
    total_lys   = vapply(out, function(o) o$total_ly, numeric(1)),
    cost_intervention = vapply(out, function(o) o$cost_int, numeric(1)),
    cost_state  = vapply(out, function(o) o$cost_state, numeric(1)),
    ly_undiscounted = vapply(out, function(o) o$ly_undisc, numeric(1)),
    n_retx      = vapply(out, function(o) o$n_retx, numeric(1)),
    stringsAsFactors = FALSE, row.names = NULL
  )

  psa_data <- data.frame(
    sim = 1,
    Usual_care_cost = out$Usual_care$total_cost, Usual_care_qaly = out$Usual_care$total_qaly,
    Zelravine_cost  = out$Zelravine$total_cost,  Zelravine_qaly  = out$Zelravine$total_qaly
  )

  results <- list(results_summary = results_summary, psa_data = psa_data,
                  parameters = params)
  if (return_trace) results$trace_data <- build_trace_data(params, traces)
  results
}

#' Run PSA Model (Internal)
#' @noRd
run_psa_model <- function(params, n_sim, return_trace, se_overrides) {

  draws <- generate_psa_draws(params, n_sim, se_overrides)
  psa_data <- data.frame(sim = seq_len(n_sim),
                         Usual_care_cost = NA_real_, Usual_care_qaly = NA_real_,
                         Zelravine_cost = NA_real_, Zelravine_qaly = NA_real_,
                         u_order_ok = TRUE)

  for (j in seq_len(n_sim)) {
    p <- generate_psa_parameters(params, draws, j)
    a <- calculate_outcomes(generate_trace(p, "Usual_care"), p, "Usual_care")
    b <- calculate_outcomes(generate_trace(p, "Zelravine"),  p, "Zelravine")
    psa_data[j, 2:5] <- c(a$total_cost, a$total_qaly, b$total_cost, b$total_qaly)
    psa_data$u_order_ok[j] <- p$u_rem > p$u_mild && p$u_mild > p$u_mod && p$u_mod > p$u_sev
  }

  results_summary <- data.frame(
    treatment   = model_arms,
    total_costs = c(mean(psa_data$Usual_care_cost), mean(psa_data$Zelravine_cost)),
    total_qalys = c(mean(psa_data$Usual_care_qaly), mean(psa_data$Zelravine_qaly)),
    stringsAsFactors = FALSE
  )

  det <- run_deterministic_model(params, return_trace)
  results <- list(results_summary = results_summary, psa_data = psa_data,
                  parameters = params, deterministic_summary = det$results_summary)
  if (return_trace) results$trace_data <- det$trace_data
  results
}

#' Summarise PSA Results
#'
#' Mean outcomes, incremental results, 95 percent intervals, the share of PSA
#' draws that violate the utility ordering (Remission > Mild > Moderate >
#' Severe), and the probability that zelravine is cost-effective.
#'
#' @param psa_data Wide-format PSA data from \code{run_model()}
#' @param wtp Numeric vector of willingness-to-pay thresholds ($ per QALY)
#' @return List with elements \code{means}, \code{incremental}, \code{prob_ce}
#'   and \code{pct_utility_order_violations}
#' @export
#' @examples
#' psa <- run_model(n_sim = 20, seed = 2024)
#' summarise_psa(psa$psa_data)$incremental
summarise_psa <- function(psa_data, wtp = c(50000, 100000, 150000)) {
  inc_c <- psa_data$Zelravine_cost - psa_data$Usual_care_cost
  inc_q <- psa_data$Zelravine_qaly - psa_data$Usual_care_qaly
  means <- colMeans(psa_data[, c("Usual_care_cost", "Usual_care_qaly",
                                 "Zelravine_cost", "Zelravine_qaly")])
  list(
    means = means,
    incremental = data.frame(
      inc_cost = mean(inc_c), inc_qaly = mean(inc_q), icer = mean(inc_c) / mean(inc_q),
      inc_cost_lo = unname(stats::quantile(inc_c, 0.025)),
      inc_cost_hi = unname(stats::quantile(inc_c, 0.975)),
      inc_qaly_lo = unname(stats::quantile(inc_q, 0.025)),
      inc_qaly_hi = unname(stats::quantile(inc_q, 0.975))),
    prob_ce = data.frame(wtp = wtp,
                         prob = vapply(wtp, function(w) mean(w * inc_q - inc_c > 0), numeric(1))),
    pct_utility_order_violations =
      if ("u_order_ok" %in% names(psa_data)) mean(!psa_data$u_order_ok) * 100 else NA_real_
  )
}

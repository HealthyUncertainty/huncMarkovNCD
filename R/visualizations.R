# CEA visualisations for the NCD model (pairwise: zelravine vs usual care).
# All functions take WIDE-format results with <arm>_cost and <arm>_qaly columns.

#' Convert Wide Format to Long Format
#'
#' @param data Wide-format data frame with \code{<arm>_cost} and
#'   \code{<arm>_qaly} columns
#' @return Long-format data frame with columns \code{sim} (PSA only), \code{arm},
#'   \code{cost}, \code{qaly}
#' @export
#' @examples
#' wide_to_long_cea(run_model()$psa_data)
wide_to_long_cea <- function(data) {
  col_names <- names(data)
  cost_cols <- grep("_cost$", col_names, value = TRUE)
  qaly_cols <- grep("_qaly$", col_names, value = TRUE)
  treatments <- sub("_cost$", "", cost_cols)

  if (length(cost_cols) != length(qaly_cols)) {
    stop("Mismatch between cost and qaly columns")
  }

  if (nrow(data) == 1) {
    data.frame(arm = treatments,
               cost = as.numeric(data[1, cost_cols]),
               qaly = as.numeric(data[1, qaly_cols]),
               stringsAsFactors = FALSE)
  } else {
    do.call(rbind, lapply(seq_along(treatments), function(i) {
      data.frame(sim = seq_len(nrow(data)), arm = treatments[i],
                 cost = data[[cost_cols[i]]], qaly = data[[qaly_cols[i]]],
                 stringsAsFactors = FALSE)
    }))
  }
}

# Shared ggplot theme
theme_cea <- function(legend_position = "right") {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14, margin = ggplot2::margin(b = 5)),
      plot.subtitle = ggplot2::element_text(size = 11, margin = ggplot2::margin(b = 20)),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 15)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 15)),
      legend.position = legend_position,
      legend.title = ggplot2::element_text(size = 12, face = "bold"),
      legend.text = ggplot2::element_text(size = 11),
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(20, 30, 20, 20)
    )
}

#' Plot Cost-Effectiveness Plane with PSA
#'
#' @param psa_data Wide-format PSA results
#' @param reference Reference treatment name (default: \code{"Usual_care"})
#' @param wtp Willingness-to-pay threshold ($ per QALY)
#' @return ggplot object
#' @export
#' @examples
#' psa <- run_model(n_sim = 50, seed = 2024)
#' plot_ce_plane_advanced(psa$psa_data, reference = "Usual_care", wtp = 100000)
plot_ce_plane_advanced <- function(psa_data, reference = "Usual_care", wtp = 100000) {

  cost_cols  <- grep("_cost$", names(psa_data), value = TRUE)
  treatments <- sub("_cost$", "", cost_cols)
  treatments <- treatments[treatments != reference]

  ref_costs <- psa_data[[paste0(reference, "_cost")]]
  ref_qalys <- psa_data[[paste0(reference, "_qaly")]]

  inc_data <- do.call(rbind, lapply(treatments, function(tx) {
    data.frame(treatment = tx,
               inc_cost = psa_data[[paste0(tx, "_cost")]] - ref_costs,
               inc_qaly = psa_data[[paste0(tx, "_qaly")]] - ref_qalys,
               stringsAsFactors = FALSE)
  }))

  mean_data <- inc_data |>
    dplyr::group_by(treatment) |>
    dplyr::summarise(mean_inc_cost = mean(inc_cost),
                     mean_inc_qaly = mean(inc_qaly), .groups = "drop")

  ggplot2::ggplot() +
    ggplot2::geom_point(data = inc_data,
                        ggplot2::aes(x = inc_qaly, y = inc_cost, color = treatment),
                        alpha = 0.3, size = 1) +
    ggplot2::stat_ellipse(data = inc_data,
                          ggplot2::aes(x = inc_qaly, y = inc_cost, color = treatment),
                          level = 0.95, type = "norm", linewidth = 0.8, linetype = "dotted") +
    ggplot2::geom_point(data = mean_data,
                        ggplot2::aes(x = mean_inc_qaly, y = mean_inc_cost, color = treatment),
                        size = 4, shape = 17) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::geom_abline(slope = wtp, intercept = 0, color = "red",
                         linetype = "solid", linewidth = 0.5) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::guides(color = ggplot2::guide_legend(
      override.aes = list(shape = 17, size = 4, alpha = 1, linetype = 0))) +
    ggplot2::labs(
      title = "Cost-Effectiveness Plane",
      subtitle = paste0("Reference: ", reference, " | WTP: $", format(wtp, big.mark = ",")),
      x = "Incremental QALYs", y = "Incremental Costs ($)", color = "Treatment") +
    theme_cea("right")
}

#' Plot Cost-Effectiveness Acceptability Curve
#'
#' @param psa_data Wide-format PSA results
#' @param wtp_range Vector of WTP thresholds to evaluate
#' @return ggplot object
#' @export
#' @examples
#' psa <- run_model(n_sim = 50, seed = 2024)
#' plot_ceac_advanced(psa$psa_data)
plot_ceac_advanced <- function(psa_data, wtp_range = seq(0, 200000, by = 5000)) {

  psa_long <- wide_to_long_cea(psa_data)
  n_sim <- max(psa_long$sim)

  ceac_data <- do.call(rbind, lapply(wtp_range, function(w) {
    psa_long |>
      dplyr::group_by(sim) |>
      dplyr::mutate(nmb = qaly * w - cost) |>
      dplyr::slice_max(nmb, n = 1, with_ties = FALSE) |>
      dplyr::ungroup() |>
      dplyr::count(arm) |>
      dplyr::mutate(prob = n / n_sim, wtp = w) |>
      dplyr::select(arm, wtp, prob)
  }))

  # Arms never optimal at a threshold get probability 0 (keeps lines complete)
  grid <- expand.grid(arm = unique(psa_long$arm), wtp = wtp_range, stringsAsFactors = FALSE)
  ceac_data <- merge(grid, ceac_data, by = c("arm", "wtp"), all.x = TRUE)
  ceac_data$prob[is.na(ceac_data$prob)] <- 0

  ggplot2::ggplot(ceac_data, ggplot2::aes(x = wtp, y = prob, color = arm)) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    ggplot2::scale_x_continuous(labels = scales::dollar) +
    ggplot2::labs(
      title = "Cost-Effectiveness Acceptability Curve",
      subtitle = "Probability each treatment is cost-effective across WTP thresholds",
      x = "Willingness-to-Pay Threshold", y = "Probability Cost-Effective",
      color = "Treatment") +
    theme_cea("right")
}

#' Plot Efficiency Frontier
#'
#' @param data Wide-format results (deterministic or PSA)
#' @return ggplot object
#' @export
#' @examples
#' plot_frontier_advanced(run_model()$psa_data)
plot_frontier_advanced <- function(data) {

  results <- wide_to_long_cea(data)
  if ("sim" %in% names(results)) {
    results <- results |>
      dplyr::group_by(arm) |>
      dplyr::summarise(cost = mean(cost), qaly = mean(qaly), .groups = "drop")
  }
  results <- results[order(results$qaly), ]

  frontier_idx <- c(1)
  if (nrow(results) > 1) for (i in 2:nrow(results)) {
    last_frontier <- frontier_idx[length(frontier_idx)]
    if (results$cost[i] > results$cost[last_frontier]) {
      if (length(frontier_idx) > 1) {
        prev <- frontier_idx[length(frontier_idx) - 1]
        icer_prev_to_current <- (results$cost[i] - results$cost[prev]) /
          (results$qaly[i] - results$qaly[prev])
        icer_prev_to_last <- (results$cost[last_frontier] - results$cost[prev]) /
          (results$qaly[last_frontier] - results$qaly[prev])
        if (icer_prev_to_last < icer_prev_to_current) {
          frontier_idx <- frontier_idx[-length(frontier_idx)]
        }
      }
      frontier_idx <- c(frontier_idx, i)
    }
  }
  results$on_frontier <- seq_len(nrow(results)) %in% frontier_idx

  ggplot2::ggplot(results, ggplot2::aes(x = qaly, y = cost)) +
    ggplot2::geom_line(data = results[results$on_frontier, ],
                       ggplot2::aes(group = 1), color = "steelblue", linewidth = 1.2) +
    ggplot2::geom_point(ggplot2::aes(color = on_frontier, shape = on_frontier), size = 4) +
    ggplot2::geom_text(ggplot2::aes(label = arm), vjust = -1.2, hjust = 0.5,
                       size = 3.5, fontface = "bold") +
    ggplot2::scale_color_manual(values = c("TRUE" = "darkgreen", "FALSE" = "gray50"),
                                labels = c("TRUE" = "On Frontier", "FALSE" = "Dominated")) +
    ggplot2::scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 4),
                                labels = c("TRUE" = "On Frontier", "FALSE" = "Dominated")) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_continuous(labels = scales::comma) +
    ggplot2::labs(title = "Cost-Effectiveness Frontier",
                  subtitle = "Non-dominated treatment strategies",
                  x = "Total QALYs", y = "Total Costs ($)",
                  color = "Status", shape = "Status") +
    theme_cea("bottom")
}

#' Create ICER Table with Dominance Checking
#'
#' Uses \code{dampack::calculate_icers()} when dampack is installed; otherwise
#' a simple sequential ICER calculation. Columns follow dampack: Strategy,
#' Cost, Effect (QALYs), Inc_Cost, Inc_Effect, ICER, Status.
#'
#' @param data Wide-format results (deterministic or PSA; PSA uses means)
#' @return Data frame with the ICER table
#' @export
#' @examples
#' create_icer_table_advanced(run_model()$psa_data)
create_icer_table_advanced <- function(data) {

  results <- wide_to_long_cea(data)
  if ("sim" %in% names(results)) {
    arms <- unique(results$arm)
    results <- data.frame(
      arm  = arms,
      cost = vapply(arms, function(a) mean(results$cost[results$arm == a]), numeric(1)),
      qaly = vapply(arms, function(a) mean(results$qaly[results$arm == a]), numeric(1)),
      stringsAsFactors = FALSE, row.names = NULL)
  }

  if (requireNamespace("dampack", quietly = TRUE)) {
    return(dampack::calculate_icers(cost = results$cost, effect = results$qaly,
                                    strategies = results$arm))
  }

  # Fallback without dampack: sequential ICERs with strict dominance only
  results <- results[order(results$cost), ]
  out <- data.frame(Strategy = results$arm, Cost = results$cost, Effect = results$qaly,
                    Inc_Cost = NA_real_, Inc_Effect = NA_real_, ICER = NA_real_,
                    Status = "ND", stringsAsFactors = FALSE)
  last_nd <- 1
  if (nrow(out) > 1) for (i in 2:nrow(out)) {
    if (out$Effect[i] <= out$Effect[last_nd]) {
      out$Status[i] <- "D"
    } else {
      out$Inc_Cost[i]   <- out$Cost[i] - out$Cost[last_nd]
      out$Inc_Effect[i] <- out$Effect[i] - out$Effect[last_nd]
      out$ICER[i]       <- out$Inc_Cost[i] / out$Inc_Effect[i]
      last_nd <- i
    }
  }
  out
}

#' Plot Markov Trace
#'
#' State occupancy over time, by arm. Solid lines are naive severity states;
#' dashed lines are the re-treated (R) states.
#'
#' @param trace_data Named list of trace matrices from
#'   \code{run_model(return_trace = TRUE)$trace_data}
#' @return ggplot object
#' @export
#' @examples
#' plot_trace(run_model(return_trace = TRUE)$trace_data)
plot_trace <- function(trace_data) {

  long <- do.call(rbind, lapply(names(trace_data), function(arm) {
    m <- trace_data[[arm]]
    states <- setdiff(colnames(m), "age")
    do.call(rbind, lapply(states, function(s) {
      data.frame(arm = arm, age = m[, "age"], state = s, occupancy = m[, s],
                 stringsAsFactors = FALSE)
    }))
  }))
  long$state <- factor(long$state, levels = get_state_names())
  long$re_treated <- grepl("_R$", long$state)
  long$severity <- factor(sub("_R$", "", long$state),
                          levels = c(sev_states, "Death"))

  ggplot2::ggplot(long, ggplot2::aes(x = age, y = occupancy, color = severity,
                                     linetype = re_treated)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::facet_wrap(~arm) +
    ggplot2::scale_linetype_manual(values = c("FALSE" = "solid", "TRUE" = "dashed"),
                                   labels = c("FALSE" = "Naive", "TRUE" = "Re-treated (R)")) +
    ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    ggplot2::labs(title = "Markov Trace: State Occupancy",
                  subtitle = "Proportion of cohort in each state at the start of each cycle",
                  x = "Age (years)", y = "Proportion of cohort",
                  color = "Severity", linetype = "Treatment history") +
    theme_cea("bottom")
}

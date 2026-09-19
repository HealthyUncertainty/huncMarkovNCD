# Zelravine vs Usual Care for Nocturnal Cortical Dysrhythmia (NCD)
# Interactive Shiny Interface (Pairwise Comparison)
# Launch with: huncMarkovNCD::launch_app()

library(shiny)
library(huncMarkovNCD)

# Load optional visualization packages if available
has_ggplot <- requireNamespace("ggplot2", quietly = TRUE)
has_dplyr  <- requireNamespace("dplyr", quietly = TRUE)
has_DT     <- requireNamespace("DT", quietly = TRUE)
has_scales <- requireNamespace("scales", quietly = TRUE)

if (has_ggplot) library(ggplot2)
if (has_dplyr)  library(dplyr)   # For %>% pipe operator
if (has_scales) library(scales)

# Initialize with default parameters
default_params <- get_default_parameters()
default_reg    <- get_parameter_registry()

# Helper: look up default SE from registry
get_default_se <- function(param_name) {
  idx <- which(default_reg$name == param_name)
  if (length(idx) == 1 && !is.na(default_reg$se[idx])) {
    return(default_reg$se[idx])
  }
  NA_real_
}

# Helper: format costs with commas (>= 1000) for display
fmt_cost <- function(x) {
  ifelse(is.na(x), NA_character_,
         formatC(round(x, 0), format = "f", digits = 0, big.mark = ","))
}

# Helper: format QALYs/LYs to 2 decimal places
fmt_qaly <- function(x) {
  ifelse(is.na(x), NA_character_,
         formatC(round(x, 2), format = "f", digits = 2))
}

# ---- Widget builders for the Parameters tab ----------------------------------
# Mean + SE side by side; SE ids use the "se_" prefix
mean_se_row <- function(mean_input, nm, se_step, se_label = "SE") {
  fluidRow(
    column(6, mean_input),
    column(6, numericInput(paste0("se_", nm), se_label, value = get_default_se(nm),
                           min = 0, step = se_step))
  )
}

utility_row <- function(nm, label) {
  mean_se_row(sliderInput(nm, label, min = 0, max = 1, value = default_params[[nm]], step = 0.01),
              nm, se_step = 0.005)
}

cost_row <- function(nm, label, with_se = TRUE) {
  inp <- numericInput(nm, label, value = default_params[[nm]], min = 0, step = 100)
  if (with_se) mean_se_row(inp, nm, se_step = 50) else fluidRow(column(6, inp))
}

# Dirichlet panel: editable means with live sum-to-1 check; PSA alphas are fixed
dirichlet_panel <- function(title, sum_id, nms, labels, n_note) {
  tagList(
    h5(title),
    lapply(seq_along(nms), function(i) {
      fluidRow(column(6, numericInput(nms[i], labels[i], value = default_params[[nms[i]]],
                                      min = 0, max = 1, step = 0.01)))
    }),
    textOutput(sum_id, inline = TRUE),
    tags$br(),
    tags$small(paste0("Dirichlet in PSA; alphas fixed from trial counts (", n_note, ").")),
    tags$hr()
  )
}

# Inputs that map directly to model parameters
dirichlet_ids <- list(
  zel = c("p_post_zel_rem", "p_post_zel_mild", "p_post_zel_mod", "p_post_zel_sev"),
  uc  = c("p_post_uc_rem",  "p_post_uc_mild",  "p_post_uc_mod",  "p_post_uc_sev"))
slider_ids  <- c("u_rem", "u_mild", "u_mod", "u_sev")
numeric_ids <- c("p_retx", "n_retx_cycles", "rr_mort",
                 "c_rem", "c_mild", "c_mod", "c_sev", "c_intervention", "c_retx",
                 unlist(dirichlet_ids))
se_ids <- c("rr_mort", "u_rem", "u_mild", "u_mod", "u_sev", "c_rem", "c_mild", "c_mod", "c_sev")

# UI
ui <- fluidPage(
  titlePanel("Zelravine for NCD: Cost-Effectiveness Model"),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      h4("Model Configuration"),

      numericInput("start_age", "Cohort Start Age (years)",
                   value = default_params$start_age, min = 18, max = 99, step = 1),
      numericInput("end_age", "Terminal Age (years)",
                   value = default_params$end_age, min = 19, max = 100, step = 1),

      sliderInput("discount_costs", "Discount Rate: Costs (%)",
                  min = 0, max = 10, value = 100 * default_params$discount_rate_costs, step = 0.5),
      sliderInput("discount_qalys", "Discount Rate: QALYs (%)",
                  min = 0, max = 10, value = 100 * default_params$discount_rate_qalys, step = 0.5),

      numericInput("wtp", "WTP Threshold ($ per QALY)",
                   value = default_params$wtp_threshold, min = 0, step = 10000),

      hr(),

      h4("Analysis Type"),
      radioButtons("analysis_type", NULL,
                   choices = c("Deterministic" = "det",
                               "Probabilistic (PSA)" = "psa"),
                   selected = "det"),

      conditionalPanel(
        condition = "input.analysis_type == 'psa'",
        numericInput("n_sims", "PSA Iterations:",
                     value = 1000, min = 100, max = 10000, step = 100),
        checkboxInput("use_seed", "Use Random Seed", value = FALSE),
        conditionalPanel(
          condition = "input.use_seed == true",
          numericInput("seed", "Seed Value:", value = 2024, min = 1)
        )
      ),

      hr(),

      actionButton("run", "Run Model", class = "btn-primary btn-lg"),
      actionButton("reset_params", "Reset to Defaults", class = "btn-secondary"),

      br(), br(),

      helpText("Source: fictional manufacturer's base case report (Marovex Therapeutics Inc., September 2026); background mortality from CDC WONDER (2020).")
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("About",
                 br(),
                 h3("Zelravine for NCD: Cost-Effectiveness Model"),
                 p("This interactive tool implements a cohort Markov state-transition model (with a one-year decision-tree phase) for zelravine versus usual care in Nocturnal Cortical Dysrhythmia (NCD). The disease, therapy, sponsor, trial data and all inputs are FICTIONAL and were invented for methods illustration."),

                 h4("Model Structure"),
                 tags$ul(
                   tags$li("9 health states: Remission, Mild, Moderate, Severe (naive); Remission (R), Mild (R), Moderate (R), Severe (R) (after one-time re-treatment); Death"),
                   tags$li("1-year cycle length; year 1 is valued on each arm's week-12 severity distribution (decision tree), then annual Markov cycles"),
                   tags$li("Default lifetime time horizon: age 45 to 100 (customizable), 55 cycles"),
                   tags$li("3% annual discount rate for costs and QALYs (cycle 1 undiscounted; no half-cycle correction)"),
                   tags$li("Zelravine arm only: 8% annual re-treatment probability in cycles 1-3, once per patient, from all alive naive states"),
                   tags$li("Mild, Moderate and Severe states carry a mortality relative risk (default 1.35) on CDC WONDER 2020 background mortality"),
                   tags$li("The pre-treatment baseline distribution is used only in the year-1 scenario analysis (see scan_year1() in the R package)")
                 ),

                 h4("Treatments"),
                 tags$ul(
                   tags$li("Usual care (comparator)"),
                   tags$li("Zelravine (single 12-week course, one-time re-treatment)")
                 ),

                 h4("Key Features"),
                 tags$ul(
                   tags$li("Deterministic and probabilistic sensitivity analysis (PSA)"),
                   tags$li("Cost-effectiveness plane visualization"),
                   tags$li("Cost-effectiveness acceptability curve (CEAC)"),
                   tags$li("Markov trace visualization"),
                   tags$li("Downloadable results and plots")
                 ),

                 h4("Source"),
                 p("Cost-Effectiveness Analysis of Zelravine for Nocturnal Cortical Dysrhythmia (NCD): Manufacturer's Base Case Economic Evaluation. Prepared for a Health Technology Assessment Agency by Marovex Therapeutics Inc. (fictitious), September 2026. The report and model were generated with Claude AI for methods illustration; no original human authors are credited. The report is included in the package (inst/report). Background mortality: CDC WONDER, Underlying Cause of Death 1999-2020, year 2020 (public data)."),

                 h4("Validation"),
                 p("Deterministic base case reproduces the report: usual care $141,524 and 13.9586 QALYs; zelravine $196,234 and 14.7556 QALYs; ICER $68,644 per QALY. PSA with seed 2024 and 1,000 iterations reproduces the report's PSA results. See the package README."),

                 hr(),

                 h4("Development"),
                 p("Ian Cromwell (healthyuncertainty@gmail.com)"),
                 p("Developed using the ",
                   tags$a(href = "https://github.com/HealthyUncertainty/hepackager",
                          "HEPackageR"),
                   " skill for Claude AI.")
        ),

        tabPanel("Parameters",
                 br(),
                 h4("Adjust Model Parameters"),
                 p("Modify parameter means and standard errors below. SE values are used in PSA sampling. Severity distributions are sampled jointly (Dirichlet) using fixed trial-count alphas, so editing their means changes the deterministic analysis but not the PSA sampling variability."),

                 tabsetPanel(
                   tabPanel("Severity Distributions", br(),
                            dirichlet_panel("Zelravine: distribution after 12-week treatment (n = 58)", "sum_zel",
                                            dirichlet_ids$zel,
                                            c("Remission", "Mild", "Moderate", "Severe"), "N = 58"),
                            dirichlet_panel("Usual care: distribution after year 1 (n = 55)", "sum_uc",
                                            dirichlet_ids$uc,
                                            c("Remission", "Mild", "Moderate", "Severe"), "N = 55")
                   ),
                   tabPanel("Re-treatment & Mortality", br(),
                            fluidRow(column(6, numericInput("p_retx", "Annual re-treatment probability (zelravine)",
                                                            value = default_params$p_retx, min = 0, max = 1, step = 0.01))),
                            fluidRow(column(6, numericInput("n_retx_cycles", "Re-treatment window (years)",
                                                            value = default_params$n_retx_cycles, min = 0, max = 54, step = 1))),
                            mean_se_row(numericInput("rr_mort", "Mortality RR (Mild / Moderate / Severe)",
                                                     value = default_params$rr_mort, min = 1, step = 0.05),
                                        "rr_mort", se_step = 0.01, se_label = "SE (log scale)")
                   ),
                   tabPanel("Utilities", br(),
                            utility_row("u_rem",  "Remission"),
                            utility_row("u_mild", "Mild"),
                            utility_row("u_mod",  "Moderate"),
                            utility_row("u_sev",  "Severe"),
                            helpText("Utilities are sampled independently in the PSA, so some draws may violate the Remission > Mild > Moderate > Severe ordering.")
                   ),
                   tabPanel("Costs", br(),
                            h5("Annual health state costs (2024 USD)"),
                            cost_row("c_rem",  "Remission"),
                            cost_row("c_mild", "Mild"),
                            cost_row("c_mod",  "Moderate"),
                            cost_row("c_sev",  "Severe"),
                            h5("Treatment course costs (fixed in PSA)"),
                            cost_row("c_intervention", "Zelravine initial course", with_se = FALSE),
                            cost_row("c_retx", "Zelravine re-treatment course", with_se = FALSE)
                   )
                 )
        ),

        tabPanel("Results",
                 br(),
                 h4("Incremental Cost-Effectiveness Ratios"),
                 p("Dominance analysis using dampack"),
                 conditionalPanel(
                   condition = "output.has_results == false",
                   div(class = "alert alert-info", style = "font-size: 16px;",
                       icon("info-circle"),
                       " Click 'Run Model' in the sidebar to generate results.")
                 ),
                 if (has_DT) {
                   DT::dataTableOutput("icer_table")
                 } else {
                   tableOutput("icer_table")
                 },
                 br(),
                 downloadButton("download_icer", "Download ICER Table (CSV)")
        ),

        tabPanel("Probabilistic",
                 br(),
                 conditionalPanel(
                   condition = "input.analysis_type == 'psa'",
                   h4("Cost-Effectiveness Plane"),
                   plotOutput("ce_plane", height = "500px"),
                   br(),
                   h4("Cost-Effectiveness Acceptability Curve"),
                   p("Probability each treatment is cost-effective across WTP thresholds"),
                   plotOutput("ceac_plot", height = "500px"),
                   br(),
                   downloadButton("download_psa", "Download PSA Plots (PNG)")
                 ),
                 conditionalPanel(
                   condition = "input.analysis_type == 'det'",
                   br(),
                   div(class = "alert alert-info", style = "font-size: 16px;",
                       icon("info-circle"),
                       " Probabilistic outputs require PSA. Switch to 'Probabilistic (PSA)' analysis mode to view CE plane and CEAC.")
                 )
        ),

        tabPanel("Trace",
                 br(),
                 h4("Markov Trace"),
                 p("State occupancy over time"),
                 plotOutput("trace_plot", height = "600px"),
                 br(),
                 downloadButton("download_trace", "Download Trace Plot (PNG)")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {

  # Reactive storage for results
  model_results <- reactiveVal(NULL)

  # Expose has_results for conditionalPanel use
  output$has_results <- reactive({ !is.null(model_results()) })
  outputOptions(output, "has_results", suspendWhenHidden = FALSE)

  # Live sum-to-1 validation for the Dirichlet groups
  sum_check <- function(nms) {
    vals <- vapply(nms, function(p) { v <- input[[p]]; if (is.null(v) || is.na(v)) 0 else v }, numeric(1))
    s <- sum(vals)
    status <- if (abs(s - 1) < 0.005) "OK" else "WARNING: does not sum to 1 (normalised in the model)"
    paste0("Sum = ", round(s, 4), " - ", status)
  }
  output$sum_zel <- renderText(sum_check(dirichlet_ids$zel))
  output$sum_uc  <- renderText(sum_check(dirichlet_ids$uc))

  # Collect SE overrides from all SE inputs
  # Returns named list of SE values keyed by parameter name (without "se_" prefix)
  collect_se_overrides <- function() {
    reg <- get_parameter_registry()
    overrides <- list()
    for (nm in reg$name) {
      input_id <- paste0("se_", nm)
      val <- input[[input_id]]
      if (!is.null(val) && !is.na(val) && val > 0) {
        overrides[[nm]] <- val
      }
    }
    overrides
  }

  # Reset parameters
  observeEvent(input$reset_params, {
    updateNumericInput(session, "start_age", value = default_params$start_age)
    updateNumericInput(session, "end_age", value = default_params$end_age)
    updateSliderInput(session, "discount_costs", value = 100 * default_params$discount_rate_costs)
    updateSliderInput(session, "discount_qalys", value = 100 * default_params$discount_rate_qalys)
    updateNumericInput(session, "wtp", value = default_params$wtp_threshold)
    for (nm in slider_ids)  updateSliderInput(session, nm, value = default_params[[nm]])
    for (nm in numeric_ids) updateNumericInput(session, nm, value = default_params[[nm]])
    for (nm in se_ids)      updateNumericInput(session, paste0("se_", nm), value = get_default_se(nm))
    showNotification("Parameters reset to defaults", type = "message")
  })

  # Run model
  observeEvent(input$run, {
    withProgress(message = 'Running analysis...', value = 0, {

      incProgress(0.2, detail = "Building parameters...")

      # Build parameters from inputs
      params <- get_default_parameters()
      params$start_age           <- input$start_age
      params$end_age             <- input$end_age
      params$discount_rate_costs <- input$discount_costs / 100
      params$discount_rate_qalys <- input$discount_qalys / 100
      params$wtp_threshold       <- input$wtp
      for (nm in c(slider_ids, numeric_ids)) {
        v <- input[[nm]]
        if (!is.null(v) && !is.na(v)) params[[nm]] <- v
      }

      # Determine n_sim and seed
      n_sim <- if (input$analysis_type == "psa") input$n_sims else 1
      seed_value <- if (input$analysis_type == "psa" && input$use_seed) input$seed else NULL

      # Collect SE overrides from UI
      se_overrides <- collect_se_overrides()

      incProgress(0.5, detail = "Running model...")

      results <- tryCatch(
        run_model(
          parameters = params,
          n_sim = n_sim,
          seed = seed_value,
          return_trace = TRUE,
          se_overrides = if (length(se_overrides) > 0) se_overrides else NULL
        ),
        error = function(e) {
          showNotification(paste("Model error:", conditionMessage(e)), type = "error", duration = 10)
          NULL
        }
      )

      if (!is.null(results)) {
        model_results(results)
        showNotification("Analysis complete!", type = "message", duration = 3)
      }

      incProgress(1.0, detail = "Complete")
    })
  })

  # ICER table (raw numbers) and display version
  icer_raw <- reactive({
    req(model_results())
    create_icer_table_advanced(model_results()$psa_data)
  })

  icer_display <- reactive({
    icer <- icer_raw()
    data.frame(
      Strategy   = icer$Strategy,
      Cost       = fmt_cost(icer$Cost),
      QALYs      = fmt_qaly(icer$Effect),
      Inc_Cost   = fmt_cost(icer$Inc_Cost),
      Inc_QALYs  = fmt_qaly(icer$Inc_Effect),
      ICER       = fmt_cost(icer$ICER),
      Status     = icer$Status,
      stringsAsFactors = FALSE
    )
  })

  output$icer_table <- if (has_DT) {
    DT::renderDataTable({
      DT::datatable(icer_display(),
                    options = list(
                      pageLength = -1,
                      dom = 't',
                      paging = FALSE,
                      ordering = FALSE,
                      autoWidth = FALSE,
                      scrollX = FALSE,
                      scrollY = FALSE
                    ),
                    rownames = FALSE)
    })
  } else {
    renderTable({
      icer_display()
    })
  }

  # CE Plane
  output$ce_plane <- renderPlot({
    req(model_results())
    req(input$analysis_type == "psa", nrow(model_results()$psa_data) > 1)

    if (!has_ggplot || !has_dplyr) {
      plot(1, type = "n", xlab = "", ylab = "", axes = FALSE)
      text(1, 1, "ggplot2 and dplyr required for CE plane", cex = 1.5)
      return()
    }

    plot_ce_plane_advanced(model_results()$psa_data, reference = "Usual_care", wtp = input$wtp)
  })

  # CEAC
  output$ceac_plot <- renderPlot({
    req(model_results())
    req(input$analysis_type == "psa", nrow(model_results()$psa_data) > 1)

    if (!has_ggplot) {
      plot(1, type = "n", xlab = "", ylab = "", axes = FALSE)
      text(1, 1, "ggplot2 required for CEAC", cex = 1.5)
      return()
    }

    plot_ceac_advanced(model_results()$psa_data)
  })

  # Trace Plot - shows the deterministic trace even when running PSA
  output$trace_plot <- renderPlot({
    req(model_results())

    if (!has_ggplot) {
      plot(1, type = "n", xlab = "", ylab = "", axes = FALSE)
      text(1, 1, "ggplot2 required for trace plot", cex = 1.5)
      return()
    }

    plot_trace(model_results()$trace_data)
  })

  # Download handlers
  output$download_icer <- downloadHandler(
    filename = function() paste0("ncd_icer_", Sys.Date(), ".csv"),
    content = function(file) {
      write.csv(icer_raw(), file, row.names = FALSE)
    }
  )

  output$download_psa <- downloadHandler(
    filename = function() paste0("ncd_psa_", Sys.Date(), ".png"),
    content = function(file) {
      req(model_results(), nrow(model_results()$psa_data) > 1)
      png(file, width = 12, height = 10, units = "in", res = 300)
      grid::grid.newpage()
      grid::pushViewport(grid::viewport(layout = grid::grid.layout(2, 1)))
      print(plot_ce_plane_advanced(model_results()$psa_data, reference = "Usual_care", wtp = input$wtp),
            vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
      print(plot_ceac_advanced(model_results()$psa_data),
            vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 1))
      dev.off()
    }
  )

  output$download_trace <- downloadHandler(
    filename = function() paste0("ncd_trace_", Sys.Date(), ".png"),
    content = function(file) {
      req(model_results())
      png(file, width = 10, height = 7, units = "in", res = 300)
      print(plot_trace(model_results()$trace_data))
      dev.off()
    }
  )
}

shinyApp(ui = ui, server = server)

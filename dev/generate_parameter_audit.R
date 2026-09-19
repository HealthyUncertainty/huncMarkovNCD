# Template: parameter_audit.R
# Claude MUST produce this artifact during the Parameter Extraction phase.
# It is delivered to the user as an HTML file via a standalone script.
#
# INSTRUCTIONS FOR CLAUDE:
# 1. Run this script after populating the registry in parameters.R
# 2. It reads get_parameter_registry() and renders an HTML summary table
# 3. The table shows every parameter with its inferred type, distribution,
#    source location, and any inference notes
# 4. Deliver the HTML to the user so they can review all assumptions
#
# The user should be able to scan this table and immediately see:
#   - What Claude extracted
#   - Where each value came from
#   - What distributional assumptions were made and why
#   - Any parameters where Claude was uncertain

generate_parameter_audit <- function(registry, model_name = "Model",
                                     output_file = "parameter_audit.html") {

  # Build HTML table rows
  rows <- vapply(seq_len(nrow(registry)), function(i) {
    r <- registry[i, ]

    # Colour-code confidence: known SE = green, inferred = amber, no SE = red
    se_status <- if (!is.na(r$distribution) && r$distribution %in% c("fixed", "dirichlet")) {
      '<span style="color:#6b7280;">&#9679; n/a (fixed / Dirichlet)</span>'
    } else if (!is.na(r$se) && r$se > 0) {
      if (grepl("inferred|assumed|default", r$dist_note, ignore.case = TRUE)) {
        '<span style="color:#d97706;">&#9679; Inferred</span>'
      } else {
        '<span style="color:#16a34a;">&#9679; Reported</span>'
      }
    } else {
      '<span style="color:#dc2626;">&#9679; Missing</span>'
    }

    sprintf(
      "<tr>
        <td><code>%s</code></td>
        <td>%s</td>
        <td style='text-align:right;'>%s</td>
        <td style='text-align:right;'>%s</td>
        <td>%s</td>
        <td>%s</td>
        <td>%s</td>
        <td>%s</td>
      </tr>",
      r$name,
      r$display_name,
      format(r$default_value, big.mark = ",", scientific = FALSE),
      if (is.na(r$se)) "—" else format(r$se, big.mark = ",", scientific = FALSE),
      r$category,
      r$type,
      if (is.na(r$distribution)) "—" else r$distribution,
      paste0(se_status, if (nzchar(r$dist_note)) paste0("<br><small>", r$dist_note, "</small>") else "")
    )
  }, character(1))

  # Count summary stats
  n_total     <- nrow(registry)
  n_reported  <- sum(!is.na(registry$se) & registry$se > 0 &
                       !grepl("inferred|assumed|default", registry$dist_note, ignore.case = TRUE))
  n_inferred  <- sum(!is.na(registry$se) & registry$se > 0 &
                       grepl("inferred|assumed|default", registry$dist_note, ignore.case = TRUE))
  n_na        <- sum(!is.na(registry$distribution) & registry$distribution %in% c("fixed", "dirichlet"))
  n_missing   <- n_total - n_reported - n_inferred - n_na

  html <- sprintf('<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<title>Parameter Audit: %s</title>
<style>
  body { font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
         max-width: 1200px; margin: 40px auto; padding: 0 20px; color: #1a1a1a; }
  h1 { border-bottom: 2px solid #2563eb; padding-bottom: 8px; }
  .summary { display: flex; gap: 24px; margin: 16px 0 24px; }
  .stat { padding: 12px 20px; border-radius: 8px; font-size: 14px; }
  .stat-green  { background: #dcfce7; color: #166534; }
  .stat-amber  { background: #fef3c7; color: #92400e; }
  .stat-red    { background: #fee2e2; color: #991b1b; }
  .stat-grey   { background: #f3f4f6; color: #374151; }
  table { border-collapse: collapse; width: 100%%; font-size: 13px; }
  th { background: #1e3a5f; color: white; padding: 10px 12px; text-align: left; }
  td { padding: 8px 12px; border-bottom: 1px solid #e5e7eb; vertical-align: top; }
  tr:hover { background: #f8fafc; }
  code { background: #f1f5f9; padding: 2px 5px; border-radius: 3px; font-size: 12px; }
  small { color: #6b7280; }
  .legend { margin: 16px 0; font-size: 13px; color: #4b5563; }
</style>
</head>
<body>
<h1>Parameter Audit: %s</h1>

<div class="summary">
  <div class="stat stat-green"><strong>%d</strong> parameters with reported SE</div>
  <div class="stat stat-amber"><strong>%d</strong> parameters with inferred SE</div>
  <div class="stat stat-red"><strong>%d</strong> parameters with no SE</div>
  <div class="stat stat-grey"><strong>%d</strong> fixed or Dirichlet (no SE needed)</div>
</div>

<div class="legend">
  <span style="color:#16a34a;">&#9679;</span> <strong>Reported</strong> — SE from source publication &nbsp;
  <span style="color:#d97706;">&#9679;</span> <strong>Inferred</strong> — SE assumed by Claude (see notes) &nbsp;
  <span style="color:#dc2626;">&#9679;</span> <strong>Missing</strong> — No SE available; parameter fixed in PSA
</div>

<table>
<thead>
<tr>
  <th>Parameter</th>
  <th>Display Name</th>
  <th>Mean / Default</th>
  <th>SE</th>
  <th>Category</th>
  <th>Type</th>
  <th>PSA Distribution</th>
  <th>Status / Notes</th>
</tr>
</thead>
<tbody>
%s
</tbody>
</table>

<hr>
<p style="font-size:12px; color:#9ca3af;">
  Generated by parameter_audit.R &bull; %s
</p>
</body>
</html>',
    model_name, model_name,
    n_reported, n_inferred, n_missing, n_na,
    paste(rows, collapse = "\n"),
    format(Sys.time(), "%Y-%m-%d %H:%M")
  )

  writeLines(html, output_file)
  message("Parameter audit written to: ", output_file)
  invisible(output_file)
}

# ---- Usage (run from the package root) ----
# devtools::load_all(".")
# source("dev/generate_parameter_audit.R")
# generate_parameter_audit(get_parameter_registry(),
#   model_name = "huncMarkovNCD: Zelravine vs Usual Care (NCD)",
#   output_file = "parameter_audit.html")

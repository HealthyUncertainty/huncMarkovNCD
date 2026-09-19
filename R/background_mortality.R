# Background mortality: CDC WONDER, Underlying Cause of Death 1999-2020, year 2020.
# All-cause crude rate per 100,000, both sexes, single-year age 18-84; ages 85+
# use the pooled 85+ crude rate (15,210.9). Real public data.

.pkg_cache <- new.env(parent = emptyenv())

#' Get Background Mortality Table
#'
#' Returns the general-population all-cause crude mortality table (CDC WONDER,
#' Underlying Cause of Death 1999-2020, year 2020; both sexes; ages 85 and over
#' use the pooled 85+ rate).
#'
#' @return Data frame with columns \code{age} (18-100) and \code{m_all_per100k}
#' @export
#' @examples
#' head(get_mortality_table())
get_mortality_table <- function() {
  if (is.null(.pkg_cache$mortality)) {
    f <- system.file("extdata", "cdc_wonder_mortality_2020.csv",
                     package = "huncMarkovNCD")
    if (!nzchar(f)) stop("Mortality table not found in package", call. = FALSE)
    .pkg_cache$mortality <- utils::read.csv(f)
  }
  .pkg_cache$mortality
}

#' Background All-Cause Mortality Rate (Internal)
#'
#' @param age Numeric vector of ages; floored and clamped to 18-100
#' @return Annual rate (per person-year)
#' @noRd
get_background_rate <- function(age) {
  tab <- get_mortality_table()
  idx <- pmin(pmax(floor(age), 18), 100)
  tab$m_all_per100k[match(idx, tab$age)] / 1e5
}

#' Summarise platform coverage in a culturomic table
#'
#' Live digital-trace data often have unequal platform coverage because Google
#' Trends, Wikimedia and news APIs may return different date ranges. This helper
#' reports how many platforms are available by concept and date so that users can
#' interpret attention indices with their data coverage.
#'
#' @param x A culturomic table or data frame containing date, concept_id and platform.
#' @return A data frame with concept-level platform coverage summaries.
#' @examples
#' data <- con_culturomics_example_data()
#' traces <- simulate_culturomic_traces(data$dictionary, from = "2024-01-01", to = "2024-01-10")
#' platform_coverage(traces)
#' @export
platform_coverage <- function(x) {
  dat <- as.data.frame(x, stringsAsFactors = FALSE)
  .cc_required_cols(dat, c("date", "concept_id", "platform"), "x")
  dat$date <- .cc_as_date(dat$date)
  dat$country <- if ("country" %in% names(dat)) .cc_normalise_country(dat$country) else "global"
  dat$language <- if ("language" %in% names(dat)) as.character(dat$language) else "und"

  key_cols <- c("date", "country", "language", "concept_id")
  keys <- .cc_make_key(dat, key_cols)
  by_key <- split(seq_len(nrow(dat)), keys, drop = TRUE)
  daily <- .cc_bind_rows_fill(lapply(by_key, function(ii) {
    z <- dat[ii, , drop = FALSE]
    data.frame(
      date = z$date[1],
      country = z$country[1],
      language = z$language[1],
      concept_id = z$concept_id[1],
      n_platforms = length(unique(z$platform)),
      platforms = paste(sort(unique(z$platform)), collapse = ";"),
      stringsAsFactors = FALSE
    )
  }))

  by_concept <- split(seq_len(nrow(daily)), daily$concept_id, drop = TRUE)
  out <- .cc_bind_rows_fill(lapply(by_concept, function(ii) {
    z <- daily[ii, , drop = FALSE]
    data.frame(
      concept_id = z$concept_id[1],
      date_min = min(z$date, na.rm = TRUE),
      date_max = max(z$date, na.rm = TRUE),
      n_dates = length(unique(z$date)),
      min_platforms_per_date = min(z$n_platforms, na.rm = TRUE),
      mean_platforms_per_date = mean(z$n_platforms, na.rm = TRUE),
      max_platforms_per_date = max(z$n_platforms, na.rm = TRUE),
      platforms_seen = paste(sort(unique(unlist(strsplit(paste(z$platforms, collapse = ";"), ";", fixed = TRUE)))), collapse = ";"),
      stringsAsFactors = FALSE
    )
  }))
  rownames(out) <- NULL
  class(out) <- c("platform_coverage", class(out))
  attr(out, "daily_coverage") <- daily
  out
}

#' Keep only rows with sufficient platform overlap
#'
#' @param x A culturomic table or data frame.
#' @param min_platforms Minimum number of distinct platforms required per date, country, language and concept.
#' @param required_platforms Optional character vector of platforms that must all be present.
#' @return Filtered culturomic table.
#' @examples
#' data <- con_culturomics_example_data()
#' traces <- simulate_culturomic_traces(data$dictionary, from = "2024-01-01", to = "2024-01-10")
#' filter_platform_overlap(traces, min_platforms = 2)
#' @export
filter_platform_overlap <- function(x, min_platforms = 1, required_platforms = NULL) {
  dat <- as.data.frame(x, stringsAsFactors = FALSE)
  .cc_required_cols(dat, c("date", "concept_id", "platform"), "x")
  dat$date <- .cc_as_date(dat$date)
  dat$country <- if ("country" %in% names(dat)) .cc_normalise_country(dat$country) else "global"
  dat$language <- if ("language" %in% names(dat)) as.character(dat$language) else "und"
  keys <- .cc_make_key(dat, c("date", "country", "language", "concept_id"))
  keep_key <- vapply(split(seq_len(nrow(dat)), keys, drop = TRUE), function(ii) {
    p <- unique(as.character(dat$platform[ii]))
    enough <- length(p) >= min_platforms
    required <- if (is.null(required_platforms)) TRUE else all(required_platforms %in% p)
    enough && required
  }, logical(1))
  keep <- keys %in% names(keep_key)[keep_key]
  out <- dat[keep, , drop = FALSE]
  rownames(out) <- NULL
  as_culturomic_tbl(out, dictionary = attr(x, "dictionary"))
}

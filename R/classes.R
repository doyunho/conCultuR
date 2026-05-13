#' Create a conservation culturomics project
#'
#' @param name Project name.
#' @param dictionary Optional concept dictionary.
#' @param traces Optional culturomic trace table.
#' @param events Optional event calendar.
#' @return A culturomic_project object.
#' @export
culturomic_project <- function(name, dictionary = NULL, traces = NULL, events = NULL) {
  structure(
    list(
      name = name,
      dictionary = dictionary,
      traces = traces,
      events = events,
      created = Sys.time()
    ),
    class = "culturomic_project"
  )
}

#' @export
print.culturomic_project <- function(x, ...) {
  cat("culturomic_project\n")
  cat("  name: ", x$name, "\n", sep = "")
  cat("  dictionary rows: ", if (is.null(x$dictionary)) 0 else nrow(x$dictionary), "\n", sep = "")
  cat("  trace rows: ", if (is.null(x$traces)) 0 else nrow(x$traces), "\n", sep = "")
  invisible(x)
}

#' Coerce data to a culturomic trace table
#'
#' @param x Data frame containing digital trace observations.
#' @param dictionary Optional dictionary attached as an attribute.
#' @return A culturomic_tbl object.
#' @export
as_culturomic_tbl <- function(x, dictionary = NULL) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  required <- c("date", "country", "language", "platform", "concept_id", "query", "metric", "raw_value")
  .cc_required_cols(x, required, "x")
  x$date <- .cc_as_date(x$date)
  if (!is.null(dictionary)) {
    dictionary <- as.data.frame(dictionary, stringsAsFactors = FALSE)
    join_cols <- intersect(c("concept_id", "query", "language", "country"), intersect(names(x), names(dictionary)))
    extra_cols <- setdiff(intersect(c("scientific_name", "query_type", "ambiguity_score", "ambiguity_class", "ambiguity_reason", "disambiguation_rule"), names(dictionary)), names(x))
    if (length(join_cols) > 0 && length(extra_cols) > 0) {
      x_key <- .cc_make_key(x, join_cols)
      d_key <- .cc_make_key(dictionary, join_cols)
      first <- !duplicated(d_key)
      d_key <- d_key[first]
      d_sub <- dictionary[first, extra_cols, drop = FALSE]
      idx <- match(x_key, d_key)
      for (col in extra_cols) x[[col]] <- d_sub[[col]][idx]
    }
  }
  if (!"value" %in% names(x)) x$value <- x$raw_value
  if (!"scaled_value" %in% names(x)) x$scaled_value <- NA_real_
  if (!"source" %in% names(x)) x$source <- NA_character_
  if (!"collection_time" %in% names(x)) x$collection_time <- as.character(Sys.time())
  attr(x, "dictionary") <- dictionary
  class(x) <- c("culturomic_tbl", setdiff(class(x), "culturomic_tbl"))
  x
}

#' @export
print.culturomic_tbl <- function(x, ...) {
  cat("culturomic_tbl\n")
  cat("  rows: ", nrow(x), "\n", sep = "")
  cat("  concepts: ", length(unique(x$concept_id)), "\n", sep = "")
  cat("  platforms: ", paste(unique(x$platform), collapse = ", "), "\n", sep = "")
  invisible(x)
}

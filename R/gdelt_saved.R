#' GDELT request URLs for collection in a web browser
#'
#' Lists the GDELT DOC 2.0 request URL for each GDELT query in a dictionary,
#' using the same query selection as [get_gdelt_attention()]. Open each URL
#' in a web browser and save the response as a file; [read_gdelt_json()] then
#' reads the saved files. This route is useful when the API refuses scripted
#' requests but still answers a browser.
#'
#' @inheritParams get_gdelt_attention
#' @return data.frame with order, concept_id, query, a suggested file_name and
#'   the request url. Open the URLs at least five seconds apart.
#' @examples
#' ex <- con_culturomics_example_data()
#' dict <- score_ambiguity(ex$dictionary)
#' gdelt_query_urls(dict, from = "2024-01-01", to = "2024-01-31")
#' @export
gdelt_query_urls <- function(dictionary,
                             from,
                             to,
                             mode = "timelinevolraw",
                             query_types = c("gdelt_query", "common", "scientific"),
                             max_queries_per_concept = 1,
                             max_ambiguity = 0.85,
                             timelinesmooth = NULL) {
  dictionary <- .cc_filter_dictionary_for_platform(dictionary, "gdelt", query_types, max_queries_per_concept, max_ambiguity)
  if (nrow(dictionary) == 0) stop("No GDELT queries remained after filtering.", call. = FALSE)
  n <- nrow(dictionary)
  data.frame(
    order = seq_len(n),
    concept_id = dictionary$concept_id,
    query = dictionary$query,
    file_name = paste0(formatC(seq_len(n), width = nchar(n), flag = "0"), "_", dictionary$concept_id, ".json"),
    url = vapply(dictionary$query, .cc_gdelt_url, character(1), from = from, to = to,
                 mode = mode, timelinesmooth = timelinesmooth, USE.NAMES = FALSE),
    stringsAsFactors = FALSE
  )
}

#' Read GDELT responses saved from a web browser
#'
#' Reads GDELT DOC 2.0 timeline responses that were opened in a browser (see
#' [gdelt_query_urls()]) and saved as files, and returns them in the same form
#' as [get_gdelt_attention()]. Each file is matched to its dictionary query by
#' the query echoed in the response (`query_details$title`), or else by the
#' concept_id in the file name, so files can keep the browser's default names.
#' Files saved as "Webpage, HTML only" are accepted as well as raw JSON.
#'
#' @inheritParams get_gdelt_attention
#' @param path A directory of saved responses, or a vector of file paths.
#' @return culturomic_tbl. attr(, "gdelt_status") records each query as "ok",
#'   "no_articles", "failed" (the file holds an error message, such as the
#'   rate-limit notice, instead of JSON) or "missing" (no file found).
#' @examples
#' ex <- con_culturomics_example_data()
#' dict <- score_ambiguity(ex$dictionary)
#' urls <- gdelt_query_urls(dict, from = "2024-01-01", to = "2024-01-03")
#' dir <- tempfile(); dir.create(dir)
#' writeLines(paste0('{"query_details": {"title": "', gsub('"', '\\\\"', urls$query[1]),
#'                   '"}, "timeline": [{"series": "Article Count", "data": [',
#'                   '{"date": "20240101T000000Z", "value": 3, "norm": 100}]}]}'),
#'            file.path(dir, urls$file_name[1]))
#' read_gdelt_json(dict, dir, from = "2024-01-01", to = "2024-01-03")
#' @export
read_gdelt_json <- function(dictionary,
                            path,
                            from,
                            to,
                            mode = "timelinevolraw",
                            query_types = c("gdelt_query", "common", "scientific"),
                            max_queries_per_concept = 1,
                            max_ambiguity = 0.85,
                            fill_missing_zero = TRUE) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) .cc_stop_missing_pkg("jsonlite")
  dictionary <- .cc_filter_dictionary_for_platform(dictionary, "gdelt", query_types, max_queries_per_concept, max_ambiguity)
  if (nrow(dictionary) == 0) stop("No GDELT queries remained after filtering.", call. = FALSE)
  files <- if (length(path) == 1 && dir.exists(path)) {
    list.files(path, pattern = "\\.(json|txt|html?)$", full.names = TRUE, ignore.case = TRUE)
  } else path
  files <- files[file.exists(files)]
  if (length(files) == 0) stop("No saved GDELT files were found.", call. = FALSE)

  parsed <- lapply(files, .cc_read_saved_gdelt)
  titles <- vapply(parsed, function(p) if (is.null(p$title)) NA_character_ else p$title, character(1))
  stems <- sub("^[0-9]+_", "", tools::file_path_sans_ext(basename(files)))

  metric <- if (tolower(mode) == "timelinevolraw") "news_article_count" else "news_volume_share"
  all_dates <- seq(.cc_as_date(from), .cc_as_date(to), by = "day")
  rows <- list()
  status <- vector("list", nrow(dictionary))
  for (i in seq_len(nrow(dictionary))) {
    hit <- which(!is.na(titles) & titles == dictionary$query[i])
    if (length(hit) == 0) hit <- which(stems == dictionary$concept_id[i])
    if (length(hit) == 0) {
      status[[i]] <- data.frame(concept_id = dictionary$concept_id[i], query = dictionary$query[i], status = "missing",
                                message = "no saved file matched this query", stringsAsFactors = FALSE)
      next
    }
    if (length(hit) > 1) {
      warning("Several saved files match '", dictionary$query[i], "'; using ", basename(files[hit[1]]), call. = FALSE)
      hit <- hit[1]
    }
    p <- parsed[[hit]]
    if (is.null(p$obj)) {
      status[[i]] <- data.frame(concept_id = dictionary$concept_id[i], query = dictionary$query[i], status = "failed",
                                message = substr(paste0(basename(files[hit]), ": ", p$text), 1, 300), stringsAsFactors = FALSE)
      warning("Saved GDELT file for '", dictionary$query[i], "' holds no JSON and was skipped.", call. = FALSE)
      next
    }
    res <- .cc_gdelt_rows(p$obj, dictionary[i, , drop = FALSE], all_dates, metric, fill_missing_zero,
                          source = paste0("gdelt_doc_api_saved:", mode))
    res$status$message <- paste0(basename(files[hit]), ": ", res$status$message)
    status[[i]] <- res$status
    if (!is.null(res$rows)) rows[[length(rows) + 1L]] <- res$rows
  }
  status <- .cc_bind_rows_fill(status)
  if (length(rows) == 0) stop("No usable GDELT rows were found in the saved files.", call. = FALSE)
  out <- as_culturomic_tbl(.cc_bind_rows_fill(rows), dictionary = dictionary)
  attr(out, "gdelt_status") <- status
  out
}

## Reads one saved response. Browsers may save the JSON wrapped in an HTML
## page (<pre>...</pre>), so the wrapper and HTML entities are removed first.
.cc_read_saved_gdelt <- function(file) {
  txt <- paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  if (grepl("<pre", txt, ignore.case = TRUE)) {
    txt <- sub("(?s)^.*?<pre[^>]*>", "", txt, perl = TRUE)
    txt <- sub("(?s)</pre>.*$", "", txt, perl = TRUE)
  }
  txt <- gsub("<[^>]+>", "", txt)
  for (e in list(c("&quot;", "\""), c("&lt;", "<"), c("&gt;", ">"), c("&#39;", "'"), c("&amp;", "&"))) {
    txt <- gsub(e[1], e[2], txt, fixed = TRUE)
  }
  txt <- trimws(txt)
  obj <- tryCatch(jsonlite::fromJSON(txt, flatten = TRUE), error = function(e) NULL)
  title <- if (is.list(obj) && !is.null(obj$query_details$title)) as.character(obj$query_details$title) else NULL
  list(obj = obj, title = title, text = substr(txt, 1, 200))
}

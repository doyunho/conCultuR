.cc_live_user_agent <- function() {
  "conCultuR/0.9.0 (conservation culturomics R package; contact: doyunho@gmail.com)"
}

.cc_fetch_json <- function(url, user_agent = .cc_live_user_agent(), retries = 0) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) .cc_stop_missing_pkg("jsonlite")
  if (requireNamespace("httr2", quietly = TRUE)) {
    req <- httr2::request(url)
    req <- httr2::req_user_agent(req, user_agent)
    req <- httr2::req_timeout(req, 60)
    if (retries > 0) {
      ## Rate-limited (429) and transient (503) responses are retried with backoff.
      req <- httr2::req_retry(req, max_tries = retries + 1, backoff = function(i) 10 * i)
    }
    resp <- httr2::req_perform(req)
    status <- httr2::resp_status(resp)
    if (status >= 400) {
      stop("HTTP request failed with status ", status, ": ", url, call. = FALSE)
    }
    txt <- httr2::resp_body_string(resp)
    jsonlite::fromJSON(txt, flatten = TRUE)
  } else {
    jsonlite::fromJSON(url, flatten = TRUE)
  }
}

.cc_sleep <- function(seconds) Sys.sleep(seconds)

.cc_normalise_country <- function(x) {
  x <- as.character(x)
  x[is.na(x) | !nzchar(x)] <- "global"
  x
}


.cc_merge_args <- function(...) {
  parts <- list(...)
  out <- list()
  for (part in parts) {
    if (is.null(part) || length(part) == 0) next
    nms <- names(part)
    if (is.null(nms)) nms <- rep("", length(part))
    for (i in seq_along(part)) {
      nm <- nms[[i]]
      if (is.na(nm) || !nzchar(nm)) {
        out[[length(out) + 1L]] <- part[[i]]
      } else {
        out[[nm]] <- part[[i]]
      }
    }
  }
  out
}

.cc_platform_alias <- function(platform) {
  p <- tolower(as.character(platform))
  p[p == "wiki"] <- "wikipedia"
  p[p == "google"] <- "google_trends"
  p
}

.cc_filter_dictionary_for_platform <- function(dictionary,
                                               platform,
                                               query_types = NULL,
                                               max_queries_per_concept = 1,
                                               max_ambiguity = 0.85,
                                               respect_platform_column = TRUE) {
  dictionary <- as.data.frame(dictionary, stringsAsFactors = FALSE)
  .cc_required_cols(dictionary, c("concept_id", "query", "language", "country"), "dictionary")
  if (!"query_type" %in% names(dictionary)) dictionary$query_type <- "query"
  if (!"ambiguity_score" %in% names(dictionary)) dictionary$ambiguity_score <- NA_real_
  dictionary$country <- .cc_normalise_country(dictionary$country)

  p <- .cc_platform_alias(platform)
  if (isTRUE(respect_platform_column) && "platform" %in% names(dictionary)) {
    dp <- .cc_platform_alias(dictionary$platform)
    keep <- is.na(dictionary$platform) | !nzchar(as.character(dictionary$platform)) | dp == p
    dictionary <- dictionary[keep, , drop = FALSE]
  }
  if (!is.null(query_types)) {
    dictionary <- dictionary[tolower(dictionary$query_type) %in% tolower(query_types), , drop = FALSE]
  }
  amb <- suppressWarnings(as.numeric(dictionary$ambiguity_score))
  keep_amb <- is.na(amb) | amb <= max_ambiguity
  dictionary <- dictionary[keep_amb, , drop = FALSE]
  dictionary <- dictionary[!is.na(dictionary$query) & nzchar(as.character(dictionary$query)), , drop = FALSE]
  if (nrow(dictionary) == 0) return(dictionary)

  priority <- rep(50, nrow(dictionary))
  qt <- tolower(as.character(dictionary$query_type))
  priority[qt %in% c("wikipedia_title", "gdelt_query", "google_query", "youtube_query", "reddit_query")] <- 1
  priority[qt == "scientific"] <- 2
  priority[qt == "common"] <- 3
  priority[qt == "translated_common"] <- 4
  priority[qt == "synonym"] <- 5
  priority[qt == "hashtag"] <- 6
  amb2 <- suppressWarnings(as.numeric(dictionary$ambiguity_score))
  amb2[is.na(amb2)] <- 0
  dictionary$.cc_priority <- priority
  dictionary$.cc_amb <- amb2
  dictionary$.cc_order <- seq_len(nrow(dictionary))
  dictionary <- dictionary[order(dictionary$concept_id, dictionary$.cc_priority, dictionary$.cc_amb, dictionary$.cc_order), , drop = FALSE]
  if (is.finite(max_queries_per_concept) && max_queries_per_concept > 0) {
    dictionary <- .cc_bind_rows_fill(lapply(split(dictionary, dictionary$concept_id), function(z) {
      z[seq_len(min(nrow(z), max_queries_per_concept)), , drop = FALSE]
    }))
  }
  dictionary$.cc_priority <- NULL
  dictionary$.cc_amb <- NULL
  dictionary$.cc_order <- NULL
  rownames(dictionary) <- NULL
  dictionary
}

#' Check dependencies for live collection
#'
#' @param platforms Platforms to check.
#' @return Data frame with dependency status.
#' @examples
#' check_live_dependencies(c("wikipedia", "gdelt", "google"))
#' @export
check_live_dependencies <- function(platforms = c("wikipedia", "gdelt", "google", "reddit", "youtube")) {
  platforms <- unique(.cc_platform_alias(platforms))
  rows <- lapply(platforms, function(p) {
    required_pkg <- switch(p,
      wikipedia = "jsonlite",
      gdelt = "jsonlite",
      google_trends = "gtrendsR",
      reddit = "jsonlite",
      youtube = "jsonlite",
      NA_character_
    )
    optional_pkg <- if (p %in% c("wikipedia", "gdelt", "reddit", "youtube")) "httr2" else NA_character_
    api_key <- if (p == "youtube") "YOUTUBE_API_KEY" else NA_character_
    data.frame(
      platform = p,
      required_package = required_pkg,
      required_package_installed = if (is.na(required_pkg)) NA else requireNamespace(required_pkg, quietly = TRUE),
      optional_package = optional_pkg,
      optional_package_installed = if (is.na(optional_pkg)) NA else requireNamespace(optional_pkg, quietly = TRUE),
      api_key_envvar = api_key,
      api_key_available = if (is.na(api_key)) NA else nzchar(Sys.getenv(api_key)),
      install_hint = if (!is.na(required_pkg) && !requireNamespace(required_pkg, quietly = TRUE)) paste0("install.packages(\"", required_pkg, "\")") else "ready",
      stringsAsFactors = FALSE
    )
  })
  .cc_bind_rows_fill(rows)
}

#' Install optional live-data dependencies
#'
#' @param platforms Platforms whose dependencies should be installed.
#' @param repos CRAN repository.
#' @return Invisibly returns dependency status after installation attempts.
#' @export
install_live_dependencies <- function(platforms = c("google"), repos = "https://cloud.r-project.org") {
  deps <- check_live_dependencies(platforms)
  pkgs <- unique(deps$required_package[!is.na(deps$required_package) & !deps$required_package_installed])
  if (length(pkgs) > 0) {
    utils::install.packages(pkgs, repos = repos)
  }
  invisible(check_live_dependencies(platforms))
}

#' Check whether Google Trends live collection is ready
#'
#' @param auto_install If TRUE, try to install gtrendsR from CRAN when missing.
#' @return TRUE if gtrendsR is available.
#' @export
google_trends_ready <- function(auto_install = FALSE) {
  ok <- requireNamespace("gtrendsR", quietly = TRUE)
  if (!ok && isTRUE(auto_install)) {
    try(utils::install.packages("gtrendsR", repos = "https://cloud.r-project.org"), silent = TRUE)
    ok <- requireNamespace("gtrendsR", quietly = TRUE)
  }
  ok
}


.cc_google_gprop <- function(gprop) {
  gprop <- as.character(gprop)[1]
  if (is.na(gprop) || !nzchar(gprop)) return("web")
  allowed <- c("web", "news", "images", "froogle", "youtube")
  if (!tolower(gprop) %in% allowed) {
    warning("Unknown Google Trends gprop '", gprop, "'. Using 'web'.", call. = FALSE)
    return("web")
  }
  tolower(gprop)
}

.cc_google_time_window <- function(from, to, time = NULL, prefer_presets = TRUE) {
  if (!is.null(time) && length(time) > 0 && !is.na(time[1]) && nzchar(as.character(time[1]))) {
    return(as.character(time[1]))
  }
  from_d <- .cc_as_date(from)
  to_d <- .cc_as_date(to)
  span <- as.integer(to_d - from_d) + 1L
  ## Presets are often more reliable in gtrendsR than very recent custom date
  ## windows, but a preset ends today, so use one only if it still reaches `from`.
  if (isTRUE(prefer_presets) && is.finite(span) && to_d >= Sys.Date() - 10) {
    days_back <- as.integer(Sys.Date() - from_d)
    if (days_back <= 29) return("today 1-m")
    if (days_back <= 89) return("today 3-m")
    ## Google returns daily values only up to about 269 days, so a shorter
    ## window that the 3-month preset misses keeps its custom dates.
    if (span > 269 && days_back <= 364) return("today 12-m")
  }
  paste(as.character(from_d), as.character(to_d))
}

.cc_extract_interest <- function(gt) {
  if (is.null(gt)) return(NULL)
  if (is.data.frame(gt) && all(c("date", "hits") %in% names(gt))) {
    return(gt)
  }
  if (is.list(gt) && !is.null(gt[["interest_over_time"]]) && is.data.frame(gt[["interest_over_time"]])) {
    return(gt[["interest_over_time"]])
  }
  ## Some gtrendsR versions or failed calls can return atomic vectors, lists
  ## without interest_over_time, or other objects. Do not use `$` on those.
  NULL
}

.cc_gtrends_call <- function(keywords, geo = "", time = "today 1-m", category = 0, gprop = "web", hl = "en-US", low_search_volume = TRUE) {
  keywords <- unique(as.character(keywords))
  keywords <- keywords[nzchar(keywords)]
  if (length(keywords) == 0) return(structure("no keywords", class = "try-error"))
  gprop <- .cc_google_gprop(gprop)
  attempts <- list(
    list(keyword = keywords, geo = geo, time = time, category = category, gprop = gprop, hl = hl, low_search_volume = low_search_volume, onlyInterest = TRUE),
    list(keyword = keywords, geo = geo, time = time, category = category, gprop = gprop, hl = hl, onlyInterest = TRUE),
    list(keyword = keywords, geo = geo, time = time, category = category, gprop = gprop, onlyInterest = TRUE),
    list(keyword = keywords, geo = geo, time = time, category = category, gprop = gprop),
    list(keyword = keywords, geo = geo, time = time, category = category),
    list(keyword = keywords, geo = geo, time = time),
    list(keyword = keywords, time = time)
  )
  last <- NULL
  for (args in attempts) {
    ans <- try(do.call(gtrendsR::gtrends, args), silent = TRUE)
    dat <- .cc_extract_interest(ans)
    if (!inherits(ans, "try-error") && !is.null(dat) && nrow(dat) > 0) return(ans)
    last <- ans
  }
  last
}

#' Diagnose Google Trends live access
#'
#' Runs a minimal high-volume Google Trends query through gtrendsR. Use this
#' before live collection when Google Trends returns no rows.
#'
#' @param keyword Test keyword, default conservation.
#' @param geo Google Trends geography code, default global.
#' @param time Google Trends time string such as today 1-m.
#' @param gprop Google Trends property. Empty values are treated as web.
#' @param hl Interface language passed to gtrendsR.
#' @return Data frame describing whether gtrendsR returned interest-over-time rows.
#' @examples
#' \dontrun{
#' diagnose_google_trends("conservation", time = "today 1-m")
#' }
#' @export
diagnose_google_trends <- function(keyword = "conservation",
                                   geo = "",
                                   time = "today 1-m",
                                   gprop = "web",
                                   hl = "en-US") {
  if (!google_trends_ready()) {
    return(data.frame(
      ok = FALSE,
      keyword = keyword,
      rows = 0L,
      message = "gtrendsR is not installed",
      stringsAsFactors = FALSE
    ))
  }
  ans <- .cc_gtrends_call(keyword, geo = geo, time = time, gprop = gprop, hl = hl)
  dat <- .cc_extract_interest(ans)
  if (inherits(ans, "try-error") || is.null(dat) || nrow(dat) == 0) {
    msg <- if (inherits(ans, "try-error")) as.character(ans) else "gtrendsR returned no interest_over_time rows"
    return(data.frame(
      ok = FALSE,
      keyword = keyword,
      rows = 0L,
      message = substr(msg, 1, 500),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    ok = TRUE,
    keyword = keyword,
    rows = nrow(dat),
    date_min = as.character(min(.cc_as_date(dat$date), na.rm = TRUE)),
    date_max = as.character(max(.cc_as_date(dat$date), na.rm = TRUE)),
    message = "Google Trends returned data",
    stringsAsFactors = FALSE
  )
}

#' Preview live collection requests
#'
#' @param dictionary Concept dictionary.
#' @param platforms Platforms to preview.
#' @param max_queries_per_concept Maximum queries per concept and platform.
#' @return Data frame of planned API queries.
#' @export
live_collection_plan <- function(dictionary,
                                 platforms = c("wikipedia", "gdelt"),
                                 max_queries_per_concept = 1) {
  plans <- list()
  for (platform in platforms) {
    p <- .cc_platform_alias(platform)
    qt <- switch(p,
      wikipedia = c("wikipedia_title", "common", "scientific"),
      gdelt = c("gdelt_query", "common", "scientific"),
      google_trends = c("google_query", "common", "scientific"),
      reddit = c("reddit_query", "common", "scientific"),
      youtube = c("youtube_query", "common", "scientific"),
      c("common", "scientific")
    )
    d <- .cc_filter_dictionary_for_platform(dictionary, p, query_types = qt, max_queries_per_concept = max_queries_per_concept)
    if (nrow(d) > 0) {
      d$planned_platform <- p
      plans[[p]] <- d[c("planned_platform", "concept_id", "scientific_name", "query", "query_type", "language", "country")]
    }
  }
  if (length(plans) == 0) return(data.frame())
  out <- .cc_bind_rows_fill(plans)
  rownames(out) <- NULL
  out
}

.cc_numeric_hits <- function(x) {
  x <- as.character(x)
  x[x %in% c("<1", "< 1", "*")] <- "0.5"
  suppressWarnings(as.numeric(x))
}

#' Collect Google Trends attention
#'
#' Collects Google Trends interest-over-time data through gtrendsR. Google Trends
#' values are relative, so the live collector supports batched collection and an
#' optional anchor keyword. If more than five target queries are collected, use
#' `anchor_keyword` to improve comparability across batches.
#'
#' @param dictionary Concept dictionary.
#' @param from Start date.
#' @param to End date.
#' @param geo Google Trends geography code. Use "" for global.
#' @param mock Return synthetic data without API calls.
#' @param granularity day, week, or month for mock data.
#' @param query_types Query types to collect when live.
#' @param max_queries_per_concept Maximum queries per concept when live.
#' @param max_ambiguity Exclude more ambiguous queries when live.
#' @param batch Collect up to five keywords per gtrendsR request. With
#'   batch = FALSE and no anchor_keyword, each series is scaled to its own
#'   maximum and levels are not comparable across concepts.
#' @param batch_size Maximum number of Google Trends keywords per request.
#' @param anchor_keyword Optional keyword added to every request (batches and
#'   single-keyword retries). Each request is rescaled so that the anchor's
#'   median matches its median in the first request.
#' @param sleep Seconds to wait between gtrendsR calls.
#' @param category Google Trends category, passed to gtrendsR.
#' @param gprop Google Trends property, passed to gtrendsR.
#' @param time Optional Google Trends time string. If NULL, from and to are used.
#' @param prefer_presets Prefer Google Trends preset windows for recent date ranges.
#' @param hl Interface language passed to gtrendsR.
#' @param low_search_volume Keep low-volume results rather than dropping them.
#' @param auto_install If TRUE, try to install gtrendsR from CRAN when missing.
#' @param fallback_single Retry individual keywords when batch requests fail.
#' @param verbose Print diagnostic messages during live collection.
#' @param return_diagnostics Return a diagnostics data frame instead of stopping when no rows are returned.
#' @return culturomic_tbl.
#' @examples
#' ex <- con_culturomics_example_data()
#' dict <- score_ambiguity(ex$dictionary)
#' get_google_attention(dict, from = "2024-01-01", to = "2024-01-31", mock = TRUE)
#' \dontrun{
#' get_google_attention(dict, from = "2024-01-01", to = "2024-03-31",
#'                      batch = TRUE, anchor_keyword = "wildlife", sleep = 3)
#' }
#' @export
get_google_attention <- function(dictionary,
                                 from,
                                 to,
                                 geo = "",
                                 mock = FALSE,
                                 granularity = "day",
                                 query_types = c("google_query", "common", "scientific"),
                                 max_queries_per_concept = 1,
                                 max_ambiguity = 0.85,
                                 batch = TRUE,
                                 batch_size = 5,
                                 anchor_keyword = NULL,
                                 sleep = 1,
                                 category = 0,
                                 gprop = "web",
                                 time = NULL,
                                 prefer_presets = TRUE,
                                 hl = "en-US",
                                 low_search_volume = TRUE,
                                 auto_install = FALSE,
                                 fallback_single = TRUE,
                                 verbose = FALSE,
                                 return_diagnostics = FALSE) {
  if (isTRUE(mock)) {
    return(.cc_mock_attention(dictionary, "google_trends", "relative_search_interest", from, to, granularity, seed = 101))
  }
  if (!google_trends_ready(auto_install = auto_install)) {
    stop("Package 'gtrendsR' is required for Google Trends live collection. Run install.packages('gtrendsR') or install_live_dependencies('google'), then retry.", call. = FALSE)
  }
  dictionary <- .cc_filter_dictionary_for_platform(dictionary, "google_trends", query_types, max_queries_per_concept, max_ambiguity)
  if (nrow(dictionary) == 0) stop("No Google Trends queries remained after filtering.", call. = FALSE)
  dictionary$country <- .cc_normalise_country(dictionary$country)
  time_window <- .cc_google_time_window(from, to, time = time, prefer_presets = prefer_presets)
  gprop <- .cc_google_gprop(gprop)
  batch_size <- max(1L, min(5L, as.integer(batch_size)))

  diagnostics <- list()
  call_gtrends <- function(keywords, geo_i) {
    ans <- .cc_gtrends_call(keywords, geo = geo_i, time = time_window, category = category, gprop = gprop, hl = hl, low_search_volume = low_search_volume)
    if (inherits(ans, "try-error")) {
      diagnostics[[length(diagnostics) + 1L]] <<- data.frame(query = paste(keywords, collapse = " | "), geo = geo_i, time = time_window, gprop = gprop, status = "failed", message = substr(as.character(ans), 1, 500), stringsAsFactors = FALSE)
    } else {
      dat <- .cc_extract_interest(ans)
      diagnostics[[length(diagnostics) + 1L]] <<- data.frame(query = paste(keywords, collapse = " | "), geo = geo_i, time = time_window, gprop = gprop, status = if (!is.null(dat) && nrow(dat) > 0) "ok" else "empty", message = "", stringsAsFactors = FALSE)
    }
    ans
  }

  make_rows <- function(dat, dict_batch, batch_id = 1L, anchor_scale = 1, anchor_used = NA_character_) {
    if (is.null(dat) || !is.data.frame(dat) || nrow(dat) == 0) return(NULL)
    if (!"keyword" %in% names(dat)) dat$keyword <- dict_batch$query[1]
    dat$keyword <- as.character(dat$keyword)
    out <- list()
    kk <- 1L
    for (j in seq_len(nrow(dict_batch))) {
      q <- as.character(dict_batch$query[j])
      z <- dat[dat$keyword == q, , drop = FALSE]
      if (nrow(z) == 0) next
      vals <- .cc_numeric_hits(z$hits) * anchor_scale
      out[[kk]] <- data.frame(
        date = .cc_as_date(z$date),
        country = .cc_normalise_country(dict_batch$country[j]),
        language = dict_batch$language[j],
        platform = "google_trends",
        concept_id = dict_batch$concept_id[j],
        query = q,
        metric = "relative_search_interest",
        raw_value = vals,
        value = vals,
        source = ifelse(is.na(anchor_used), "gtrendsR_live", paste0("gtrendsR_live_anchor:", anchor_used)),
        collection_time = as.character(Sys.time()),
        google_trends_batch_id = batch_id,
        google_anchor_scale = anchor_scale,
        stringsAsFactors = FALSE
      )
      kk <- kk + 1L
    }
    if (length(out) == 0) NULL else .cc_bind_rows_fill(out)
  }

  rows <- list()
  k <- 1L

  ## Google Trends geo is request-level. If country codes are supplied in the
  ## dictionary, run separate requests by country. "global" maps to geo = "".
  if (identical(geo, "auto")) {
    geos <- unique(.cc_normalise_country(dictionary$country))
  } else {
    dictionary$.cc_geo <- geo
    geos <- geo
  }
  if (!".cc_geo" %in% names(dictionary)) {
    dictionary$.cc_geo <- ifelse(tolower(dictionary$country) %in% c("global", "world", ""), "", toupper(dictionary$country))
  }

  for (geo_i in unique(dictionary$.cc_geo)) {
    dgeo <- dictionary[dictionary$.cc_geo == geo_i, , drop = FALSE]
    if (nrow(dgeo) == 0) next
    dgeo$.cc_row <- seq_len(nrow(dgeo))

    use_anchor <- !is.null(anchor_keyword) && nzchar(anchor_keyword)
    if (!isTRUE(batch) && !use_anchor) {
      if (nrow(dgeo) > 1) {
        warning("Google Trends keywords were requested one at a time without anchor_keyword. Each series is scaled to its own maximum, so levels are not comparable across concepts.", call. = FALSE)
      }
      for (i in seq_len(nrow(dgeo))) {
        gt <- call_gtrends(dgeo$query[i], geo_i)
        dat_gt <- .cc_extract_interest(gt)
        if (inherits(gt, "try-error") || is.null(dat_gt) || nrow(dat_gt) == 0) next
        rr <- make_rows(dat_gt, dgeo[i, , drop = FALSE], batch_id = i)
        if (!is.null(rr)) { rows[[k]] <- rr; k <- k + 1L }
        if (sleep > 0) Sys.sleep(sleep)
      }
    } else {
      ## batch = FALSE with an anchor requests each keyword paired with the anchor.
      target_per_batch <- if (!isTRUE(batch)) 1L else if (use_anchor) max(1L, batch_size - 1L) else batch_size
      idx <- split(seq_len(nrow(dgeo)), ceiling(seq_len(nrow(dgeo)) / target_per_batch))
      reference_anchor <- NA_real_
      batch_id <- 1L
      ## Rescales a request to the first batch through the anchor keyword's median.
      anchor_scale_for <- function(dat) {
        if (!use_anchor || !"keyword" %in% names(dat) || !anchor_keyword %in% dat$keyword) return(NA_real_)
        anchor_med <- stats::median(.cc_numeric_hits(dat$hits[dat$keyword == anchor_keyword]), na.rm = TRUE)
        if (!is.finite(anchor_med) || anchor_med <= 0) return(NA_real_)
        if (!is.finite(reference_anchor)) reference_anchor <<- anchor_med
        reference_anchor / anchor_med
      }
      for (ids in idx) {
        db <- dgeo[ids, , drop = FALSE]
        keywords <- unique(as.character(db$query))
        if (use_anchor) keywords <- unique(c(keywords, anchor_keyword))
        keywords <- keywords[seq_len(min(length(keywords), batch_size))]
        gt <- call_gtrends(keywords, geo_i)
        dat_gt <- .cc_extract_interest(gt)
        if (inherits(gt, "try-error") || is.null(dat_gt) || nrow(dat_gt) == 0) {
          if (isTRUE(fallback_single)) {
            if (isTRUE(verbose)) message("Google Trends batch failed; retrying single keywords for batch ", batch_id, ".")
            for (ii in seq_len(nrow(db))) {
              ## In anchor mode each retry keeps the anchor so the keyword stays on the common scale.
              kw1 <- as.character(db$query[ii])
              if (use_anchor) kw1 <- unique(c(kw1, anchor_keyword))
              gt1 <- call_gtrends(kw1, geo_i)
              dat_gt1 <- .cc_extract_interest(gt1)
              if (inherits(gt1, "try-error") || is.null(dat_gt1) || nrow(dat_gt1) == 0) next
              sf1 <- anchor_scale_for(dat_gt1)
              if (use_anchor && !is.finite(sf1)) {
                warning("Google Trends single-keyword retry for '", db$query[ii], "' returned no usable anchor values; keyword skipped.", call. = FALSE)
                next
              }
              rr1 <- make_rows(dat_gt1, db[ii, , drop = FALSE], batch_id = batch_id,
                               anchor_scale = if (use_anchor) sf1 else 1,
                               anchor_used = if (use_anchor) anchor_keyword else NA_character_)
              if (!is.null(rr1)) { rows[[k]] <- rr1; k <- k + 1L }
              if (sleep > 0) Sys.sleep(sleep)
            }
          }
          batch_id <- batch_id + 1L
          next
        }
        dat <- dat_gt
        scale_factor <- 1
        if (use_anchor) {
          sf <- anchor_scale_for(dat)
          if (is.finite(sf)) {
            scale_factor <- sf
          } else {
            warning("Google Trends batch ", batch_id, " returned no usable anchor values; values in this batch are not rescaled.", call. = FALSE)
          }
        } else if (length(idx) > 1) {
          warning("Google Trends was collected in multiple batches without anchor_keyword. Values are relative within each batch and should be compared cautiously.", call. = FALSE)
        }
        rr <- make_rows(dat, db, batch_id = batch_id, anchor_scale = scale_factor, anchor_used = if (use_anchor) anchor_keyword else NA_character_)
        if (!is.null(rr)) { rows[[k]] <- rr; k <- k + 1L }
        batch_id <- batch_id + 1L
        if (sleep > 0) Sys.sleep(sleep)
      }
    }
  }

  if (length(rows) == 0) {
    diag <- if (length(diagnostics) > 0) .cc_bind_rows_fill(diagnostics) else data.frame()
    if (isTRUE(return_diagnostics)) return(diag)
    stop("No Google Trends rows were returned. First run diagnose_google_trends('conservation'). If that fails, Google/gtrendsR is blocked or rate-limited in this session. If it succeeds, retry with gprop = 'web', time = 'today 1-m', batch = FALSE, fallback_single = TRUE, and higher-volume queries.", call. = FALSE)
  }
  out <- .cc_bind_rows_fill(rows)
  out$.cc_geo <- NULL
  as_culturomic_tbl(out, dictionary = dictionary)
}

#' Collect Wikipedia pageviews
#'
#' @param dictionary Concept dictionary.
#' @param from Start date.
#' @param to End date.
#' @param project Wikimedia project or auto for language-specific projects.
#' @param access Access type.
#' @param agent Agent type.
#' @param mock Return synthetic data without API calls.
#' @param query_types Query types to collect when live.
#' @param max_queries_per_concept Maximum queries per concept when live.
#' @param max_ambiguity Exclude more ambiguous queries when live.
#' @param user_agent User agent sent to Wikimedia when httr2 is installed.
#' @return culturomic_tbl.
#' @examples
#' ex <- con_culturomics_example_data()
#' dict <- score_ambiguity(ex$dictionary)
#' get_wiki_attention(dict, from = "2024-01-01", to = "2024-01-31", mock = TRUE)
#' \dontrun{
#' get_wiki_attention(dict, from = "2024-01-01", to = "2024-01-31")
#' }
#' @export
get_wiki_attention <- function(dictionary,
                               from,
                               to,
                               project = "auto",
                               access = "all-access",
                               agent = "user",
                               mock = FALSE,
                               query_types = c("wikipedia_title", "common", "scientific"),
                               max_queries_per_concept = 1,
                               max_ambiguity = 0.85,
                               user_agent = .cc_live_user_agent()) {
  if (isTRUE(mock)) {
    return(.cc_mock_attention(dictionary, "wikipedia", "pageviews", from, to, "day", seed = 202))
  }
  dictionary <- .cc_filter_dictionary_for_platform(dictionary, "wikipedia", query_types, max_queries_per_concept, max_ambiguity)
  if (nrow(dictionary) == 0) stop("No Wikipedia queries remained after filtering.", call. = FALSE)
  start <- format(.cc_as_date(from), "%Y%m%d")
  end <- format(.cc_as_date(to), "%Y%m%d")
  rows <- list()
  k <- 1
  for (i in seq_len(nrow(dictionary))) {
    lang <- ifelse(is.na(dictionary$language[i]) || !nzchar(dictionary$language[i]), "en", dictionary$language[i])
    project_i <- if (identical(project, "auto")) paste0(lang, ".wikipedia.org") else project
    if ("wiki_project" %in% names(dictionary) && !is.na(dictionary$wiki_project[i]) && nzchar(dictionary$wiki_project[i])) {
      project_i <- dictionary$wiki_project[i]
    }
    article <- utils::URLencode(gsub(" ", "_", dictionary$query[i]), reserved = TRUE)
    url <- paste0(
      "https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/",
      project_i, "/", access, "/", agent, "/", article, "/daily/", start, "/", end
    )
    obj <- try(.cc_fetch_json(url, user_agent = user_agent), silent = TRUE)
    if (inherits(obj, "try-error") || is.null(obj$items)) next
    dat <- obj$items
    if (!is.data.frame(dat) || nrow(dat) == 0) next
    rows[[k]] <- data.frame(
      date = as.Date(substr(as.character(dat$timestamp), 1, 8), format = "%Y%m%d"),
      country = .cc_normalise_country(dictionary$country[i]),
      language = dictionary$language[i],
      platform = "wikipedia",
      concept_id = dictionary$concept_id[i],
      query = dictionary$query[i],
      metric = "pageviews",
      raw_value = as.numeric(dat$views),
      value = as.numeric(dat$views),
      source = paste0("wikimedia_pageviews_api:", project_i),
      collection_time = as.character(Sys.time()),
      stringsAsFactors = FALSE
    )
    k <- k + 1
  }
  if (length(rows) == 0) stop("No Wikipedia rows were returned. Check article titles and project codes.", call. = FALSE)
  as_culturomic_tbl(.cc_bind_rows_fill(rows), dictionary = dictionary)
}

#' Collect Reddit attention
#'
#' @param dictionary Concept dictionary.
#' @param from Start date retained in metadata.
#' @param to End date retained in metadata.
#' @param limit Number of search results per query.
#' @param mock Return synthetic data without API calls.
#' @param query_types Query types to collect when live.
#' @param max_queries_per_concept Maximum queries per concept when live.
#' @param max_ambiguity Exclude more ambiguous queries when live.
#' @return culturomic_tbl.
#' @export
get_reddit_attention <- function(dictionary,
                                 from,
                                 to,
                                 limit = 100,
                                 mock = FALSE,
                                 query_types = c("reddit_query", "common", "scientific"),
                                 max_queries_per_concept = 1,
                                 max_ambiguity = 0.85) {
  if (isTRUE(mock)) {
    return(.cc_mock_attention(dictionary, "reddit", "mentions", from, to, "day", seed = 303))
  }
  dictionary <- .cc_filter_dictionary_for_platform(dictionary, "reddit", query_types, max_queries_per_concept, max_ambiguity)
  if (nrow(dictionary) == 0) stop("No Reddit queries remained after filtering.", call. = FALSE)
  rows <- list()
  k <- 1
  for (i in seq_len(nrow(dictionary))) {
    url <- paste0("https://www.reddit.com/search.json?q=", utils::URLencode(dictionary$query[i], reserved = TRUE), "&limit=", limit)
    obj <- try(.cc_fetch_json(url), silent = TRUE)
    if (inherits(obj, "try-error")) next
    n <- if (is.null(obj$data$children)) 0 else length(obj$data$children)
    rows[[k]] <- data.frame(
      date = .cc_as_date(to),
      country = .cc_normalise_country(dictionary$country[i]),
      language = dictionary$language[i],
      platform = "reddit",
      concept_id = dictionary$concept_id[i],
      query = dictionary$query[i],
      metric = "snapshot_search_hits",
      raw_value = n,
      value = n,
      source = "reddit_public_search_snapshot",
      collection_time = as.character(Sys.time()),
      stringsAsFactors = FALSE
    )
    k <- k + 1
  }
  if (length(rows) == 0) stop("No Reddit rows were returned.", call. = FALSE)
  as_culturomic_tbl(.cc_bind_rows_fill(rows), dictionary = dictionary)
}

#' Collect YouTube attention
#'
#' @param dictionary Concept dictionary.
#' @param from Start date.
#' @param to End date.
#' @param api_key YouTube Data API key.
#' @param max_results Maximum results per query.
#' @param mock Return synthetic data without API calls.
#' @param query_types Query types to collect when live.
#' @param max_queries_per_concept Maximum queries per concept when live.
#' @param max_ambiguity Exclude more ambiguous queries when live.
#' @return culturomic_tbl.
#' @export
get_youtube_attention <- function(dictionary,
                                  from,
                                  to,
                                  api_key = Sys.getenv("YOUTUBE_API_KEY"),
                                  max_results = 50,
                                  mock = FALSE,
                                  query_types = c("youtube_query", "common", "scientific"),
                                  max_queries_per_concept = 1,
                                  max_ambiguity = 0.85) {
  if (isTRUE(mock)) {
    return(.cc_mock_attention(dictionary, "youtube", "video_search_hits", from, to, "day", seed = 404))
  }
  if (!nzchar(api_key)) stop("A YouTube Data API key is required. Set api_key or YOUTUBE_API_KEY.", call. = FALSE)
  dictionary <- .cc_filter_dictionary_for_platform(dictionary, "youtube", query_types, max_queries_per_concept, max_ambiguity)
  if (nrow(dictionary) == 0) stop("No YouTube queries remained after filtering.", call. = FALSE)
  rows <- list()
  k <- 1
  for (i in seq_len(nrow(dictionary))) {
    url <- paste0(
      "https://www.googleapis.com/youtube/v3/search?part=snippet&type=video&maxResults=",
      max_results, "&q=", utils::URLencode(dictionary$query[i], reserved = TRUE),
      "&publishedAfter=", format(.cc_as_date(from), "%Y-%m-%dT00:00:00Z"),
      "&publishedBefore=", format(.cc_as_date(to), "%Y-%m-%dT23:59:59Z"),
      "&key=", api_key
    )
    obj <- try(.cc_fetch_json(url), silent = TRUE)
    if (inherits(obj, "try-error")) next
    total <- if (!is.null(obj$pageInfo$totalResults)) obj$pageInfo$totalResults else if (!is.null(obj$items)) nrow(obj$items) else 0
    rows[[k]] <- data.frame(
      date = .cc_as_date(to),
      country = .cc_normalise_country(dictionary$country[i]),
      language = dictionary$language[i],
      platform = "youtube",
      concept_id = dictionary$concept_id[i],
      query = dictionary$query[i],
      metric = "video_search_hits",
      raw_value = as.numeric(total),
      value = as.numeric(total),
      source = "youtube_data_api_snapshot",
      collection_time = as.character(Sys.time()),
      stringsAsFactors = FALSE
    )
    k <- k + 1
  }
  if (length(rows) == 0) stop("No YouTube rows were returned.", call. = FALSE)
  as_culturomic_tbl(.cc_bind_rows_fill(rows), dictionary = dictionary)
}

.cc_parse_gdelt_date <- function(x) {
  digits <- gsub("[^0-9]", "", as.character(x))
  digits <- substr(digits, 1, 8)
  as.Date(digits, format = "%Y%m%d")
}

#' Collect GDELT attention
#'
#' @param dictionary Concept dictionary.
#' @param from Start date. GDELT DOC timeline queries are best suited to recent windows.
#' @param to End date.
#' @param mode GDELT API mode, usually timelinevolraw or timelinevol.
#' @param mock Return synthetic data without API calls.
#' @param query_types Query types to collect when live.
#' @param max_queries_per_concept Maximum queries per concept when live.
#' @param max_ambiguity Exclude more ambiguous queries when live.
#' @param timelinesmooth Optional GDELT smoothing window.
#' @param fill_missing_zero Fill dates absent from a successful GDELT timeline
#'   with zero (GDELT omits days without matching articles). Failed requests
#'   are never zero-filled: they are skipped with a warning.
#' @param sleep Seconds to wait between requests. GDELT allows about one
#'   request every 5 seconds per client.
#' @param retries Number of retries for rate-limited (HTTP 429) or transient
#'   failures.
#' @param user_agent User agent sent with each request.
#' @return culturomic_tbl. attr(, "gdelt_status") records the outcome of each
#'   query ("ok", "no_articles" or "failed").
#' @examples
#' ex <- con_culturomics_example_data()
#' dict <- score_ambiguity(ex$dictionary)
#' get_gdelt_attention(dict, from = "2024-01-01", to = "2024-01-31", mock = TRUE)
#' \dontrun{
#' get_gdelt_attention(dict, from = "2024-01-01", to = "2024-01-31", mode = "timelinevolraw")
#' }
#' @export
get_gdelt_attention <- function(dictionary,
                                from,
                                to,
                                mode = "timelinevolraw",
                                mock = FALSE,
                                query_types = c("gdelt_query", "common", "scientific"),
                                max_queries_per_concept = 1,
                                max_ambiguity = 0.85,
                                timelinesmooth = NULL,
                                fill_missing_zero = TRUE,
                                sleep = 6,
                                retries = 3,
                                user_agent = .cc_live_user_agent()) {
  if (isTRUE(mock)) {
    return(.cc_mock_attention(dictionary, "gdelt", "news_volume", from, to, "day", seed = 505))
  }
  dictionary <- .cc_filter_dictionary_for_platform(dictionary, "gdelt", query_types, max_queries_per_concept, max_ambiguity)
  if (nrow(dictionary) == 0) stop("No GDELT queries remained after filtering.", call. = FALSE)
  if (.cc_as_date(from) < Sys.Date() - 90) {
    warning("GDELT DOC API timelines cover roughly the last three months. Dates before ", Sys.Date() - 90,
            " are likely to return no articles.", call. = FALSE)
  }
  metric <- if (tolower(mode) == "timelinevolraw") "news_article_count" else "news_volume_share"
  all_dates <- seq(.cc_as_date(from), .cc_as_date(to), by = "day")
  rows <- list()
  status <- vector("list", nrow(dictionary))
  for (i in seq_len(nrow(dictionary))) {
    if (i > 1 && sleep > 0) Sys.sleep(sleep)
    url <- .cc_gdelt_url(dictionary$query[i], from, to, mode, timelinesmooth)
    obj <- try(.cc_fetch_json(url, user_agent = user_agent, retries = retries), silent = TRUE)
    if (!inherits(obj, "try-error") && length(obj) == 0 && sleep > 0) {
      ## GDELT answers both a query without matches and some throttled
      ## requests with HTTP 200 and an empty object, so ask once more.
      .cc_sleep(2 * sleep)
      obj <- try(.cc_fetch_json(url, user_agent = user_agent, retries = retries), silent = TRUE)
    }
    if (inherits(obj, "try-error")) {
      msg <- conditionMessage(attr(obj, "condition"))
      status[[i]] <- data.frame(concept_id = dictionary$concept_id[i], query = dictionary$query[i], status = "failed", message = substr(msg, 1, 300), stringsAsFactors = FALSE)
      warning("GDELT request failed for '", dictionary$query[i], "' and was skipped: ", msg, call. = FALSE)
      next
    }
    res <- .cc_gdelt_rows(obj, dictionary[i, , drop = FALSE], all_dates, metric, fill_missing_zero,
                          source = paste0("gdelt_doc_api:", mode))
    status[[i]] <- res$status
    if (!is.null(res$rows)) rows[[length(rows) + 1L]] <- res$rows
  }
  status <- .cc_bind_rows_fill(status)
  if (nrow(status) > 1 && all(status$status == "no_articles")) {
    warning("Every GDELT query returned an empty response. This usually indicates throttling rather than ",
            "true zero coverage; check gdelt_status before analysing these rows.", call. = FALSE)
  }
  if (length(rows) == 0) stop("No GDELT rows were returned. Use a date range within the last three months and check query syntax.", call. = FALSE)
  out <- as_culturomic_tbl(.cc_bind_rows_fill(rows), dictionary = dictionary)
  attr(out, "gdelt_status") <- status
  out
}

## Builds the GDELT DOC 2.0 request URL for one query.
.cc_gdelt_url <- function(query, from, to, mode = "timelinevolraw", timelinesmooth = NULL) {
  url <- paste0(
    "https://api.gdeltproject.org/api/v2/doc/doc?query=",
    utils::URLencode(query, reserved = TRUE),
    "&mode=", mode,
    "&format=json&startdatetime=", format(.cc_as_date(from), "%Y%m%d000000"),
    "&enddatetime=", format(.cc_as_date(to), "%Y%m%d235959")
  )
  if (!is.null(timelinesmooth)) url <- paste0(url, "&timelinesmooth=", timelinesmooth)
  url
}

## Turns one parsed GDELT response into trace rows and a status row. Shared
## by live collection and by import of responses saved from a browser.
.cc_gdelt_rows <- function(obj, dict_row, all_dates, metric, fill_missing_zero = TRUE, source = "gdelt_doc_api") {
  dat <- .cc_gdelt_timeline(obj)
  if (is.null(dat)) {
    ## A valid response without a timeline means no matching articles.
    st <- "no_articles"
    dat <- data.frame(date = all_dates[0], value = numeric(0))
  } else {
    st <- "ok"
  }
  if (isTRUE(fill_missing_zero)) {
    filled <- data.frame(date = all_dates, value = 0)
    hit <- match(dat$date, all_dates)
    filled$value[hit[!is.na(hit)]] <- dat$value[!is.na(hit)]
    dat <- filled
  }
  status <- data.frame(concept_id = dict_row$concept_id, query = dict_row$query, status = st,
                       message = if (st == "ok") paste0(sum(dat$value > 0, na.rm = TRUE), " days with articles") else "valid response without timeline (no matches, or throttling)",
                       stringsAsFactors = FALSE)
  rows <- NULL
  if (nrow(dat) > 0) {
    rows <- data.frame(
      date = dat$date,
      country = .cc_normalise_country(dict_row$country),
      language = dict_row$language,
      platform = "gdelt",
      concept_id = dict_row$concept_id,
      query = dict_row$query,
      metric = metric,
      raw_value = dat$value,
      value = dat$value,
      source = paste0(source, if (st == "no_articles") ":no_articles" else ""),
      collection_time = as.character(Sys.time()),
      stringsAsFactors = FALSE
    )
  }
  list(rows = rows, status = status)
}

## Extracts the article timeline from a GDELT DOC response. The API nests it
## as timeline[[series]]$data with date/value columns; NULL means no timeline.
.cc_gdelt_timeline <- function(obj) {
  tl <- obj[["timeline"]]
  if (is.null(tl) || length(tl) == 0) return(NULL)
  dat <- NULL
  if (is.data.frame(tl) && "data" %in% names(tl)) {
    dat <- tl$data[[1]]
  } else if (is.data.frame(tl) && "date" %in% names(tl)) {
    dat <- tl
  } else if (is.list(tl) && !is.null(tl[[1]][["data"]])) {
    dat <- tl[[1]][["data"]]
  }
  if (is.null(dat)) return(NULL)
  dat <- as.data.frame(dat, stringsAsFactors = FALSE)
  if (nrow(dat) == 0 || !all(c("date", "value") %in% names(dat))) return(NULL)
  data.frame(date = .cc_parse_gdelt_date(dat$date), value = suppressWarnings(as.numeric(dat$value)))
}

#' Alias for social attention collection
#'
#' @param dictionary Concept dictionary.
#' @param from Start date.
#' @param to End date.
#' @param platform Social platform to collect.
#' @param ... Passed to the platform collector.
#' @return culturomic_tbl.
#' @export
get_social_attention <- function(dictionary, from, to, platform = c("reddit", "youtube"), ...) {
  platform <- match.arg(platform)
  switch(platform,
    reddit = get_reddit_attention(dictionary, from, to, ...),
    youtube = get_youtube_attention(dictionary, from, to, ...)
  )
}

#' Collect attention from multiple platforms
#' @param dictionary Concept dictionary.
#' @param platforms Platforms to collect.
#' @param from Start date.
#' @param to End date.
#' @param mock Return synthetic data.
#' @param platform_args Optional named list of platform-specific arguments.
#' @param continue_on_error Continue when one platform fails.
#' @param trim_dates Drop rows outside from-to. Google Trends preset windows
#'   (e.g. "today 3-m") can return dates beyond the requested range.
#' @param ... Passed to all platform functions.
#' @return culturomic_tbl.
#' @examples
#' ex <- con_culturomics_example_data()
#' dict <- score_ambiguity(ex$dictionary)
#' tr <- collect_attention(dict, from = "2024-01-01", to = "2024-03-31", mock = TRUE)
#' table(tr$platform)
#' \dontrun{
#' # live collection; Google Trends values share one scale via anchor_keyword
#' live <- collect_attention(dict, from = "2024-01-01", to = "2024-03-31",
#'   platform_args = list(google_trends = list(batch = TRUE, anchor_keyword = "wildlife")))
#' }
#' @export
collect_attention <- function(dictionary,
                              platforms = c("wikipedia", "gdelt", "google"),
                              from,
                              to,
                              mock = FALSE,
                              platform_args = list(),
                              continue_on_error = TRUE,
                              trim_dates = TRUE,
                              ...) {
  rows <- list()
  diagnostics <- list()
  dots <- list(...)
  for (platform in platforms) {
    p <- .cc_platform_alias(platform)
    fun <- switch(p,
      google_trends = get_google_attention,
      wikipedia = get_wiki_attention,
      reddit = get_reddit_attention,
      youtube = get_youtube_attention,
      gdelt = get_gdelt_attention,
      stop("Unknown platform: ", platform, call. = FALSE)
    )
    base_args <- list(dictionary = dictionary, from = from, to = to, mock = mock)
    alias_args <- if (!is.null(platform_args[[p]])) platform_args[[p]] else list()
    original_args <- if (!identical(platform, p) && !is.null(platform_args[[platform]])) platform_args[[platform]] else list()
    pargs <- .cc_merge_args(base_args, dots, alias_args, original_args)
    ans <- try(do.call(fun, pargs), silent = TRUE)
    if (inherits(ans, "try-error")) {
      msg <- paste0("Platform collection failed for ", platform, ": ", conditionMessage(attr(ans, "condition")))
      if (isTRUE(continue_on_error)) {
        warning(msg, call. = FALSE)
        next
      } else {
        stop(msg, call. = FALSE)
      }
    }
    dat <- try(as.data.frame(ans, stringsAsFactors = FALSE), silent = TRUE)
    if (inherits(dat, "try-error") || is.null(dat) || nrow(dat) == 0) {
      msg <- paste0("Platform collection returned no rows for ", platform, ".")
      if (isTRUE(continue_on_error)) {
        warning(msg, call. = FALSE)
        next
      } else {
        stop(msg, call. = FALSE)
      }
    }
    dat$.cc_source_platform <- p
    for (a in c("gdelt_status")) if (!is.null(attr(ans, a))) diagnostics[[p]] <- attr(ans, a)
    if (isTRUE(trim_dates) && "date" %in% names(dat)) {
      d <- .cc_as_date(dat$date)
      dat <- dat[!is.na(d) & d >= .cc_as_date(from) & d <= .cc_as_date(to), , drop = FALSE]
      if (nrow(dat) == 0) {
        msg <- paste0("Platform collection returned no rows within from-to for ", platform, ".")
        if (isTRUE(continue_on_error)) {
          warning(msg, call. = FALSE)
          next
        } else {
          stop(msg, call. = FALSE)
        }
      }
    }
    rows[[platform]] <- dat
  }
  if (length(rows) == 0) stop("No platform returned data.", call. = FALSE)
  bound <- .cc_bind_rows_fill(rows)
  out <- as_culturomic_tbl(bound, dictionary = dictionary)
  attr(out, "platform_diagnostics") <- diagnostics
  attr(out, "platform_collection_status") <- data.frame(
    platform = names(rows),
    rows = vapply(rows, nrow, integer(1)),
    stringsAsFactors = FALSE
  )
  out
}

#' Install Google Trends dependency
#'
#' Convenience wrapper around install_live_dependencies("google").
#'
#' @param repos CRAN repository.
#' @return Invisibly returns dependency status after installation attempts.
#' @export
install_google_trends_dependency <- function(repos = "https://cloud.r-project.org") {
  install_live_dependencies("google", repos = repos)
}

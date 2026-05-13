.cc_required_cols <- function(x, cols, object = deparse(substitute(x))) {
  missing <- setdiff(cols, names(x))
  if (length(missing) > 0) {
    stop(object, " is missing required columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

.cc_as_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  as.Date(x)
}

.cc_date_sequence <- function(from, to, granularity = c("day", "week", "month")) {
  granularity <- match.arg(granularity)
  from <- .cc_as_date(from)
  to <- .cc_as_date(to)
  by <- switch(granularity, day = "day", week = "week", month = "month")
  seq(from, to, by = by)
}

.cc_rescale01 <- function(x) {
  if (all(is.na(x))) return(rep(NA_real_, length(x)))
  rng <- range(x, na.rm = TRUE)
  if (!is.finite(rng[1]) || !is.finite(rng[2]) || diff(rng) == 0) {
    return(rep(0.5, length(x)))
  }
  (x - rng[1]) / diff(rng)
}

.cc_zscore <- function(x) {
  if (all(is.na(x))) return(rep(NA_real_, length(x)))
  s <- stats::sd(x, na.rm = TRUE)
  m <- mean(x, na.rm = TRUE)
  if (!is.finite(s) || s == 0) return(rep(0, length(x)))
  (x - m) / s
}


.cc_make_key <- function(df, cols) {
  .cc_required_cols(df, cols, "df")
  parts <- lapply(df[cols], function(z) {
    z <- as.character(z)
    z[is.na(z) | !nzchar(z)] <- "<NA>"
    z
  })
  do.call(paste, c(parts, sep = "\r"))
}

.cc_group_apply <- function(x, group, fun) {
  split_x <- split(seq_along(x), group, drop = TRUE)
  out <- rep(NA_real_, length(x))
  for (idx in split_x) out[idx] <- fun(x[idx])
  out
}

.cc_safe_mean <- function(x) {
  if (length(x) == 0 || all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

.cc_safe_sd <- function(x) {
  if (length(x) <= 1 || all(is.na(x))) return(NA_real_)
  stats::sd(x, na.rm = TRUE)
}

.cc_mock_attention <- function(dictionary, platform, metric, from, to, granularity = "day", seed = 1) {
  .cc_required_cols(dictionary, c("concept_id", "query", "language", "country"), "dictionary")
  set.seed(seed)
  dates <- .cc_date_sequence(from, to, granularity)
  rows <- vector("list", nrow(dictionary))
  for (i in seq_len(nrow(dictionary))) {
    n <- length(dates)
    baseline <- stats::runif(1, 5, 80)
    trend <- seq(0, stats::runif(1, -10, 10), length.out = n)
    season <- sin(seq(0, 2 * pi, length.out = n)) * stats::runif(1, 0, 15)
    noise <- stats::rnorm(n, 0, 5)
    value <- pmax(0, baseline + trend + season + noise)
    rows[[i]] <- data.frame(
      date = dates,
      country = dictionary$country[i],
      language = dictionary$language[i],
      platform = platform,
      concept_id = dictionary$concept_id[i],
      query = dictionary$query[i],
      metric = metric,
      raw_value = value,
      value = value,
      source = "mock",
      collection_time = as.character(Sys.time()),
      stringsAsFactors = FALSE
    )
  }
  as_culturomic_tbl(.cc_bind_rows_fill(rows), dictionary = dictionary)
}

.cc_stop_missing_pkg <- function(pkg) {
  stop("Package '", pkg, "' is required for this live collection function. Install it or use mock = TRUE for examples.", call. = FALSE)
}

.cc_compact <- function(x) x[!vapply(x, is.null, logical(1))]

.cc_robust_zscore <- function(x) {
  if (all(is.na(x))) return(rep(NA_real_, length(x)))
  med <- stats::median(x, na.rm = TRUE)
  madv <- stats::mad(x, constant = 1.4826, na.rm = TRUE)
  if (!is.finite(madv) || madv == 0) return(rep(0, length(x)))
  (x - med) / madv
}

.cc_transform_values <- function(x, transform = c("none", "log1p")) {
  transform <- match.arg(transform)
  x <- as.numeric(x)
  if (transform == "log1p") {
    min_x <- suppressWarnings(min(x, na.rm = TRUE))
    if (is.finite(min_x) && min_x < 0) {
      x <- x - min_x
    }
    return(log1p(x))
  }
  x
}

.cc_weighted_mean <- function(x, w = NULL) {
  x <- as.numeric(x)
  if (is.null(w)) w <- rep(1, length(x))
  w <- as.numeric(w)
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

.cc_rank_percentile <- function(x, method = c("midrank", "rank_n")) {
  method <- match.arg(method)
  out <- rep(NA_real_, length(x))
  ok <- !is.na(x)
  n <- sum(ok)
  if (n == 0) return(out)
  r <- rank(x[ok], ties.method = "average")
  out[ok] <- if (method == "midrank") (r - 0.5) / n else r / n
  out
}

.cc_bind_rows_fill <- function(x) {
  x <- x[!vapply(x, is.null, logical(1))]
  if (length(x) == 0) return(data.frame())
  x <- lapply(x, function(z) {
    z <- as.data.frame(z, stringsAsFactors = FALSE)
    if (nrow(z) == 0 && ncol(z) == 0) return(z)
    z
  })
  cols <- unique(unlist(lapply(x, names), use.names = FALSE))
  if (length(cols) == 0) return(data.frame())
  x <- lapply(x, function(z) {
    missing <- setdiff(cols, names(z))
    for (m in missing) z[[m]] <- NA
    z <- z[cols]
    ## Live platform APIs can return slightly different column types.
    ## Normalise unsupported list columns before row-binding so a single
    ## platform cannot break the whole collection step.
    for (nm in names(z)) {
      if (is.list(z[[nm]]) && !inherits(z[[nm]], "data.frame")) {
        z[[nm]] <- vapply(z[[nm]], function(v) paste(as.character(v), collapse = ";"), character(1))
      }
    }
    z
  })
  out <- tryCatch(
    do.call(rbind.data.frame, c(x, stringsAsFactors = FALSE, make.row.names = FALSE)),
    error = function(e) {
      ## Last-resort coercion for heterogeneous API rows.
      x2 <- lapply(x, function(z) {
        for (nm in names(z)) {
          if (inherits(z[[nm]], "Date")) next
          z[[nm]] <- as.character(z[[nm]])
        }
        z
      })
      do.call(rbind.data.frame, c(x2, stringsAsFactors = FALSE, make.row.names = FALSE))
    }
  )
  rownames(out) <- NULL
  out
}

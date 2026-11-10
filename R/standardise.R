#' Standardise platform-specific attention values
#'
#' @param x culturomic_tbl.
#' @param method zscore, robust_zscore, minmax, share, or baseline.
#' @param group_cols Columns defining standardisation groups.
#' @param baseline_period Optional two-date vector for baseline method.
#' @param transform Transformation applied before scaling. Use log1p for count-like traces.
#' @return culturomic_tbl with scaled_value.
#' @examples
#' ex <- con_culturomics_example_data()
#' dict <- score_ambiguity(ex$dictionary)
#' tr <- collect_attention(dict, from = "2024-01-01", to = "2024-03-31", mock = TRUE)
#' std <- standardise_attention(tr, method = "robust_zscore", transform = "log1p")
#' tapply(std$scaled_value, std$platform, median)
#' @export
standardise_attention <- function(x,
                                  method = c("robust_zscore", "zscore", "minmax", "share", "baseline"),
                                  group_cols = c("platform", "metric"),
                                  baseline_period = NULL,
                                  transform = c("log1p", "none")) {
  method <- match.arg(method)
  transform <- match.arg(transform)
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  .cc_required_cols(x, c("raw_value", group_cols), "x")
  x$.cc_value_for_scaling <- .cc_transform_values(x$raw_value, transform = transform)
  group <- .cc_make_key(x, group_cols)
  ## A group without spread (e.g. a platform whose queries all failed and were
  ## zero-filled) standardises to a constant and carries no information.
  if (method %in% c("zscore", "robust_zscore", "minmax")) {
    spread <- vapply(split(x$.cc_value_for_scaling, group), function(v) {
      v <- v[is.finite(v)]
      if (length(v) < 2) return(NA_real_)
      ## Only truly constant groups lack spread; robust_zscore falls back to
      ## the mean absolute deviation when MAD alone is 0.
      diff(range(v))
    }, numeric(1))
    flat <- names(spread)[!is.na(spread) & spread == 0]
    if (length(flat) > 0) {
      warning("Standardisation group(s) without spread: ", paste(gsub("\r", " / ", flat), collapse = ", "),
              ". All values in these groups are set to 0 and add no information to the attention index; ",
              "check for failed or zero-filled collection.", call. = FALSE)
    }
  }
  if (method == "zscore") {
    x$scaled_value <- .cc_group_apply(x$.cc_value_for_scaling, group, .cc_zscore)
  } else if (method == "robust_zscore") {
    x$scaled_value <- .cc_group_apply(x$.cc_value_for_scaling, group, .cc_robust_zscore)
  } else if (method == "minmax") {
    x$scaled_value <- .cc_group_apply(x$.cc_value_for_scaling, group, .cc_rescale01)
  } else if (method == "share") {
    x$scaled_value <- x$.cc_value_for_scaling
    for (g in unique(group)) {
      idx <- which(group == g)
      denom <- sum(x$scaled_value[idx], na.rm = TRUE)
      x$scaled_value[idx] <- if (denom == 0) NA_real_ else x$scaled_value[idx] / denom
    }
  } else if (method == "baseline") {
    if (is.null(baseline_period) || length(baseline_period) != 2) {
      stop("baseline_period must contain start and end dates when method = 'baseline'.", call. = FALSE)
    }
    x$date <- .cc_as_date(x$date)
    start <- .cc_as_date(baseline_period[1])
    end <- .cc_as_date(baseline_period[2])
    x$scaled_value <- NA_real_
    for (g in unique(group)) {
      idx <- which(group == g)
      base_idx <- idx[x$date[idx] >= start & x$date[idx] <= end]
      base <- mean(x$.cc_value_for_scaling[base_idx], na.rm = TRUE)
      x$scaled_value[idx] <- if (!is.finite(base) || base == 0) NA_real_ else x$.cc_value_for_scaling[idx] / base
    }
  }
  x$standardisation_method <- method
  x$standardisation_transform <- transform
  x$.cc_value_for_scaling <- NULL
  as_culturomic_tbl(x, dictionary = attr(x, "dictionary"))
}

#' American spelling alias for standardise_attention
#'
#' @param x culturomic_tbl.
#' @param method zscore, robust_zscore, minmax, share, or baseline.
#' @param group_cols Columns defining standardisation groups.
#' @param baseline_period Optional two-date vector for baseline method.
#' @param transform Transformation applied before scaling.
#' @return culturomic_tbl with scaled_value.
#' @export
standardize_attention <- function(x, method = c("robust_zscore", "zscore", "minmax", "share", "baseline"), group_cols = c("platform", "metric"), baseline_period = NULL, transform = c("log1p", "none")) {
  standardise_attention(x = x, method = method, group_cols = group_cols, baseline_period = baseline_period, transform = transform)
}

.cc_query_type_weight <- function(query_type) {
  q <- tolower(as.character(query_type))
  out <- rep(0.7, length(q))
  out[q == "scientific"] <- 1.0
  out[q == "common"] <- 0.8
  out[q == "translated_common"] <- 0.7
  out[q == "synonym"] <- 0.7
  out[q == "hashtag"] <- 0.5
  out[is.na(q) | !nzchar(q)] <- 0.7
  out
}

#' Estimate cross-platform attention index
#'
#' @param x culturomic_tbl.
#' @param weights Optional platform weights named by platform.
#' @param min_platforms Minimum platforms required per date and concept.
#' @param query_weight_col Optional column containing query-level reliability weights.
#' @param use_ambiguity Downweight ambiguous queries using 1 - ambiguity_score.
#' @param query_type_weights Optional named vector overriding default query type weights.
#' @param aggregate Two-stage aggregation is the recommended default.
#' @return Data frame with attention_index and platform_disagreement.
#' @examples
#' ex <- con_culturomics_example_data()
#' dict <- score_ambiguity(ex$dictionary)
#' tr <- collect_attention(dict, from = "2024-01-01", to = "2024-03-31", mock = TRUE)
#' std <- standardise_attention(tr)
#' idx <- attention_index(std, min_platforms = 2)
#' head(idx[, c("concept_id", "date", "attention_index", "platform_disagreement")])
#' # give news twice the weight of other platforms
#' attention_index(std, weights = c(gdelt = 2))
#' @export
attention_index <- function(x,
                            weights = NULL,
                            min_platforms = 1,
                            query_weight_col = NULL,
                            use_ambiguity = TRUE,
                            query_type_weights = NULL,
                            aggregate = c("two_stage", "flat")) {
  aggregate <- match.arg(aggregate)
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  .cc_required_cols(x, c("date", "country", "language", "platform", "concept_id", "scaled_value"), "x")
  if (all(is.na(x$scaled_value))) {
    x <- as.data.frame(standardise_attention(x), stringsAsFactors = FALSE)
  }
  if (!"query" %in% names(x)) x$query <- "<query>"
  if (!"query_type" %in% names(x)) x$query_type <- NA_character_
  if (!"ambiguity_score" %in% names(x)) x$ambiguity_score <- 0
  x$scaled_value <- as.numeric(x$scaled_value)

  qtw <- .cc_query_type_weight(x$query_type)
  if (!is.null(query_type_weights)) {
    hit <- match(tolower(as.character(x$query_type)), tolower(names(query_type_weights)))
    qtw[!is.na(hit)] <- as.numeric(query_type_weights[hit[!is.na(hit)]])
  }
  aw <- if (isTRUE(use_ambiguity)) pmax(0.05, 1 - pmin(1, pmax(0, as.numeric(x$ambiguity_score)))) else rep(1, nrow(x))
  cw <- if (!is.null(query_weight_col) && query_weight_col %in% names(x)) as.numeric(x[[query_weight_col]]) else rep(1, nrow(x))
  x$.query_weight <- qtw * aw * cw

  key_cols <- c("date", "country", "language", "concept_id")
  key <- .cc_make_key(x, key_cols)
  plat <- as.character(x$platform)
  pw <- if (is.null(weights)) rep(1, nrow(x)) else unname(weights[plat])
  pw[is.na(pw)] <- 1

  ## Platform-level values: one row per key x platform.
  kp <- paste(key, plat, sep = "\r")
  if (aggregate == "flat") {
    ai_by_key <- .cc_group_wmean(x$scaled_value, x$.query_weight * pw, key)
    sv <- x$scaled_value
    ok <- !is.na(sv)
    ## Unweighted platform means (NaN when all values are missing, as mean(na.rm = TRUE)).
    p_val <- rowsum(ifelse(ok, sv, 0), kp, reorder = FALSE)[, 1] / rowsum(as.numeric(ok), kp, reorder = FALSE)[, 1]
  } else {
    ## Stage 1: query values within platform; query weight = mean row weight.
    qk <- paste(key, .cc_make_key(x, c("platform", "query")), sep = "\r")
    q_first <- !duplicated(qk)
    q_val <- .cc_group_wmean(x$scaled_value, x$.query_weight, qk)
    qw_ok <- !is.na(x$.query_weight)
    q_w <- rowsum(ifelse(qw_ok, x$.query_weight, 0), qk, reorder = FALSE)[, 1] /
      rowsum(as.numeric(qw_ok), qk, reorder = FALSE)[, 1]
    q_kp <- kp[q_first]
    ## Stage 2: platform values from query values.
    p_val <- .cc_group_wmean(q_val[qk[q_first]], q_w[qk[q_first]], q_kp)
    p_first_rows <- !duplicated(kp)
    ai_by_key <- .cc_group_wmean(p_val[kp[p_first_rows]], pw[p_first_rows], key[p_first_rows])
  }
  p_first <- !duplicated(kp)
  p_key <- key[p_first]
  p_values <- unname(p_val[kp[p_first]])
  disagreement <- vapply(split(p_values, p_key), .cc_safe_sd, numeric(1))

  ## Same key order as split(): sorted factor levels.
  levs <- levels(factor(key))
  first_row <- match(levs, key)
  n_platforms <- vapply(split(plat, key), function(z) length(unique(z)), integer(1))[levs]
  n_queries <- vapply(split(as.character(x$query), key), function(z) length(unique(z)), integer(1))[levs]
  out <- data.frame(
    date = x$date[first_row],
    country = x$country[first_row],
    language = x$language[first_row],
    concept_id = x$concept_id[first_row],
    attention_index = unname(ai_by_key[levs]),
    platform_disagreement = unname(disagreement[levs]),
    n_platforms = unname(n_platforms),
    n_queries = unname(n_queries),
    aggregation_method = rep(aggregate, length(levs)),
    stringsAsFactors = FALSE
  )
  out <- out[out$n_platforms >= min_platforms, , drop = FALSE]
  rownames(out) <- NULL
  out
}

## Grouped weighted mean with .cc_weighted_mean() rules: rows with non-finite
## values or weights, or weights <= 0, are dropped; empty groups give NA.
.cc_group_wmean <- function(x, w, g) {
  x <- as.numeric(x)
  w <- as.numeric(w)
  ok <- is.finite(x) & is.finite(w) & w > 0
  num <- rowsum(ifelse(ok, x * w, 0), g, reorder = FALSE)[, 1]
  den <- rowsum(ifelse(ok, w, 0), g, reorder = FALSE)[, 1]
  out <- num / den
  out[den == 0] <- NA_real_
  out
}

#' Detect attention bursts
#'
#' @param x Attention index table or culturomic_tbl.
#' @param value_col Value column.
#' @param group_cols Group columns.
#' @param window Previous observations used as local baseline.
#' @param z_threshold Burst threshold.
#' @return Data frame with burst diagnostics.
#' @export
detect_bursts <- function(x,
                          value_col = "attention_index",
                          group_cols = c("concept_id", "country", "language"),
                          window = 30,
                          z_threshold = 2.5) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  if (!value_col %in% names(x) && "scaled_value" %in% names(x)) {
    idx <- attention_index(x)
    return(detect_bursts(idx, value_col = "attention_index", group_cols = group_cols, window = window, z_threshold = z_threshold))
  }
  .cc_required_cols(x, c("date", value_col, group_cols), "x")
  x$date <- .cc_as_date(x$date)
  x <- x[order(interaction(x[group_cols], drop = TRUE), x$date), , drop = FALSE]
  x$burst_z <- NA_real_
  x$is_burst <- FALSE
  grp <- .cc_make_key(x, group_cols)
  for (g in unique(grp)) {
    idx <- which(grp == g)
    vals <- as.numeric(x[[value_col]][idx])
    for (j in seq_along(idx)) {
      if (j == 1) next
      prev <- vals[max(1, j - window):(j - 1)]
      if (length(prev) < 3 || stats::sd(prev, na.rm = TRUE) == 0) next
      z <- (vals[j] - mean(prev, na.rm = TRUE)) / stats::sd(prev, na.rm = TRUE)
      x$burst_z[idx[j]] <- z
      x$is_burst[idx[j]] <- is.finite(z) && z >= z_threshold
    }
  }
  x
}

#' Decompose seasonal attention patterns
#'
#' @param x Attention index table.
#' @param value_col Value column.
#' @return Data frame with monthly baseline and anomaly.
#' @export
decompose_seasonality <- function(x, value_col = "attention_index") {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  .cc_required_cols(x, c("date", "concept_id", value_col), "x")
  x$date <- .cc_as_date(x$date)
  x$month <- as.integer(format(x$date, "%m"))
  key_cols <- intersect(c("concept_id", "country", "language", "month"), names(x))
  key <- .cc_make_key(x, key_cols)
  baseline <- tapply(as.numeric(x[[value_col]]), key, mean, na.rm = TRUE)
  x$seasonal_baseline <- as.numeric(baseline[as.character(key)])
  x$seasonal_anomaly <- as.numeric(x[[value_col]]) - x$seasonal_baseline
  x
}

#' Seasonal attention alias
#'
#' @param x Attention index table.
#' @param value_col Value column.
#' @return Data frame with monthly baseline and anomaly.
#' @export
seasonal_attention <- function(x, value_col = "attention_index") {
  decompose_seasonality(x = x, value_col = value_col)
}

#' Query sensitivity analysis
#'
#' @param x culturomic_tbl or standardised trace table.
#' @param value_col Value column.
#' @return Data frame summarising query-level instability.
#' @examples
#' ex <- con_culturomics_example_data()
#' dict <- score_ambiguity(ex$dictionary)
#' tr <- collect_attention(dict, from = "2024-01-01", to = "2024-03-31", mock = TRUE)
#' query_sensitivity(standardise_attention(tr))
#' @export
query_sensitivity <- function(x, value_col = "scaled_value") {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  .cc_required_cols(x, c("concept_id", "query", value_col), "x")
  if (all(is.na(x[[value_col]])) && "raw_value" %in% names(x)) {
    x <- standardise_attention(x)
  }
  query_means <- stats::aggregate(as.numeric(x[[value_col]]), x[c("concept_id", "query")], mean, na.rm = TRUE)
  names(query_means)[names(query_means) == "x"] <- "query_mean"
  res <- lapply(split(query_means, query_means$concept_id), function(dat) {
    n_q <- nrow(dat)
    full_mean <- mean(dat$query_mean, na.rm = TRUE)
    loo <- rep(0, n_q)
    if (n_q > 1) {
      for (i in seq_len(n_q)) {
        loo[i] <- abs(full_mean - mean(dat$query_mean[-i], na.rm = TRUE))
      }
    }
    q1 <- suppressWarnings(stats::quantile(dat$query_mean, probs = 0.25, na.rm = TRUE, names = FALSE, type = 7))
    q3 <- suppressWarnings(stats::quantile(dat$query_mean, probs = 0.75, na.rm = TRUE, names = FALSE, type = 7))
    data.frame(
      concept_id = dat$concept_id[1],
      n_queries = n_q,
      query_mean_min = min(dat$query_mean, na.rm = TRUE),
      query_mean_max = max(dat$query_mean, na.rm = TRUE),
      query_sensitivity_range = if (n_q <= 1) 0 else diff(range(dat$query_mean, na.rm = TRUE)),
      query_sensitivity_sd = .cc_safe_sd(dat$query_mean),
      query_sensitivity_iqr = if (n_q <= 1) 0 else as.numeric(q3 - q1),
      max_leave_one_query_influence = if (n_q <= 1) 0 else max(loo, na.rm = TRUE),
      mean_leave_one_query_influence = if (n_q <= 1) 0 else mean(loo, na.rm = TRUE),
      query_sensitivity_index = if (n_q <= 1) 0 else diff(range(dat$query_mean, na.rm = TRUE)),
      most_influential_query = dat$query[which.max(loo)],
      most_extreme_query = dat$query[which.max(abs(dat$query_mean - full_mean))],
      stringsAsFactors = FALSE
    )
  })
  .cc_bind_rows_fill(res)
}

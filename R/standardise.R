#' Standardise platform-specific attention values
#'
#' @param x culturomic_tbl.
#' @param method zscore, robust_zscore, minmax, share, or baseline.
#' @param group_cols Columns defining standardisation groups.
#' @param baseline_period Optional two-date vector for baseline method.
#' @param transform Transformation applied before scaling. Use log1p for count-like traces.
#' @return culturomic_tbl with scaled_value.
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
  res <- lapply(split(seq_len(nrow(x)), key), function(idx) {
    dat <- x[idx, , drop = FALSE]
    platforms <- unique(dat$platform)
    if (length(platforms) < min_platforms) return(NULL)

    if (aggregate == "flat") {
      if (is.null(weights)) {
        pw_obs <- rep(1, nrow(dat))
      } else {
        pw_obs <- weights[dat$platform]
        pw_obs[is.na(pw_obs)] <- 1
      }
      ai <- .cc_weighted_mean(dat$scaled_value, dat$.query_weight * pw_obs)
      platform_means <- tapply(dat$scaled_value, dat$platform, mean, na.rm = TRUE)
    } else {
      qp_key <- .cc_make_key(dat, c("platform", "query"))
      query_rows <- lapply(split(seq_len(nrow(dat)), qp_key), function(j) {
        sub <- dat[j, , drop = FALSE]
        data.frame(
          platform = sub$platform[1],
          query = sub$query[1],
          query_value = .cc_weighted_mean(sub$scaled_value, sub$.query_weight),
          query_weight = mean(sub$.query_weight, na.rm = TRUE),
          stringsAsFactors = FALSE
        )
      })
      qdat <- .cc_bind_rows_fill(query_rows)
      p_rows <- lapply(split(seq_len(nrow(qdat)), qdat$platform), function(j) {
        sub <- qdat[j, , drop = FALSE]
        data.frame(
          platform = sub$platform[1],
          platform_value = .cc_weighted_mean(sub$query_value, sub$query_weight),
          n_queries_platform = length(unique(sub$query)),
          stringsAsFactors = FALSE
        )
      })
      pdat <- .cc_bind_rows_fill(p_rows)
      if (is.null(weights)) {
        pdat$platform_weight <- 1
      } else {
        pdat$platform_weight <- weights[pdat$platform]
        pdat$platform_weight[is.na(pdat$platform_weight)] <- 1
      }
      ai <- .cc_weighted_mean(pdat$platform_value, pdat$platform_weight)
      platform_means <- stats::setNames(pdat$platform_value, pdat$platform)
    }

    data.frame(
      date = dat$date[1],
      country = dat$country[1],
      language = dat$language[1],
      concept_id = dat$concept_id[1],
      attention_index = ai,
      platform_disagreement = .cc_safe_sd(platform_means),
      n_platforms = length(platforms),
      n_queries = length(unique(dat$query)),
      aggregation_method = aggregate,
      stringsAsFactors = FALSE
    )
  })
  out <- .cc_bind_rows_fill(res[!vapply(res, is.null, logical(1))])
  if (is.null(out)) out <- data.frame()
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

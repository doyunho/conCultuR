#' Estimate conservation attention gaps
#'
#' @param attention Attention index table from attention_index.
#' @param conservation_status Data frame with concept_id and status or threat score.
#' @param status_col Column containing IUCN-like status.
#' @param threat_score_col Optional numeric threat score column.
#' @param attention_col Attention column.
#' @param high_threat Percentile threshold for high threat.
#' @param low_attention Percentile threshold for low attention.
#' @param high_attention Percentile threshold for high attention.
#' @param gap_threshold Minimum threat minus attention percentile gap for under-attended concepts.
#' @param near_margin Margin used to flag near-threshold candidates.
#' @param tolerance Numeric tolerance used when comparing percentiles to thresholds.
#' @param percentile_method midrank or rank_n.
#' @return culturomic_gap object.
#' @export
attention_gap <- function(attention,
                          conservation_status,
                          status_col = "status",
                          threat_score_col = NULL,
                          attention_col = "attention_index",
                          high_threat = 2/3,
                          low_attention = 0.50,
                          high_attention = 2/3,
                          gap_threshold = 0.25,
                          near_margin = 0.05,
                          tolerance = sqrt(.Machine$double.eps),
                          percentile_method = c("midrank", "rank_n")) {
  percentile_method <- match.arg(percentile_method)
  attention <- as.data.frame(attention, stringsAsFactors = FALSE)
  conservation_status <- as.data.frame(conservation_status, stringsAsFactors = FALSE)
  .cc_required_cols(attention, c("concept_id", attention_col), "attention")
  .cc_required_cols(conservation_status, "concept_id", "conservation_status")

  att <- stats::aggregate(as.numeric(attention[[attention_col]]), attention["concept_id"], mean, na.rm = TRUE)
  names(att)[2] <- "mean_attention"

  cs <- .cc_collapse_conservation_status(
    conservation_status,
    status_col = status_col,
    threat_score_col = threat_score_col
  )
  dat <- merge(att, cs, by = "concept_id", all.x = TRUE)

  if (!is.null(threat_score_col) && threat_score_col %in% names(dat)) {
    dat$threat_score <- suppressWarnings(as.numeric(dat[[threat_score_col]]))
  } else if (status_col %in% names(dat)) {
    ranks <- c(LC = 1, NT = 2, VU = 3, EN = 4, CR = 5, EW = 6, EX = 7, DD = NA_real_, NE = NA_real_)
    dat$status_normalised <- toupper(trimws(as.character(dat[[status_col]])))
    dat$status_normalised[dat$status_normalised %in% c("", "NA", "N/A", "NULL", "UNKNOWN")] <- NA_character_
    dat$threat_score <- unname(ranks[dat$status_normalised])
  } else {
    stop("conservation_status must contain status_col or threat_score_col.", call. = FALSE)
  }

  dat$attention_percentile <- .cc_rank_percentile(dat$mean_attention, method = percentile_method)
  dat$threat_percentile <- .cc_rank_percentile(dat$threat_score, method = percentile_method)
  dat$attention_gap_score <- dat$threat_percentile - dat$attention_percentile

  cls <- rep("intermediate", nrow(dat))
  cls[is.na(dat$threat_score) | is.na(dat$threat_percentile)] <- "data_deficient_or_unscored"
  is_scored <- !is.na(dat$threat_percentile) & !is.na(dat$attention_percentile) & !is.na(dat$attention_gap_score)
  ht <- high_threat - tolerance
  la <- low_attention + tolerance
  ha <- high_attention - tolerance
  gt <- gap_threshold - tolerance
  cls[is_scored & dat$threat_percentile >= ht &
        dat$attention_percentile <= la &
        dat$attention_gap_score >= gt] <- "high_threat_low_attention"
  cls[is_scored & dat$threat_percentile >= ht &
        dat$attention_percentile >= ha] <- "high_threat_high_attention"
  cls[is_scored & dat$threat_percentile < (1 - high_threat + tolerance) &
        dat$attention_percentile >= ha] <- "low_threat_high_attention"
  dat$attention_gap_class <- cls

  near <- rep(FALSE, nrow(dat))
  near[is_scored & dat$attention_gap_class != "high_threat_low_attention" &
         dat$threat_percentile >= (high_threat - near_margin - tolerance) &
         dat$attention_percentile <= (low_attention + near_margin + tolerance) &
         dat$attention_gap_score >= (gap_threshold - near_margin - tolerance)] <- TRUE
  dat$near_high_threat_low_attention <- near
  dat$classification_rule <- paste0(
    "high_threat>=", signif(high_threat, 6),
    "; low_attention<=", signif(low_attention, 6),
    "; high_attention>=", signif(high_attention, 6),
    "; gap>=", signif(gap_threshold, 6),
    "; near_margin=", signif(near_margin, 6),
    "; tolerance=", signif(tolerance, 6),
    "; percentile=", percentile_method
  )
  dat <- dat[order(dat$attention_gap_score, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
  rownames(dat) <- NULL
  class(dat) <- c("culturomic_gap", class(dat))
  attr(dat, "n_unique_concepts") <- length(unique(dat$concept_id))
  dat
}

.cc_collapse_conservation_status <- function(conservation_status,
                                             status_col = "status",
                                             threat_score_col = NULL) {
  cs <- as.data.frame(conservation_status, stringsAsFactors = FALSE)
  .cc_required_cols(cs, "concept_id", "conservation_status")
  cs <- unique(cs)

  status_available <- rep(FALSE, nrow(cs))
  if (!is.null(threat_score_col) && threat_score_col %in% names(cs)) {
    status_available <- status_available | !is.na(suppressWarnings(as.numeric(cs[[threat_score_col]])))
  }
  if (status_col %in% names(cs)) {
    tmp <- toupper(trimws(as.character(cs[[status_col]])))
    status_available <- status_available | (!is.na(tmp) & !(tmp %in% c("", "NA", "N/A", "NULL", "UNKNOWN")))
  }
  sci_available <- if ("scientific_name" %in% names(cs)) !is.na(cs$scientific_name) & nzchar(as.character(cs$scientific_name)) else rep(FALSE, nrow(cs))
  cs$.cc_priority <- as.integer(status_available) * 10 + as.integer(sci_available)
  cs$.cc_order <- seq_len(nrow(cs))
  cs <- cs[order(cs$concept_id, -cs$.cc_priority, cs$.cc_order), , drop = FALSE]
  out <- cs[!duplicated(cs$concept_id), , drop = FALSE]
  out$.cc_priority <- NULL
  out$.cc_order <- NULL
  rownames(out) <- NULL
  out
}

#' Test robustness of attention-gap classifications to thresholds
#'
#' @param attention Attention index table.
#' @param conservation_status Conservation status table.
#' @param high_threat_grid Candidate high-threat percentile cutoffs.
#' @param low_attention_grid Candidate low-attention percentile cutoffs.
#' @param gap_threshold_grid Candidate minimum gap cutoffs.
#' @param ... Additional arguments passed to attention_gap.
#' @return Data frame with concept-level robustness summaries.
#' @export
attention_gap_sensitivity <- function(attention,
                                      conservation_status,
                                      high_threat_grid = c(0.60, 2/3, 0.75, 0.80),
                                      low_attention_grid = c(0.25, 0.33, 0.50),
                                      gap_threshold_grid = c(0.20, 0.25, 0.33),
                                      ...) {
  grid <- expand.grid(
    high_threat = high_threat_grid,
    low_attention = low_attention_grid,
    gap_threshold = gap_threshold_grid,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  runs <- vector("list", nrow(grid))
  for (i in seq_len(nrow(grid))) {
    g <- attention_gap(
      attention,
      conservation_status,
      high_threat = grid$high_threat[i],
      low_attention = grid$low_attention[i],
      gap_threshold = grid$gap_threshold[i],
      ...
    )
    gd <- as.data.frame(g)
    gd$high_threat <- grid$high_threat[i]
    gd$low_attention <- grid$low_attention[i]
    gd$gap_threshold <- grid$gap_threshold[i]
    gd$scenario_id <- i
    runs[[i]] <- gd
  }
  all_runs <- .cc_bind_rows_fill(runs)
  all_runs$is_high_threat_low_attention <- all_runs$attention_gap_class == "high_threat_low_attention"
  out <- lapply(split(all_runs, all_runs$concept_id), function(dat) {
    first <- dat[1, , drop = FALSE]
    data.frame(
      concept_id = first$concept_id,
      scientific_name = if ("scientific_name" %in% names(first)) first$scientific_name else NA_character_,
      status = if ("status" %in% names(first)) first$status else NA_character_,
      mean_attention = first$mean_attention,
      attention_percentile = first$attention_percentile,
      threat_percentile = first$threat_percentile,
      attention_gap_score = first$attention_gap_score,
      n_scenarios = nrow(dat),
      high_threat_low_attention_n = sum(dat$is_high_threat_low_attention, na.rm = TRUE),
      high_threat_low_attention_fraction = mean(dat$is_high_threat_low_attention, na.rm = TRUE),
      robust_high_threat_low_attention = mean(dat$is_high_threat_low_attention, na.rm = TRUE) >= 0.5,
      stringsAsFactors = FALSE
    )
  })
  out <- .cc_bind_rows_fill(out)
  out <- out[order(out$high_threat_low_attention_fraction, out$attention_gap_score, decreasing = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "scenario_results") <- all_runs
  class(out) <- c("attention_gap_sensitivity", class(out))
  out
}

#' @export
print.culturomic_gap <- function(x, ...) {
  cat("culturomic_gap\n")
  cat("  concepts: ", length(unique(x$concept_id)), "\n", sep = "")
  if ("attention_gap_class" %in% names(x)) print(table(x$attention_gap_class, useNA = "ifany"))
  invisible(x)
}

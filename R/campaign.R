#' Construct a control pool for campaign evaluation
#'
#' @param attention Attention index table.
#' @param treated_concepts Treated concept identifiers.
#' @param exclude Additional concepts excluded from controls.
#' @return Control concept identifiers.
#' @examples
#' ex <- con_culturomics_example_data()
#' dict <- score_ambiguity(ex$dictionary)
#' tr <- collect_attention(dict, from = "2024-01-01", to = "2024-03-31", mock = TRUE)
#' idx <- attention_index(standardise_attention(tr))
#' make_control_pool(idx, treated_concepts = "vaquita")
#' @export
make_control_pool <- function(attention, treated_concepts, exclude = NULL) {
  .cc_required_cols(attention, "concept_id", "attention")
  setdiff(unique(attention$concept_id), c(treated_concepts, exclude))
}

.cc_fit_campaign_once <- function(attention, campaign_date, treated_concepts, control_concepts,
                                  pre_days = 180, post_days = 90, value_col = "attention_index") {
  start_pre <- campaign_date - pre_days
  end_post <- campaign_date + post_days
  dat <- attention[attention$date >= start_pre & attention$date <= end_post, , drop = FALSE]
  dat$group <- ifelse(dat$concept_id %in% treated_concepts, "treated", ifelse(dat$concept_id %in% control_concepts, "control", NA))
  dat <- dat[!is.na(dat$group), , drop = FALSE]

  daily <- stats::aggregate(as.numeric(dat[[value_col]]), dat[c("date", "group")], mean, na.rm = TRUE)
  names(daily)[3] <- "value"
  daily$date <- .cc_as_date(daily$date)
  treated <- daily[daily$group == "treated", c("date", "value")]
  control <- daily[daily$group == "control", c("date", "value")]
  names(treated)[2] <- "treated"
  names(control)[2] <- "control"
  merged <- merge(treated, control, by = "date", all = FALSE)
  merged$date <- .cc_as_date(merged$date)
  if (nrow(merged) < 10) stop("Not enough overlapping treated and control observations.", call. = FALSE)
  pre <- merged[merged$date < campaign_date, , drop = FALSE]
  post <- merged[merged$date >= campaign_date, , drop = FALSE]
  if (nrow(pre) < 5 || nrow(post) < 1) stop("Pre or post campaign window is too small.", call. = FALSE)

  fit <- stats::lm(treated ~ control, data = pre)
  merged$counterfactual <- as.numeric(stats::predict(fit, newdata = merged))
  ## Public plotting/reporting API uses observed/counterfactual naming.
  ## Keep treated/control as backwards-compatible internal columns.
  merged$observed <- merged$treated
  merged$control_attention <- merged$control
  merged$effect <- merged$observed - merged$counterfactual
  merged$period <- ifelse(merged$date < campaign_date, "pre", "post")
  pre_eff <- merged[merged$date < campaign_date, , drop = FALSE]
  pre_rmse <- sqrt(mean(pre_eff$effect^2, na.rm = TRUE))
  pre_sd <- stats::sd(pre_eff$effect, na.rm = TRUE)
  denom <- pre_sd
  if (!is.finite(denom) || denom == 0) denom <- pre_rmse
  if (!is.finite(denom) || denom == 0) denom <- NA_real_
  merged$standardised_effect <- merged$effect / denom
  pre_eff <- merged[merged$date < campaign_date, , drop = FALSE]
  post_eff <- merged[merged$date >= campaign_date, , drop = FALSE]
  list(
    fit = fit,
    daily_effects = merged,
    average_pre_effect = mean(pre_eff$effect, na.rm = TRUE),
    pre_rmse = pre_rmse,
    pre_sd = pre_sd,
    standardisation_denominator = denom,
    average_post_effect = mean(post_eff$effect, na.rm = TRUE),
    cumulative_post_effect = sum(post_eff$effect, na.rm = TRUE),
    max_post_effect = max(post_eff$effect, na.rm = TRUE),
    min_post_effect = min(post_eff$effect, na.rm = TRUE),
    standardised_average_post_effect = mean(post_eff$standardised_effect, na.rm = TRUE)
  )
}

.cc_campaign_placebo <- function(attention, campaign_date, treated_concepts, control_concepts,
                                 pre_days, post_days, value_col, placebo_n, placebo_seed,
                                 exclude_event = TRUE) {
  if (is.null(placebo_n) || placebo_n <= 0) return(NULL)
  set.seed(placebo_seed)
  dates <- sort(unique(.cc_as_date(attention$date)))
  min_date <- min(dates, na.rm = TRUE) + pre_days
  max_date <- max(dates, na.rm = TRUE) - post_days
  eligible <- dates[dates >= min_date & dates <= max_date & dates != campaign_date]
  if (isTRUE(exclude_event)) {
    ## A placebo window that reaches the real post-event period would carry
    ## the real effect into the reference distribution.
    overlaps <- (eligible - pre_days) <= (campaign_date + post_days) & (eligible + post_days) >= campaign_date
    eligible <- eligible[!overlaps]
  }
  if (length(eligible) == 0) {
    warning("No eligible placebo dates: the series is too short for pre_days + post_days outside the event window.", call. = FALSE)
    return(NULL)
  }
  if (length(eligible) < placebo_n) {
    warning("Only ", length(eligible), " distinct placebo dates are available (placebo_n = ", placebo_n,
            "); all are used once. The smallest attainable placebo p-value is ",
            signif(1 / (1 + length(eligible)), 3), ".", call. = FALSE)
  }
  ## Each date is used at most once: repeated dates are not independent placebos.
  sampled <- sort(sample(eligible, size = min(placebo_n, length(eligible)), replace = FALSE))
  rows <- vector("list", length(sampled))
  for (i in seq_along(sampled)) {
    val <- tryCatch(
      .cc_fit_campaign_once(attention, sampled[i], treated_concepts, control_concepts, pre_days, post_days, value_col),
      error = function(e) NULL
    )
    if (is.null(val)) next
    rows[[i]] <- data.frame(
      placebo_date = sampled[i],
      average_post_effect = val$average_post_effect,
      cumulative_post_effect = val$cumulative_post_effect,
      standardised_average_post_effect = val$standardised_average_post_effect,
      stringsAsFactors = FALSE
    )
  }
  out <- .cc_bind_rows_fill(rows[!vapply(rows, is.null, logical(1))])
  if (is.null(out)) data.frame() else out
}

#' Estimate campaign-associated deviation using a counterfactual attention model
#'
#' @param attention Attention index table.
#' @param campaign_date Campaign date.
#' @param treated_concepts Treated concept identifiers.
#' @param control_concepts Optional control concepts.
#' @param pre_days Pre-campaign window.
#' @param post_days Post-campaign window.
#' @param value_col Attention value column.
#' @param placebo_n Maximum number of distinct placebo dates. Dates are drawn
#'   without replacement; when fewer are eligible, all are used and a warning
#'   gives the smallest attainable p-value, 1 / (1 + number of placebos).
#' @param placebo_seed Random seed for placebo dates.
#' @param placebo Backwards-compatible logical alias. If FALSE, disables placebo diagnostics.
#' @param n_placebo Backwards-compatible alias for placebo_n.
#' @param placebo_exclude_event Exclude placebo dates whose pre/post window
#'   overlaps the real post-event period.
#' @param alternative Direction of the placebo test. "two.sided" (default)
#'   compares absolute effects; "greater" or "less" test a direction stated
#'   before looking at the data (e.g. that an event increases attention).
#' @return campaign_effect object. The two-sided placebo p-value is
#'   (1 + number of placebos with |effect| >= |observed|) / (1 + number of
#'   placebos); one-sided p-values count placebos with effect >= observed
#'   ("greater") or <= observed ("less"). placebo_alternative records the
#'   direction, and placebo_n_used and min_attainable_p report the resolution.
#' @examples
#' ex <- con_culturomics_example_data()
#' tr <- collect_attention(ex$dictionary, from = "2023-06-01", to = "2024-06-30", mock = TRUE)
#' idx <- attention_index(standardise_attention(tr))
#' ce <- campaign_effect(idx, campaign_date = "2024-03-01", treated_concepts = "vaquita",
#'                       pre_days = 90, post_days = 30, placebo_n = 20)
#' ce
#' @export
campaign_effect <- function(attention,
                            campaign_date,
                            treated_concepts,
                            control_concepts = NULL,
                            pre_days = 180,
                            post_days = 90,
                            value_col = "attention_index",
                            placebo_n = 100,
                            placebo_seed = 1,
                            placebo = NULL,
                            n_placebo = NULL,
                            placebo_exclude_event = TRUE,
                            alternative = c("two.sided", "greater", "less")) {
  alternative <- match.arg(alternative)
  if (!is.null(n_placebo)) placebo_n <- n_placebo
  if (!is.null(placebo) && isFALSE(placebo)) placebo_n <- 0
  if (!is.null(placebo) && !is.logical(placebo)) {
    stop("placebo must be TRUE or FALSE when supplied.", call. = FALSE)
  }
  if (!is.numeric(placebo_n) || length(placebo_n) != 1 || is.na(placebo_n)) {
    stop("placebo_n must be a single non-missing numeric value.", call. = FALSE)
  }
  placebo_n <- max(0, as.integer(placebo_n))
  attention <- as.data.frame(attention, stringsAsFactors = FALSE)
  .cc_required_cols(attention, c("date", "concept_id", value_col), "attention")
  attention$date <- .cc_as_date(attention$date)
  campaign_date <- .cc_as_date(campaign_date)
  if (is.null(control_concepts)) control_concepts <- make_control_pool(attention, treated_concepts)

  main <- .cc_fit_campaign_once(attention, campaign_date, treated_concepts, control_concepts, pre_days, post_days, value_col)
  placebo <- .cc_campaign_placebo(attention, campaign_date, treated_concepts, control_concepts,
                                  pre_days, post_days, value_col, placebo_n, placebo_seed,
                                  exclude_event = placebo_exclude_event)
  placebo_p_value <- NA_real_
  placebo_effect_mean <- NA_real_
  placebo_effect_sd <- NA_real_
  effect_percentile_against_placebo <- NA_real_
  placebo_n_used <- 0L
  if (!is.null(placebo) && nrow(placebo) > 0) {
    placebo_n_used <- sum(is.finite(placebo$average_post_effect))
    obs <- abs(main$average_post_effect)
    placebo_abs <- abs(placebo$average_post_effect)
    pe <- placebo$average_post_effect
    exceed <- switch(alternative,
      two.sided = placebo_abs >= obs,
      greater = pe >= main$average_post_effect,
      less = pe <= main$average_post_effect)
    placebo_p_value <- (1 + sum(exceed, na.rm = TRUE)) / (1 + sum(is.finite(pe)))
    placebo_effect_mean <- mean(placebo$average_post_effect, na.rm = TRUE)
    placebo_effect_sd <- stats::sd(placebo$average_post_effect, na.rm = TRUE)
    effect_percentile_against_placebo <- mean(placebo_abs <= obs, na.rm = TRUE)
  }

  out <- c(
    list(
      campaign_date = campaign_date,
      treated_concepts = treated_concepts,
      control_concepts = control_concepts,
      model = main$fit,
      daily_effects = main$daily_effects,
      placebo_effects = placebo,
      placebo_p_value = placebo_p_value,
      placebo_alternative = alternative,
      placebo_effect_mean = placebo_effect_mean,
      placebo_effect_sd = placebo_effect_sd,
      effect_percentile_against_placebo = effect_percentile_against_placebo,
      placebo_n_used = placebo_n_used,
      min_attainable_p = if (placebo_n_used > 0) 1 / (1 + placebo_n_used) else NA_real_,
      method_note = "Pre-period calibrated controlled interrupted time-series; interpret as campaign-associated attention deviation, not causal proof."
    ),
    main[setdiff(names(main), c("fit", "daily_effects"))]
  )
  class(out) <- "campaign_effect"
  out
}

#' @export
print.campaign_effect <- function(x, ...) {
  cat("campaign_effect\n")
  cat("  campaign_date: ", as.character(x$campaign_date), "\n", sep = "")
  cat("  average_pre_effect: ", round(x$average_pre_effect, 3), "\n", sep = "")
  cat("  pre_rmse: ", round(x$pre_rmse, 3), "\n", sep = "")
  cat("  average_post_effect: ", round(x$average_post_effect, 3), "\n", sep = "")
  cat("  cumulative_post_effect: ", round(x$cumulative_post_effect, 3), "\n", sep = "")
  cat("  standardised_average_post_effect: ", round(x$standardised_average_post_effect, 3), "\n", sep = "")
  if (!is.na(x$placebo_p_value)) {
    alt <- if (is.null(x$placebo_alternative)) "two.sided" else x$placebo_alternative
    cat("  placebo_p_value (", alt, "): ", round(x$placebo_p_value, 3), " (", x$placebo_n_used,
        " distinct placebo dates; smallest attainable p = ", signif(x$min_attainable_p, 3), ")\n", sep = "")
  }
  invisible(x)
}

#' Build a markdown report for culturomics results
#'
#' @param x Object returned by attention_gap, campaign_effect, or query_sensitivity.
#' @param title Report title.
#' @return Character vector with markdown text.
#' @export
culturomic_report <- function(x, title = "Conservation culturomics report") {
  lines <- c(paste0("# ", title), "")
  if (inherits(x, "culturomic_gap")) {
    tab <- table(x$attention_gap_class, useNA = "ifany")
    n_concepts <- length(unique(x$concept_id))
    top_under <- x[x$attention_gap_class == "high_threat_low_attention", , drop = FALSE]
    top_under <- top_under[order(top_under$attention_gap_score, decreasing = TRUE), , drop = FALSE]
    label <- if ("scientific_name" %in% names(top_under)) {
      ifelse(is.na(top_under$scientific_name) | !nzchar(as.character(top_under$scientific_name)), top_under$concept_id, as.character(top_under$scientific_name))
    } else {
      top_under$concept_id
    }
    top_lines <- if (nrow(top_under) == 0) {
      "None under the current strict thresholds."
    } else {
      paste0("- ", label, " (", top_under$concept_id, "): gap score ", round(top_under$attention_gap_score, 3), collapse = "\n")
    }
    near <- if ("near_high_threat_low_attention" %in% names(x)) x[x$near_high_threat_low_attention %in% TRUE, , drop = FALSE] else x[FALSE, , drop = FALSE]
    near <- near[order(near$attention_gap_score, decreasing = TRUE), , drop = FALSE]
    near_label <- if (nrow(near) > 0 && "scientific_name" %in% names(near)) {
      ifelse(is.na(near$scientific_name) | !nzchar(as.character(near$scientific_name)), near$concept_id, as.character(near$scientific_name))
    } else if (nrow(near) > 0) near$concept_id else character()
    near_lines <- if (nrow(near) == 0) {
      "None."
    } else {
      paste0("- ", near_label, " (", near$concept_id, "): gap score ", round(near$attention_gap_score, 3), collapse = "\n")
    }
    lines <- c(
      lines,
      "## Attention gap summary",
      "",
      paste0("Analysed concepts: ", n_concepts),
      paste0("High threat and low attention concepts: ", sum(x$attention_gap_class == "high_threat_low_attention", na.rm = TRUE)),
      paste0("Near-threshold high-threat low-attention candidates: ", nrow(near)),
      paste0("Data deficient or unscored concepts: ", sum(x$attention_gap_class == "data_deficient_or_unscored", na.rm = TRUE)),
      "",
      "### Class counts",
      "",
      paste(names(tab), as.integer(tab), sep = ": ", collapse = "\n"),
      "",
      "### Under-attended high-threat concepts",
      "",
      top_lines,
      "",
      "### Near-threshold candidates",
      "",
      near_lines
    )
  } else if (inherits(x, "campaign_effect")) {
    lines <- c(
      lines,
      "## Campaign effect summary",
      "",
      paste0("Campaign date: ", as.character(x$campaign_date)),
      paste0("Average pre-campaign effect: ", round(x$average_pre_effect, 3)),
      paste0("Pre-campaign RMSE: ", round(x$pre_rmse, 3)),
      paste0("Average post-campaign effect: ", round(x$average_post_effect, 3)),
      paste0("Standardised average post-campaign effect: ", ifelse(is.finite(x$standardised_average_post_effect), round(x$standardised_average_post_effect, 3), "not available")),
      paste0("Cumulative post-campaign effect: ", round(x$cumulative_post_effect, 3)),
      paste0("Maximum post-campaign effect: ", round(x$max_post_effect, 3)),
      paste0("Minimum post-campaign effect: ", round(x$min_post_effect, 3)),
      paste0("Placebo p-value: ", ifelse(is.finite(x$placebo_p_value), round(x$placebo_p_value, 3), "not available")),
      paste0("Effect percentile against placebo dates: ", ifelse(is.finite(x$effect_percentile_against_placebo), round(x$effect_percentile_against_placebo, 3), "not available")),
      "",
      "Interpretation note: this is a pre-period calibrated counterfactual deviation, not causal proof."
    )
  } else if (is.data.frame(x) && "query_sensitivity_index" %in% names(x)) {
    top <- x[order(x$query_sensitivity_index, decreasing = TRUE), , drop = FALSE]
    top <- utils::head(top, 5)
    influence_col <- if ("max_leave_one_query_influence" %in% names(top)) "max_leave_one_query_influence" else "query_sensitivity_index"
    top_lines <- paste0(
      "- ", top$concept_id,
      ": range ", round(top$query_sensitivity_index, 3),
      "; sd ", ifelse("query_sensitivity_sd" %in% names(top), round(top$query_sensitivity_sd, 3), "NA"),
      "; max leave-one-query influence ", round(top[[influence_col]], 3),
      " (", top$most_influential_query, ")",
      collapse = "\n"
    )
    lines <- c(
      lines,
      "## Query sensitivity summary",
      "",
      paste0("Analysed concepts: ", nrow(x)),
      paste0("Mean query sensitivity range: ", round(mean(x$query_sensitivity_index, na.rm = TRUE), 3)),
      paste0("Mean query sensitivity SD: ", ifelse("query_sensitivity_sd" %in% names(x), round(mean(x$query_sensitivity_sd, na.rm = TRUE), 3), "not available")),
      paste0("Mean max leave-one-query influence: ", ifelse("max_leave_one_query_influence" %in% names(x), round(mean(x$max_leave_one_query_influence, na.rm = TRUE), 3), "not available")),
      "",
      "### Most query-sensitive concepts",
      "",
      top_lines
    )
  } else if (inherits(x, "attention_gap_sensitivity") || (is.data.frame(x) && "high_threat_low_attention_fraction" %in% names(x))) {
    top <- x[order(x$high_threat_low_attention_fraction, decreasing = TRUE), , drop = FALSE]
    top <- utils::head(top, 5)
    label <- if ("scientific_name" %in% names(top)) {
      ifelse(is.na(top$scientific_name) | !nzchar(as.character(top$scientific_name)), top$concept_id, as.character(top$scientific_name))
    } else top$concept_id
    top_lines <- paste0("- ", label, " (", top$concept_id, "): robust fraction ", round(top$high_threat_low_attention_fraction, 3), collapse = "\n")
    lines <- c(
      lines,
      "## Attention gap sensitivity summary",
      "",
      paste0("Analysed concepts: ", nrow(x)),
      paste0("Robust high-threat low-attention concepts: ", sum(x$robust_high_threat_low_attention, na.rm = TRUE)),
      "",
      "### Most robust under-attended high-threat concepts",
      "",
      top_lines
    )
  } else {
    lines <- c(lines, "No specialised report method was available for this object.")
  }
  lines
}

#' Write culturomics report
#' @param x Result object.
#' @param file Output file.
#' @param title Report title.
#' @return File path invisibly.
#' @export
write_culturomic_report <- function(x, file, title = "Conservation culturomics report") {
  lines <- if (is.character(x)) x else culturomic_report(x, title = title)
  writeLines(lines, con = file)
  invisible(file)
}

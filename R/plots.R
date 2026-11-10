.cc_plot_require_ggplot2 <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for this plotting function.", call. = FALSE)
  }
  invisible(TRUE)
}

.cc_label_table <- function(dat, label_table = NULL, label_col = "common_name") {
  dat <- as.data.frame(dat, stringsAsFactors = FALSE)
  if (is.null(label_table)) return(dat)
  label_table <- as.data.frame(label_table, stringsAsFactors = FALSE)
  if (!"concept_id" %in% names(label_table)) return(dat)
  keep <- unique(c("concept_id", intersect(c(label_col, "scientific_name", "common_name", "status"), names(label_table))))
  label_table <- unique(label_table[keep])
  merge(dat, label_table, by = "concept_id", all.x = TRUE)
}

.cc_display_label <- function(dat, preferred = "common_name") {
  dat <- as.data.frame(dat, stringsAsFactors = FALSE)
  if (preferred %in% names(dat)) {
    lab <- as.character(dat[[preferred]])
  } else if ("common_name" %in% names(dat)) {
    lab <- as.character(dat$common_name)
  } else if ("scientific_name" %in% names(dat)) {
    lab <- as.character(dat$scientific_name)
  } else {
    lab <- as.character(dat$concept_id)
  }
  bad <- is.na(lab) | !nzchar(lab)
  lab[bad] <- as.character(dat$concept_id[bad])
  lab
}

.cc_floor_date <- function(x, unit = c("day", "week", "month")) {
  unit <- match.arg(unit)
  x <- .cc_as_date(x)
  if (unit == "day") return(x)
  if (unit == "month") return(as.Date(paste0(format(x, "%Y-%m"), "-01")))
  x - as.POSIXlt(x)$wday
}

.cc_collapse_frame <- function(frame) {
  frame <- as.character(frame)
  out <- frame
  out[frame %in% c("extinction", "threat", "habitat", "pollution", "climate")] <- "threat_pressure"
  out[frame %in% c("protection", "policy", "recovery")] <- "conservation_action"
  out[frame %in% c("charisma", "tourism")] <- "public_appeal"
  out[frame %in% c("trade", "conflict", "disease", "welfare")] <- "human_wildlife_conflict"
  out[is.na(frame) | !nzchar(frame) | frame %in% c("none", "unclassified")] <- "unclassified"
  out
}

#' Plot query ambiguity audit
#'
#' @param x Dictionary or output from [audit_queries()].
#' @param max_ambiguity Disambiguation threshold drawn as a dashed line.
#' @return A ggplot object.
#' @examples
#' dat <- con_culturomics_example_data()
#' dict <- score_ambiguity(dat$dictionary)
#' if (requireNamespace("ggplot2", quietly = TRUE)) plot_query_ambiguity(dict)
#' @export
plot_query_ambiguity <- function(x, max_ambiguity = 0.60) {
  .cc_plot_require_ggplot2()
  dat <- as.data.frame(x, stringsAsFactors = FALSE)
  if (!"query_status" %in% names(dat)) dat <- audit_queries(dat, max_ambiguity = max_ambiguity)
  .cc_required_cols(dat, c("ambiguity_score", "ambiguity_class", "query_type"), "x")
  dat$ambiguity_class <- factor(dat$ambiguity_class, levels = c("low", "medium", "high"))
  ## Points carry every query, so the boxplot draws no separate outliers and
  ## jitter is horizontal only to keep scores at their true values.
  ggplot2::ggplot(dat, ggplot2::aes(x = query_type, y = ambiguity_score)) +
    ggplot2::geom_hline(yintercept = max_ambiguity, linetype = 2, colour = "grey45") +
    ggplot2::geom_boxplot(outlier.shape = NA, fill = NA, colour = "grey55", width = 0.6) +
    ggplot2::geom_point(ggplot2::aes(colour = ambiguity_class, shape = ambiguity_class),
                        position = ggplot2::position_jitter(width = 0.15, height = 0, seed = 1),
                        size = 2.2, alpha = 0.85) +
    ggplot2::scale_colour_manual(values = .cc_ambiguity_colours, drop = TRUE) +
    ggplot2::scale_shape_manual(values = c(low = 16, medium = 17, high = 15), drop = TRUE) +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::labs(x = "Query type", y = "Ambiguity score", colour = "Ambiguity", shape = "Ambiguity") +
    .cc_plot_theme()
}

## Okabe-Ito colours, distinguishable under the common forms of colour blindness.
.cc_gap_class_colours <- c(
  high_threat_low_attention = "#D55E00",
  high_threat_high_attention = "#0072B2",
  low_threat_high_attention = "#009E73",
  intermediate = "#999999",
  data_deficient_or_unscored = "#DDDDDD"
)
.cc_gap_class_labels <- c(
  high_threat_low_attention = "High threat, low attention",
  high_threat_high_attention = "High threat, high attention",
  low_threat_high_attention = "Low threat, high attention",
  intermediate = "Intermediate",
  data_deficient_or_unscored = "Unscored"
)
.cc_ambiguity_colours <- c(low = "#56B4E9", medium = "#E69F00", high = "#D55E00")

## Day and English month abbreviation, independent of the system locale.
.cc_date_label <- function(d) {
  ifelse(is.na(d), NA_character_, paste(as.integer(format(d, "%d")), month.abb[as.integer(format(d, "%m"))]))
}

.cc_plot_theme <- function() {
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   legend.position = "bottom",
                   axis.text.x = ggplot2::element_text(angle = 0))
}

## Thresholds used by attention_gap(), read from its classification_rule column.
.cc_gap_rule <- function(gap) {
  out <- list(high_threat = 2/3, low_attention = 0.50, high_attention = 2/3)
  rule <- if ("classification_rule" %in% names(gap)) as.character(gap$classification_rule[1]) else NA_character_
  if (is.na(rule)) return(out)
  for (nm in names(out)) {
    m <- regmatches(rule, regexpr(paste0(nm, "[<>]=[0-9.eE+-]+"), rule))
    if (length(m) == 1) out[[nm]] <- as.numeric(sub("^.*=", "", m))
  }
  out
}

#' Plot high-ambiguity queries
#' @param x Dictionary or output from [audit_queries()].
#' @param top_n Number of queries to show.
#' @return A ggplot object.
#' @export
plot_query_ambiguity_top <- function(x, top_n = 20) {
  .cc_plot_require_ggplot2()
  dat <- as.data.frame(x, stringsAsFactors = FALSE)
  if (!"query_status" %in% names(dat)) dat <- audit_queries(dat)
  .cc_required_cols(dat, c("query", "ambiguity_score", "ambiguity_class"), "x")
  dat <- dat[order(dat$ambiguity_score, decreasing = TRUE), , drop = FALSE]
  dat <- dat[seq_len(min(top_n, nrow(dat))), , drop = FALSE]
  dat$label <- paste0(dat$query, " [", dat$concept_id, "]")
  ggplot2::ggplot(dat, ggplot2::aes(x = stats::reorder(label, ambiguity_score), y = ambiguity_score)) +
    ggplot2::geom_col(ggplot2::aes(fill = ambiguity_class)) +
    ggplot2::coord_flip(ylim = c(0, 1)) +
    ggplot2::labs(x = "Query", y = "Ambiguity score", fill = "Ambiguity") +
    ggplot2::theme_minimal()
}

#' Plot query-type composition
#' @param dictionary Concept dictionary.
#' @return A ggplot object.
#' @export
plot_query_type_mix <- function(dictionary) {
  .cc_plot_require_ggplot2()
  dat <- as.data.frame(dictionary, stringsAsFactors = FALSE)
  .cc_required_cols(dat, c("concept_id", "query_type"), "dictionary")
  tab <- as.data.frame(table(dat$concept_id, dat$query_type), stringsAsFactors = FALSE)
  names(tab) <- c("concept_id", "query_type", "n")
  tab <- tab[tab$n > 0, , drop = FALSE]
  ggplot2::ggplot(tab, ggplot2::aes(x = concept_id, y = n, fill = query_type)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "Concept", y = "Number of dictionary queries", fill = "Query type") +
    ggplot2::theme_minimal()
}

#' Plot attention time series
#' @param attention Attention index table.
#' @param label_table Optional table with concept labels.
#' @param label_col Label column.
#' @param top_n Optional number of most attended concepts to show.
#' @param facet Whether to facet by concept.
#' @param value_col Attention column.
#' @param time_unit Date aggregation unit.
#' @param smooth Whether to add a smooth trend.
#' @return A ggplot object.
#' @export
plot_attention_timeseries <- function(attention,
                                      label_table = NULL,
                                      label_col = "common_name",
                                      top_n = NULL,
                                      facet = TRUE,
                                      value_col = "attention_index",
                                      time_unit = c("day", "week", "month"),
                                      smooth = FALSE) {
  .cc_plot_require_ggplot2()
  time_unit <- match.arg(time_unit)
  dat <- as.data.frame(attention, stringsAsFactors = FALSE)
  .cc_required_cols(dat, c("date", "concept_id", value_col), "attention")
  dat$date <- .cc_floor_date(dat$date, time_unit)
  dat <- .cc_label_table(dat, label_table, label_col)
  dat$label <- .cc_display_label(dat, label_col)
  dat$.plot_value <- as.numeric(dat[[value_col]])
  dat <- stats::aggregate(.plot_value ~ date + concept_id + label, data = dat, FUN = mean, na.rm = TRUE)
  if (!is.null(top_n) && is.finite(top_n)) {
    means <- stats::aggregate(.plot_value ~ label, data = dat, FUN = mean, na.rm = TRUE)
    keep <- means$label[order(means$.plot_value, decreasing = TRUE)][seq_len(min(top_n, nrow(means)))]
    dat <- dat[dat$label %in% keep, , drop = FALSE]
  }
  p <- ggplot2::ggplot(dat, ggplot2::aes(x = date, y = .plot_value, group = label)) +
    ggplot2::geom_hline(yintercept = 0, linetype = 3, alpha = 0.5) +
    ggplot2::labs(x = "Date", y = "Attention index") +
    ggplot2::theme_minimal()
  if (isTRUE(smooth) && nrow(dat) > 6) {
    p <- p + ggplot2::geom_line(alpha = 0.25, linewidth = 0.35) + ggplot2::geom_smooth(se = FALSE, linewidth = 0.75)
  } else {
    p <- p + ggplot2::geom_line(alpha = 0.76, linewidth = 0.55)
  }
  p <- p + ggplot2::scale_x_date(date_breaks = if (time_unit == "month") "2 months" else "3 months", date_labels = "%Y-%m")
  if (isTRUE(facet)) p + ggplot2::facet_wrap(~label, scales = "free_y") else p + ggplot2::aes(linetype = label)
}

#' Plot attention heatmap
#' @param attention Attention index table.
#' @param label_table Optional label table.
#' @param label_col Label column.
#' @param value_col Attention column.
#' @return A ggplot object.
#' @export
plot_attention_heatmap <- function(attention, label_table = NULL, label_col = "common_name", value_col = "attention_index") {
  .cc_plot_require_ggplot2()
  dat <- as.data.frame(attention, stringsAsFactors = FALSE)
  .cc_required_cols(dat, c("date", "concept_id", value_col), "attention")
  dat$date <- .cc_as_date(dat$date)
  dat$month <- format(dat$date, "%Y-%m")
  dat <- .cc_label_table(dat, label_table, label_col)
  dat$label <- .cc_display_label(dat, label_col)
  agg <- stats::aggregate(as.numeric(dat[[value_col]]), dat[c("label", "month")], mean, na.rm = TRUE)
  names(agg)[3] <- "attention_index"
  ggplot2::ggplot(agg, ggplot2::aes(x = month, y = label, fill = attention_index)) +
    ggplot2::geom_tile() +
    ggplot2::labs(x = "Month", y = "Concept", fill = "Attention") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

#' Plot platform disagreement
#' @param attention Attention index table.
#' @param label_table Optional label table.
#' @param label_col Label column.
#' @return A ggplot object.
#' @export
plot_platform_disagreement <- function(attention, label_table = NULL, label_col = "common_name") {
  .cc_plot_require_ggplot2()
  dat <- as.data.frame(attention, stringsAsFactors = FALSE)
  .cc_required_cols(dat, c("concept_id", "platform_disagreement"), "attention")
  dat <- .cc_label_table(dat, label_table, label_col)
  dat$label <- .cc_display_label(dat, label_col)
  agg <- stats::aggregate(dat$platform_disagreement, dat["label"], mean, na.rm = TRUE)
  names(agg)[2] <- "platform_disagreement"
  agg <- agg[order(agg$platform_disagreement, decreasing = TRUE), , drop = FALSE]
  ggplot2::ggplot(agg, ggplot2::aes(x = stats::reorder(label, platform_disagreement), y = platform_disagreement)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "Concept", y = "Mean platform disagreement") +
    ggplot2::theme_minimal()
}

#' Plot query sensitivity
#' @param sensitivity Output from [query_sensitivity()].
#' @param label_table Optional table with concept labels.
#' @param label_col Label column.
#' @param metric Sensitivity column to plot.
#' @return A ggplot object.
#' @export
plot_query_sensitivity <- function(sensitivity, label_table = NULL, label_col = "common_name", metric = "query_sensitivity_sd") {
  .cc_plot_require_ggplot2()
  dat <- as.data.frame(sensitivity, stringsAsFactors = FALSE)
  if (!metric %in% names(dat)) metric <- "query_sensitivity_index"
  .cc_required_cols(dat, c("concept_id", metric), "sensitivity")
  dat <- .cc_label_table(dat, label_table, label_col)
  dat$label <- .cc_display_label(dat, label_col)
  dat <- dat[order(dat[[metric]], decreasing = TRUE), , drop = FALSE]
  dat$.plot_metric <- as.numeric(dat[[metric]])
  ggplot2::ggplot(dat, ggplot2::aes(x = stats::reorder(label, .plot_metric), y = .plot_metric)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "Concept", y = metric) +
    ggplot2::theme_minimal()
}

#' Plot attention gap
#' @param gap Output from [attention_gap()].
#' @param label_table Optional table containing concept labels.
#' @param label_col Label column.
#' @param label_top_n Number of highest-gap concepts to label in addition to
#'   all concepts in a non-intermediate class and near-threshold cases.
#' @return A ggplot object. Threshold lines are taken from the
#'   classification rule stored by [attention_gap()]; the shaded corner is
#'   the high-threat low-attention region. Labels use ggrepel when installed.
#' @export
plot_attention_gap <- function(gap, label_table = NULL, label_col = "common_name", label_top_n = 0) {
  .cc_plot_require_ggplot2()
  gap <- as.data.frame(gap, stringsAsFactors = FALSE)
  .cc_required_cols(gap, c("attention_percentile", "threat_percentile", "attention_gap_class"), "gap")
  rule <- .cc_gap_rule(gap)
  gap <- .cc_label_table(gap, label_table, label_col)
  gap$.plot_label <- .cc_display_label(gap, label_col)
  keep <- gap$attention_gap_class %in% setdiff(names(.cc_gap_class_colours), c("intermediate", "data_deficient_or_unscored"))
  if ("near_high_threat_low_attention" %in% names(gap)) keep <- keep | gap$near_high_threat_low_attention %in% TRUE
  if (!is.null(label_top_n) && is.finite(label_top_n) && label_top_n > 0 && "attention_gap_score" %in% names(gap)) {
    ord <- order(gap$attention_gap_score, decreasing = TRUE, na.last = TRUE)
    keep[ord[seq_len(min(label_top_n, length(ord)))]] <- TRUE
  }
  gap$.plot_label[!keep | is.na(gap$.plot_label)] <- ""
  gap$attention_gap_class <- factor(gap$attention_gap_class, levels = names(.cc_gap_class_colours))
  lab_dat <- gap[nzchar(gap$.plot_label), , drop = FALSE]

  label_layer <- if (requireNamespace("ggrepel", quietly = TRUE)) {
    ggrepel::geom_text_repel(data = lab_dat, ggplot2::aes(label = .plot_label), size = 3,
                             min.segment.length = 0, box.padding = 0.4, seed = 1, max.overlaps = Inf)
  } else {
    ggplot2::geom_text(data = lab_dat, ggplot2::aes(label = .plot_label), size = 3, vjust = -0.9)
  }
  ggplot2::ggplot(gap, ggplot2::aes(x = attention_percentile, y = threat_percentile)) +
    ggplot2::annotate("rect", xmin = -Inf, xmax = rule$low_attention, ymin = rule$high_threat, ymax = Inf,
                      fill = .cc_gap_class_colours[["high_threat_low_attention"]], alpha = 0.08) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 2, colour = "grey55") +
    ggplot2::geom_hline(yintercept = rule$high_threat, linetype = 3, colour = "grey35") +
    ggplot2::geom_vline(xintercept = c(rule$low_attention, rule$high_attention), linetype = 3, colour = "grey35") +
    ggplot2::geom_point(ggplot2::aes(colour = attention_gap_class, shape = attention_gap_class), size = 2.8) +
    label_layer +
    ggplot2::scale_colour_manual(values = .cc_gap_class_colours, labels = .cc_gap_class_labels, drop = TRUE) +
    ggplot2::scale_shape_manual(values = c(high_threat_low_attention = 17, high_threat_high_attention = 15,
                                           low_threat_high_attention = 18, intermediate = 16,
                                           data_deficient_or_unscored = 1),
                                labels = .cc_gap_class_labels, drop = TRUE) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
    ggplot2::labs(x = "Attention percentile", y = "Reference (threat) percentile", colour = NULL, shape = NULL) +
    .cc_plot_theme() +
    ggplot2::guides(colour = ggplot2::guide_legend(nrow = 2), shape = ggplot2::guide_legend(nrow = 2))
}

#' Plot ranked attention gap
#' @param gap Output from [attention_gap()].
#' @param label_table Optional concept label table.
#' @param label_col Label column.
#' @return A ggplot object.
#' @export
plot_attention_gap_rank <- function(gap, label_table = NULL, label_col = "common_name") {
  .cc_plot_require_ggplot2()
  dat <- as.data.frame(gap, stringsAsFactors = FALSE)
  .cc_required_cols(dat, c("concept_id", "attention_gap_score"), "gap")
  dat <- .cc_label_table(dat, label_table, label_col)
  dat$label <- .cc_display_label(dat, label_col)
  dat <- dat[order(dat$attention_gap_score, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
  if (!"attention_gap_class" %in% names(dat)) dat$attention_gap_class <- "intermediate"
  dat$attention_gap_class <- factor(dat$attention_gap_class, levels = names(.cc_gap_class_colours))
  ggplot2::ggplot(dat, ggplot2::aes(x = stats::reorder(label, attention_gap_score), y = attention_gap_score)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey35") +
    ggplot2::geom_col(ggplot2::aes(fill = attention_gap_class), width = 0.75) +
    ggplot2::scale_fill_manual(values = .cc_gap_class_colours, labels = .cc_gap_class_labels, drop = TRUE) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = "Attention gap (reference minus attention percentile)", fill = NULL) +
    .cc_plot_theme() +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2))
}

#' Plot attention-gap threshold sensitivity
#' @param sensitivity Output from [attention_gap_sensitivity()].
#' @param label_table Optional table with concept labels.
#' @param label_col Label column.
#' @param show_zero Show concepts never classed high-threat low-attention.
#' @param robust_cutoff Fraction at or above which a concept is called robust
#'   (matches [attention_gap_sensitivity()]); drawn as a dashed line.
#' @return A ggplot object.
#' @export
plot_attention_gap_sensitivity <- function(sensitivity, label_table = NULL, label_col = "common_name",
                                           show_zero = TRUE, robust_cutoff = 0.5) {
  .cc_plot_require_ggplot2()
  dat <- as.data.frame(sensitivity, stringsAsFactors = FALSE)
  .cc_required_cols(dat, c("concept_id", "high_threat_low_attention_fraction"), "sensitivity")
  dat <- .cc_label_table(dat, label_table, label_col)
  dat$label <- .cc_display_label(dat, label_col)
  if (!isTRUE(show_zero)) dat <- dat[dat$high_threat_low_attention_fraction > 0, , drop = FALSE]
  dat <- dat[order(dat$high_threat_low_attention_fraction, decreasing = TRUE), , drop = FALSE]
  dat$robustness <- factor(ifelse(dat$high_threat_low_attention_fraction >= robust_cutoff, "Robust", "Threshold-dependent"),
                           levels = c("Robust", "Threshold-dependent"))
  ggplot2::ggplot(dat, ggplot2::aes(x = stats::reorder(label, high_threat_low_attention_fraction), y = high_threat_low_attention_fraction)) +
    ggplot2::geom_col(ggplot2::aes(fill = robustness), width = 0.75) +
    ggplot2::geom_hline(yintercept = robust_cutoff, linetype = 2, colour = "grey35") +
    ggplot2::scale_fill_manual(values = c(Robust = "#D55E00", `Threshold-dependent` = "#F0B48C"), drop = FALSE) +
    ggplot2::coord_flip(ylim = c(0, 1)) +
    ggplot2::labs(x = NULL, y = "Fraction of threshold scenarios classed high threat, low attention", fill = NULL) +
    .cc_plot_theme()
}

#' Plot seasonality anomalies
#' @param seasonality Output from [decompose_seasonality()].
#' @param label_table Optional label table.
#' @param label_col Label column.
#' @return A ggplot object.
#' @export
plot_seasonality <- function(seasonality, label_table = NULL, label_col = "common_name") {
  .cc_plot_require_ggplot2()
  dat <- as.data.frame(seasonality, stringsAsFactors = FALSE)
  .cc_required_cols(dat, c("month", "concept_id", "seasonal_anomaly"), "seasonality")
  dat <- .cc_label_table(dat, label_table, label_col)
  dat$label <- .cc_display_label(dat, label_col)
  ggplot2::ggplot(dat, ggplot2::aes(x = factor(month), y = seasonal_anomaly, group = label)) +
    ggplot2::geom_line(alpha = 0.70) +
    ggplot2::facet_wrap(~label, scales = "free_y") +
    ggplot2::labs(x = "Month", y = "Seasonal anomaly") +
    ggplot2::theme_minimal()
}

#' Plot burst counts by concept
#' @param bursts Output from [detect_bursts()].
#' @param label_table Optional concept label table.
#' @param label_col Label column.
#' @return A ggplot object.
#' @export
plot_burst_counts <- function(bursts, label_table = NULL, label_col = "common_name") {
  .cc_plot_require_ggplot2()
  dat <- as.data.frame(bursts, stringsAsFactors = FALSE)
  .cc_required_cols(dat, c("concept_id", "is_burst"), "bursts")
  dat <- .cc_label_table(dat, label_table, label_col)
  dat$label <- .cc_display_label(dat, label_col)
  dat$is_burst <- dat$is_burst %in% TRUE
  tab <- stats::aggregate(as.integer(dat$is_burst), dat["label"], sum, na.rm = TRUE)
  names(tab)[2] <- "n_bursts"
  tab <- tab[order(tab$n_bursts, decreasing = TRUE), , drop = FALSE]
  ggplot2::ggplot(tab, ggplot2::aes(x = stats::reorder(label, n_bursts), y = n_bursts)) +
    ggplot2::geom_col() + ggplot2::coord_flip() +
    ggplot2::labs(x = "Concept", y = "Detected burst days") + ggplot2::theme_minimal()
}

#' Plot detected attention bursts
#' @param bursts Output from [detect_bursts()].
#' @param label_table Optional concept labels.
#' @param label_col Label column.
#' @param time_unit Date aggregation unit.
#' @return A ggplot object.
#' @export
plot_bursts <- function(bursts, label_table = NULL, label_col = "common_name", time_unit = c("day", "week", "month")) {
  .cc_plot_require_ggplot2()
  time_unit <- match.arg(time_unit)
  dat <- as.data.frame(bursts, stringsAsFactors = FALSE)
  .cc_required_cols(dat, c("date", "concept_id", "attention_index", "is_burst"), "bursts")
  dat$date <- .cc_floor_date(dat$date, time_unit)
  dat <- .cc_label_table(dat, label_table, label_col)
  dat$label <- .cc_display_label(dat, label_col)
  dat$is_burst <- dat$is_burst %in% TRUE
  agg <- stats::aggregate(attention_index ~ date + concept_id + label, data = dat, FUN = mean, na.rm = TRUE)
  burst_dates <- unique(dat[dat$is_burst, c("date", "concept_id", "label"), drop = FALSE])
  burst_values <- merge(burst_dates, agg, by = c("date", "concept_id", "label"), all.x = TRUE)
  ggplot2::ggplot(agg, ggplot2::aes(x = date, y = attention_index)) +
    ggplot2::geom_line(alpha = 0.50, linewidth = 0.4) +
    ggplot2::geom_point(data = burst_values, ggplot2::aes(x = date, y = attention_index), size = 1.6) +
    ggplot2::scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m") +
    ggplot2::facet_wrap(~label, scales = "free_y") +
    ggplot2::labs(x = "Date", y = "Attention index") + ggplot2::theme_minimal()
}

#' Plot campaign counterfactual attention
#' @param effect Output from [campaign_effect()].
#' @return A ggplot object.
#' @export
plot_counterfactual_attention <- function(effect) {
  .cc_plot_require_ggplot2()
  dat <- as.data.frame(effect$daily_effects, stringsAsFactors = FALSE)
  if (nrow(dat) == 0) {
    return(ggplot2::ggplot(data.frame(x = 0, y = 0), ggplot2::aes(x = x, y = y)) +
             ggplot2::geom_text(ggplot2::aes(label = "No daily campaign effects available"), size = 4) +
             ggplot2::theme_void())
  }
  if (!"observed" %in% names(dat) && "treated" %in% names(dat)) dat$observed <- dat$treated
  if (!"counterfactual" %in% names(dat) && "predicted" %in% names(dat)) dat$counterfactual <- dat$predicted
  .cc_required_cols(dat, c("date", "observed", "counterfactual"), "effect$daily_effects")
  dat$date <- .cc_as_date(dat$date)
  event <- .cc_as_date(effect$campaign_date)
  long <- rbind(data.frame(date = dat$date, value = dat$observed, series = "Observed"),
                data.frame(date = dat$date, value = dat$counterfactual, series = "Counterfactual"))
  long$series <- factor(long$series, levels = c("Observed", "Counterfactual"))
  ggplot2::ggplot(long, ggplot2::aes(x = date, y = value, colour = series, linetype = series)) +
    ggplot2::annotate("rect", xmin = event, xmax = max(dat$date), ymin = -Inf, ymax = Inf, fill = "grey85", alpha = 0.5) +
    ggplot2::geom_vline(xintercept = event, linetype = 2, colour = "grey30") +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::scale_colour_manual(values = c(Observed = "#D55E00", Counterfactual = "grey35")) +
    ggplot2::scale_linetype_manual(values = c(Observed = 1, Counterfactual = 2)) +
    ggplot2::scale_x_date(labels = .cc_date_label) +
    ggplot2::labs(x = NULL, y = "Attention index", colour = NULL, linetype = NULL) +
    .cc_plot_theme()
}

#' Plot campaign effect sizes
#' @param effect Output from [campaign_effect()].
#' @param standardised Plot standardised effect when available.
#' @return A ggplot object.
#' @export
plot_campaign_effect_size <- function(effect, standardised = TRUE) {
  .cc_plot_require_ggplot2()
  dat <- as.data.frame(effect$daily_effects, stringsAsFactors = FALSE)
  metric <- if (isTRUE(standardised) && "standardised_effect" %in% names(dat)) "standardised_effect" else "effect"
  dat$date <- .cc_as_date(dat$date)
  dat$.metric <- as.numeric(dat[[metric]])
  event <- .cc_as_date(effect$campaign_date)
  dat$period <- factor(ifelse(dat$date < event, "Pre-event", "Post-event"), levels = c("Pre-event", "Post-event"))
  ylab <- if (metric == "standardised_effect") "Deviation from counterfactual (pre-event SD units)" else "Deviation from counterfactual"
  ggplot2::ggplot(dat, ggplot2::aes(x = date, y = .metric, fill = period)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey35") +
    ggplot2::geom_col(width = 0.85) +
    ggplot2::geom_vline(xintercept = event - 0.5, linetype = 2, colour = "grey30") +
    ggplot2::scale_fill_manual(values = c(`Pre-event` = "grey70", `Post-event` = "#D55E00")) +
    ggplot2::scale_x_date(labels = .cc_date_label) +
    ggplot2::labs(x = NULL, y = ylab, fill = NULL) +
    .cc_plot_theme()
}

#' Plot campaign placebo distribution
#' @param effect Output from [campaign_effect()].
#' @param metric Placebo metric to plot.
#' @return A ggplot object.
#' @export
plot_campaign_placebo_distribution <- function(effect, metric = "average_post_effect") {
  .cc_plot_require_ggplot2()
  if (is.null(effect$placebo_effects)) {
    return(ggplot2::ggplot(data.frame(x = 0, y = 0), ggplot2::aes(x = x, y = y)) +
             ggplot2::geom_text(ggplot2::aes(label = "No placebo effects available"), size = 4) + ggplot2::theme_void())
  }
  dat <- as.data.frame(effect$placebo_effects, stringsAsFactors = FALSE)
  if (!metric %in% names(dat)) metric <- names(dat)[vapply(dat, is.numeric, logical(1))][1]
  dat$.metric <- as.numeric(dat[[metric]])
  dat <- dat[is.finite(dat$.metric), , drop = FALSE]
  obs <- effect[[metric]]
  if (is.null(obs) || length(obs) == 0 || !is.finite(obs)) obs <- NA_real_
  if (nrow(dat) == 0) {
    return(ggplot2::ggplot(data.frame(x = 0, y = 0), ggplot2::aes(x = x, y = y)) +
             ggplot2::geom_text(ggplot2::aes(label = "No finite placebo effects"), size = 4) + ggplot2::theme_void())
  }
  xlab <- switch(metric,
    average_post_effect = "Average post-event deviation",
    cumulative_post_effect = "Cumulative post-event deviation",
    standardised_average_post_effect = "Standardised average post-event deviation",
    metric)
  sub <- if (!is.null(effect$placebo_p_value) && is.finite(effect$placebo_p_value)) {
    alt <- if (is.null(effect$placebo_alternative)) "two.sided" else effect$placebo_alternative
    alt_label <- c(two.sided = "two-sided", greater = "one-sided (greater)", less = "one-sided (less)")[[alt]]
    sprintf("Observed (dashed) vs %d placebo dates; %s placebo p = %.3f", nrow(dat), alt_label, effect$placebo_p_value)
  } else NULL
  ## A dot per placebo date shows the reference distribution without binning
  ## artefacts at small placebo counts.
  ggplot2::ggplot(dat, ggplot2::aes(x = .metric)) +
    ggplot2::geom_dotplot(binwidth = diff(range(c(dat$.metric, obs), na.rm = TRUE)) / 30 + 1e-9,
                          fill = "grey60", colour = "grey40", method = "histodot") +
    ggplot2::geom_vline(xintercept = obs, linetype = 2, linewidth = 0.9, colour = "#D55E00") +
    ggplot2::scale_y_continuous(NULL, breaks = NULL) +
    ggplot2::labs(x = xlab, subtitle = sub) +
    .cc_plot_theme()
}

#' Plot narrative frame counts
#' @param frames Output from [narrative_frame()].
#' @param label_table Optional concept label table.
#' @param label_col Label column.
#' @param collapse_groups Collapse detailed frames to broader communication-frame groups.
#' @return A ggplot object.
#' @export
plot_narrative_frames <- function(frames, label_table = NULL, label_col = "common_name", collapse_groups = FALSE) {
  .cc_plot_require_ggplot2()
  dat <- as.data.frame(frames, stringsAsFactors = FALSE)
  .cc_required_cols(dat, c("concept_id", "dominant_frame"), "frames")
  dat$dominant_frame[is.na(dat$dominant_frame) | !nzchar(dat$dominant_frame)] <- "unclassified"
  if (isTRUE(collapse_groups)) dat$dominant_frame <- .cc_collapse_frame(dat$dominant_frame)
  dat <- .cc_label_table(dat, label_table, label_col)
  dat$label <- .cc_display_label(dat, label_col)
  tab <- as.data.frame(table(dat$label, dat$dominant_frame), stringsAsFactors = FALSE)
  names(tab) <- c("label", "frame", "n")
  tab <- tab[tab$n > 0, , drop = FALSE]
  ggplot2::ggplot(tab, ggplot2::aes(x = stats::reorder(label, n), y = n, fill = frame)) +
    ggplot2::geom_col() + ggplot2::coord_flip() +
    ggplot2::labs(x = "Concept", y = "Text count", fill = "Frame") + ggplot2::theme_minimal()
}

#' Plot sentiment by concept
#' @param sentiment Output from [sentiment_by_concept()].
#' @param label_table Optional concept label table.
#' @param label_col Label column.
#' @param metric Sentiment metric to plot.
#' @return A ggplot object.
#' @export
plot_sentiment <- function(sentiment, label_table = NULL, label_col = "common_name", metric = NULL) {
  .cc_plot_require_ggplot2()
  dat <- as.data.frame(sentiment, stringsAsFactors = FALSE)
  if (is.null(metric)) metric <- if ("sentiment_index" %in% names(dat)) "sentiment_index" else "sentiment_score"
  .cc_required_cols(dat, c("concept_id", metric), "sentiment")
  dat <- .cc_label_table(dat, label_table, label_col)
  dat$label <- .cc_display_label(dat, label_col)
  dat$.metric <- as.numeric(dat[[metric]])
  dat <- dat[order(dat$.metric, decreasing = TRUE), , drop = FALSE]
  ggplot2::ggplot(dat, ggplot2::aes(x = stats::reorder(label, .metric), y = .metric)) +
    ggplot2::geom_hline(yintercept = 0, linetype = 3) +
    ggplot2::geom_col() + ggplot2::coord_flip() +
    ggplot2::labs(x = "Concept", y = metric) + ggplot2::theme_minimal()
}

#' Plot a simple culturomic dashboard
#' @param x Data frame or package output object.
#' @return A ggplot object when possible.
#' @export
plot_culturomic_dashboard <- function(x) {
  dat <- as.data.frame(x, stringsAsFactors = FALSE)
  if (all(c("attention_percentile", "threat_percentile") %in% names(dat))) return(plot_attention_gap(dat))
  if (all(c("date", "attention_index", "concept_id") %in% names(dat))) return(plot_attention_timeseries(dat))
  stop("No dashboard plot is available for this object.", call. = FALSE)
}

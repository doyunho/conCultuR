# CRAN check helpers for non-standard evaluation used in ggplot2 calls
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    ".metric", ".plot_label", ".plot_metric", ".plot_value",
    "ambiguity_class", "ambiguity_score", "attention_gap_class",
    "attention_gap_score", "attention_percentile", "concept_id",
    "counterfactual", "frame", "high_threat_low_attention_fraction",
    "label", "month", "n", "n_bursts", "observed",
    "platform_disagreement", "query_type", "seasonal_anomaly",
    "threat_percentile", "x", "y"
  ))
}

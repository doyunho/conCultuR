############################################################
## Example 1 (conCultuR 0.9.0)
## Step 3: how key options change the result (runs offline)
##
## Repeats the analysis of 02_analysis.R while changing one option at a
## time from its default, and records the attention-gap classes under
## each setting. Used for Section 3 and Table 3 of the manuscript.
## Run from the repository root:  Rscript paper/example1/03_options.R
############################################################

library(conCultuR)
stopifnot(packageVersion("conCultuR") >= "0.9.0")

out_dir <- Sys.getenv("EX1_OUT", "paper/example1/outputs")
targets <- read.csv(file.path(out_dir, "00_targets.csv"), stringsAsFactors = FALSE)
dict <- read.csv(file.path(out_dir, "01_concept_dictionary.csv"), stringsAsFactors = FALSE)
raw <- read.csv(file.path(out_dir, "05_raw_traces.csv"), stringsAsFactors = FALSE)
raw$date <- as.Date(raw$date)
status <- targets[c("concept_id", "status")]
labels <- setNames(targets$common_name, targets$concept_id)

## Query audit threshold -------------------------------------------------------------
audit_tab <- do.call(rbind, lapply(c(0.6, 0.45), function(m) {
  a <- audit_queries(dict, max_ambiguity = m)
  data.frame(max_ambiguity = m, n_flagged = sum(a$query_status == "needs_disambiguation"),
             flagged = paste(unique(a$query[a$query_status == "needs_disambiguation"]), collapse = "; "))
}))
write.csv(audit_tab, file.path(out_dir, "14_option_audit.csv"), row.names = FALSE)
print(audit_tab)

## Index and gap options -------------------------------------------------------------
run <- function(label, keep = rep(TRUE, nrow(raw)), std_method = "robust_zscore",
                index_args = list(), gap_args = list()) {
  std <- standardise_attention(as_culturomic_tbl(raw[keep, ]), method = std_method, transform = "log1p")
  idx <- do.call(attention_index, c(list(std, min_platforms = 1), index_args))
  gap <- do.call(attention_gap, c(list(idx, status), gap_args))
  htla <- gap$concept_id[gap$attention_gap_class == "high_threat_low_attention"]
  htla <- htla[order(-gap$attention_gap_score[match(htla, gap$concept_id)])]
  data.frame(setting = label,
             n_high_threat_low_attention = length(htla),
             n_low_threat_high_attention = sum(gap$attention_gap_class == "low_threat_high_attention"),
             high_threat_low_attention = paste(labels[htla], collapse = "; "),
             tiger_percentile = round(gap$attention_percentile[gap$concept_id == "tiger"], 3))
}
p <- raw$platform
res <- rbind(
  run("defaults"),
  run("use_ambiguity = FALSE", index_args = list(use_ambiguity = FALSE)),
  run("aggregate = \"flat\"", index_args = list(aggregate = "flat")),
  run("method = \"zscore\"", std_method = "zscore"),
  run("without Wikipedia", keep = p != "wikipedia"),
  run("without GDELT", keep = p != "gdelt"),
  run("without Google Trends", keep = p != "google_trends"),
  run("high_threat = 0.3 (CR and EN)", gap_args = list(high_threat = 0.3)),
  run("percentile_method = \"rank_n\"", gap_args = list(percentile_method = "rank_n"))
)
write.csv(res, file.path(out_dir, "15_option_comparison.csv"), row.names = FALSE)
print(res)

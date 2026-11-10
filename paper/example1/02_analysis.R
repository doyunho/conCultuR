############################################################
## Example 1 (conCultuR 0.9.0)
## Step 2: analysis from archived raw traces (runs offline)
##
## Reads outputs/05_raw_traces.csv written by 01_collect_live.R and
## reproduces Boxes 3-5 of the manuscript, Table 2, Figure 2 and
## Figure S1 using conCultuR functions only.
## Run from the repository root:  Rscript paper/example1/02_analysis.R
############################################################

library(conCultuR)
library(ggplot2)
library(patchwork)
stopifnot(packageVersion("conCultuR") >= "0.9.0")

out_dir <- Sys.getenv("EX1_OUT", "paper/example1/outputs")
fig_dir <- Sys.getenv("EX1_FIG", "paper/example1/figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
save_csv <- function(x, name) write.csv(as.data.frame(x), file.path(out_dir, name), row.names = FALSE)

targets <- read.csv(file.path(out_dir, "00_targets.csv"), stringsAsFactors = FALSE)
dict <- read.csv(file.path(out_dir, "01_concept_dictionary.csv"), stringsAsFactors = FALSE)
audit <- read.csv(file.path(out_dir, "02_query_audit.csv"), stringsAsFactors = FALSE)
raw <- read.csv(file.path(out_dir, "05_raw_traces.csv"), stringsAsFactors = FALSE)
raw$date <- as.Date(raw$date)
traces <- as_culturomic_tbl(raw)
labels <- targets[c("concept_id", "common_name")]

## Coverage --------------------------------------------------------------------
coverage <- platform_coverage(traces)
coverage_daily <- platform_coverage(traces, level = "daily")
save_csv(coverage, "06_platform_coverage.csv")
save_csv(coverage_daily, "06b_platform_coverage_daily.csv")
save_csv(as.data.frame(table(platform = raw$platform)), "06c_platform_row_counts.csv")

## Box 3. Standardisation, attention index, query sensitivity -----------------
traces_std <- standardise_attention(traces, method = "robust_zscore", transform = "log1p")
idx <- attention_index(traces_std, min_platforms = 1)
qs <- query_sensitivity(traces_std)
save_csv(traces_std, "07_standardised_traces.csv")
save_csv(idx, "08_attention_index.csv")
save_csv(as.data.frame(table(n_platforms = idx$n_platforms)), "08b_index_platform_counts.csv")
save_csv(qs, "09_query_sensitivity.csv")

disagreement <- aggregate(platform_disagreement ~ concept_id, idx, mean)
disagreement <- merge(disagreement, labels, by = "concept_id")
save_csv(disagreement[order(-disagreement$platform_disagreement), ], "09b_mean_platform_disagreement.csv")

## Box 4. Attention gap and threshold sensitivity -----------------------------
status <- targets[c("concept_id", "status")]
gap <- attention_gap(idx, status)
sens <- attention_gap_sensitivity(idx, status)
save_csv(merge(gap, labels, by = "concept_id"), "10_attention_gap.csv")
save_csv(merge(sens, labels, by = "concept_id"), "11_attention_gap_sensitivity.csv")

htla <- merge(gap[gap$attention_gap_class == "high_threat_low_attention",
                  c("concept_id", "status", "attention_percentile", "attention_gap_score")],
              sens[c("concept_id", "high_threat_low_attention_fraction", "robust_high_threat_low_attention")],
              by = "concept_id")
htla <- merge(labels, htla, by = "concept_id")
htla <- htla[order(-htla$attention_gap_score), ]
save_csv(htla, "12_table2_high_threat_low_attention.csv")

## Box 5. Report ---------------------------------------------------------------
write_culturomic_report(gap, file = file.path(out_dir, "13_attention_gap_report.md"),
                        title = "Example 1: attention gaps in threatened species")

## Figure 2 (panels a-d) and Figure S1 -----------------------------------------
fig2 <- (plot_query_ambiguity(audit) + plot_attention_gap(gap, label_table = labels)) /
  (plot_attention_gap_rank(gap, label_table = labels) + plot_attention_gap_sensitivity(sens, label_table = labels, show_zero = FALSE)) +
  plot_annotation(tag_levels = "a")
ggsave(file.path(fig_dir, "Figure2_attention_gap_example1.png"), fig2, width = 12, height = 10, dpi = 300)
ggsave(file.path(fig_dir, "Figure2_attention_gap_example1.pdf"), fig2, width = 12, height = 10)

figS1 <- plot_platform_disagreement(idx, label_table = labels) +
  plot_query_sensitivity(qs, label_table = labels, metric = "query_sensitivity_index") +
  plot_annotation(tag_levels = "a")
ggsave(file.path(fig_dir, "FigureS1_disagreement_query_sensitivity.png"), figS1, width = 12, height = 6, dpi = 300)
ggsave(file.path(fig_dir, "FigureS1_disagreement_query_sensitivity.pdf"), figS1, width = 12, height = 6)

writeLines(capture.output(sessionInfo()), file.path(out_dir, "sessionInfo.txt"))

print(table(gap$attention_gap_class))
print(htla)

############################################################
## Example 2 (conCultuR 0.9.0)
## Step 2: event diagnostic from archived raw traces (runs offline)
##
## Treated concept: Convention on Biological Diversity. Controls: seven
## environmental issues.
## Event: Leaders' Summit of the UN Biodiversity Conference (CBD COP17,
## Yerevan, 16 October 2026), which opened the conference (19-30 October)
## and drew the largest attention peak; dating the event at the opening
## day would place that peak inside the pre-event window.
## Windows: 14 days before and 14 days after (inclusive of the event
## day), so that placebo dates outside the event window remain within
## the GDELT DOC API's three-month coverage.
## Run from the repository root:  Rscript paper/example2/02_analysis.R
############################################################

library(conCultuR)
library(ggplot2)
library(patchwork)
stopifnot(packageVersion("conCultuR") >= "0.9.0")

out_dir <- Sys.getenv("EX2_OUT", "paper/example2/outputs")
fig_dir <- Sys.getenv("EX2_FIG", "paper/example2/figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
save_csv <- function(x, name) write.csv(as.data.frame(x), file.path(out_dir, name), row.names = FALSE)

event_date <- as.Date("2026-10-16")
event_label <- "CBD COP17 Leaders' Summit"
treated <- "biodiversity_convention"
pre_days <- 14
post_days <- 14
## Directional hypothesis: the event increases attention to the treated
## concept, so the placebo test is one-sided. The concept and date were
## chosen after inspecting the data, so the two-sided p-value is reported
## alongside.
alternative <- "greater"

targets <- read.csv(file.path(out_dir, "00_targets.csv"), stringsAsFactors = FALSE)
audit <- read.csv(file.path(out_dir, "02_query_audit.csv"), stringsAsFactors = FALSE)
raw <- read.csv(file.path(out_dir, "05_raw_traces.csv"), stringsAsFactors = FALSE)
raw$date <- as.Date(raw$date)
traces <- as_culturomic_tbl(raw)
labels <- targets[c("concept_id", "common_name")]
controls <- setdiff(targets$concept_id, treated)

## Coverage, standardisation, attention index ---------------------------------
save_csv(platform_coverage(traces), "06_platform_coverage.csv")
save_csv(as.data.frame(table(platform = raw$platform)), "06c_platform_row_counts.csv")
traces_std <- standardise_attention(traces, method = "robust_zscore", transform = "log1p")
idx <- attention_index(traces_std, min_platforms = 1)
save_csv(traces_std, "07_standardised_traces.csv")
save_csv(idx, "08_attention_index.csv")
save_csv(as.data.frame(table(n_platforms = idx$n_platforms)), "08b_index_platform_counts.csv")
save_csv(query_sensitivity(traces_std), "09_query_sensitivity.csv")

## Event diagnostic --------------------------------------------------------------
warn_log <- character()
ce <- withCallingHandlers(
  campaign_effect(idx, campaign_date = event_date, treated_concepts = treated,
                  control_concepts = controls, pre_days = pre_days, post_days = post_days,
                  placebo_n = 100, placebo_seed = 1, alternative = alternative),
  warning = function(w) { warn_log <<- c(warn_log, conditionMessage(w)); invokeRestart("muffleWarning") }
)
summary_tab <- data.frame(
  treated_concept = treated, event_label = event_label, event_date = as.character(event_date),
  pre_days = pre_days, post_days = post_days,
  n_pre_days = sum(ce$daily_effects$period == "pre"), n_post_days = sum(ce$daily_effects$period == "post"),
  pre_fit_r2 = summary(ce$model)$r.squared, pre_rmse = ce$pre_rmse,
  average_post_effect = ce$average_post_effect, cumulative_post_effect = ce$cumulative_post_effect,
  standardised_average_post_effect = ce$standardised_average_post_effect,
  placebo_n_used = ce$placebo_n_used, placebo_effect_mean = ce$placebo_effect_mean,
  placebo_effect_sd = ce$placebo_effect_sd, placebo_alternative = ce$placebo_alternative,
  placebo_p_value = ce$placebo_p_value,
  placebo_p_value_two_sided = (1 + sum(abs(ce$placebo_effects$average_post_effect) >= abs(ce$average_post_effect), na.rm = TRUE)) /
    (1 + ce$placebo_n_used),
  min_attainable_p = ce$min_attainable_p
)
save_csv(summary_tab, "10_event_effect_summary.csv")
save_csv(ce$daily_effects, "11_event_daily_effects.csv")
save_csv(ce$placebo_effects, "12_event_placebo_effects.csv")
writeLines(if (length(warn_log)) warn_log else "no warnings", file.path(out_dir, "12b_event_warnings.txt"))

## Figures -------------------------------------------------------------------------
figS2 <- (plot_query_ambiguity(audit) + plot_counterfactual_attention(ce)) /
  (plot_campaign_effect_size(ce) + plot_campaign_placebo_distribution(ce)) +
  plot_annotation(tag_levels = "a")
ggsave(file.path(fig_dir, "FigureS2_event_diagnostic_example2.png"), figS2, width = 12, height = 9, dpi = 300)
ggsave(file.path(fig_dir, "FigureS2_event_diagnostic_example2.pdf"), figS2, width = 12, height = 9)

ts_plot <- plot_attention_timeseries(idx, label_table = labels, facet = TRUE) +
  geom_vline(xintercept = event_date, linetype = 2, colour = "#D55E00")
ggsave(file.path(fig_dir, "FigureS3_issue_timeseries_example2.png"), ts_plot, width = 11, height = 7, dpi = 300)

writeLines(capture.output(sessionInfo()), file.path(out_dir, "sessionInfo.txt"))
print(summary_tab)
print(warn_log)

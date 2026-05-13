############################################################
## conCultuR real-data analysis template
## Wikimedia, GDELT and Google Trends live-data workflow
############################################################

if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("gtrendsR", quietly = TRUE)) install.packages("gtrendsR")

library(conCultuR)
library(ggplot2)

set.seed(1)
cat("conCultuR version\n")
print(packageVersion("conCultuR"))

out_dir <- "conCultuR_real_analysis_output"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

targets <- data.frame(
  concept_id = c("vaquita", "saola", "black_rhino", "hawksbill_turtle", "giant_panda", "tiger"),
  scientific_name = c("Phocoena sinus", "Pseudoryx nghetinhensis", "Diceros bicornis", "Eretmochelys imbricata", "Ailuropoda melanoleuca", "Panthera tigris"),
  common_name = c("vaquita", "saola", "black rhino", "hawksbill turtle", "giant panda", "tiger"),
  wikipedia_title = c("Vaquita", "Saola", "Black rhinoceros", "Hawksbill sea turtle", "Giant panda", "Tiger"),
  gdelt_query = c('"vaquita"', '"saola"', '("black rhino" OR "black rhinoceros")', '("hawksbill turtle" OR "hawksbill sea turtle")', '"giant panda"', '"tiger conservation"'),
  google_query = c("vaquita", "saola", "black rhino", "hawksbill turtle", "giant panda", "tiger conservation"),
  status = c("CR", "CR", "CR", "CR", "VU", "EN"),
  stringsAsFactors = FALSE
)

date_from <- Sys.Date() - 75
date_to <- Sys.Date() - 3
google_time_window <- "today 1-m"

make_live_dictionary <- function(targets) {
  wiki <- data.frame(
    concept_id = targets$concept_id,
    scientific_name = targets$scientific_name,
    common_name = targets$common_name,
    query = targets$wikipedia_title,
    query_type = "wikipedia_title",
    language = "en",
    country = "global",
    platform = "wikipedia",
    stringsAsFactors = FALSE
  )
  gdelt <- data.frame(
    concept_id = targets$concept_id,
    scientific_name = targets$scientific_name,
    common_name = targets$common_name,
    query = targets$gdelt_query,
    query_type = "gdelt_query",
    language = "en",
    country = "global",
    platform = "gdelt",
    stringsAsFactors = FALSE
  )
  google <- data.frame(
    concept_id = targets$concept_id,
    scientific_name = targets$scientific_name,
    common_name = targets$common_name,
    query = targets$google_query,
    query_type = "common",
    language = "en",
    country = "global",
    platform = "google_trends",
    stringsAsFactors = FALSE
  )
  dict <- rbind(wiki, gdelt, google)
  dict <- score_ambiguity(dict)
  class(dict) <- c("culturomic_dictionary", class(dict))
  dict
}

save_csv <- function(x, file) {
  if (is.null(x)) return(invisible(FALSE))
  write.csv(as.data.frame(x), file.path(out_dir, file), row.names = FALSE)
  invisible(TRUE)
}

save_text <- function(x, file) {
  if (is.null(x)) return(invisible(FALSE))
  writeLines(as.character(x), con = file.path(out_dir, file))
  invisible(TRUE)
}

plot_and_save <- function(plot_obj, filename, width = 8, height = 5, dpi = 300) {
  if (is.null(plot_obj)) return(invisible(FALSE))
  ok <- tryCatch({
    ggplot2::ggsave(file.path(out_dir, filename), plot = plot_obj, width = width, height = height, dpi = dpi)
    TRUE
  }, error = function(e) {
    message("Plot failed: ", filename)
    message(conditionMessage(e))
    FALSE
  })
  invisible(ok)
}

call_plot <- function(fun, x, ...) {
  f <- match.fun(fun)
  extra <- list(...)
  fm <- names(formals(f))
  if (!("..." %in% fm)) extra <- extra[names(extra) %in% fm]
  tryCatch(do.call(f, c(list(x), extra)), error = function(e) {
    message("Plot function failed: ", deparse(substitute(fun)))
    message(conditionMessage(e))
    NULL
  })
}

run_campaign_effect <- function(idx, campaign_target, campaign_date) {
  f <- conCultuR::campaign_effect
  fm <- names(formals(f))
  args <- list()
  if ("attention" %in% fm) args$attention <- idx else if ("x" %in% fm) args$x <- idx else args[[fm[1]]] <- idx
  if ("treated_concepts" %in% fm) args$treated_concepts <- campaign_target
  if ("campaign_date" %in% fm) args$campaign_date <- campaign_date
  if ("pre_days" %in% fm) args$pre_days <- 21
  if ("post_days" %in% fm) args$post_days <- 21
  if ("placebo_n" %in% fm) args$placebo_n <- 30
  if ("n_placebo" %in% fm) args$n_placebo <- 30
  if ("placebo" %in% fm) args$placebo <- TRUE
  if ("placebo_seed" %in% fm) args$placebo_seed <- 1
  do.call(f, args)
}

dict <- make_live_dictionary(targets)
save_csv(dict, "01_dictionary.csv")
query_audit <- audit_queries(dict)
save_csv(query_audit, "02_query_audit.csv")

conservation_status <- data.frame(
  concept_id = targets$concept_id,
  scientific_name = targets$scientific_name,
  common_name = targets$common_name,
  status = targets$status,
  stringsAsFactors = FALSE
)
label_table <- data.frame(
  concept_id = targets$concept_id,
  label = targets$common_name,
  scientific_name = targets$scientific_name,
  stringsAsFactors = FALSE
)
save_csv(conservation_status, "03_conservation_status.csv")

deps <- check_live_dependencies(c("wikipedia", "gdelt", "google"))
save_csv(deps, "04_live_dependencies.csv")
print(deps)

gt_diag <- diagnose_google_trends("conservation", time = google_time_window, gprop = "web")
save_csv(gt_diag, "05_google_trends_diagnostic.csv")
print(gt_diag)

traces <- collect_attention(
  dict,
  platforms = c("wikipedia", "gdelt", "google"),
  from = date_from,
  to = date_to,
  mock = FALSE,
  continue_on_error = TRUE,
  max_queries_per_concept = 1,
  platform_args = list(
    gdelt = list(mode = "timelinevolraw", fill_missing_zero = TRUE),
    google_trends = list(batch = FALSE, fallback_single = TRUE, verbose = TRUE, sleep = 3, geo = "", time = google_time_window, gprop = "web")
  )
)
traces_df <- as.data.frame(traces)
if (nrow(traces_df) == 0) stop("No live data were collected. Check network access, platform availability, and query settings.")
save_csv(traces_df, "06_raw_traces.csv")
save_csv(as.data.frame(table(traces_df$platform)), "06b_platform_row_counts.csv")
save_csv(as.data.frame(table(traces_df$concept_id, traces_df$platform)), "06c_concept_platform_counts.csv")

coverage <- platform_coverage(traces)
if (!is.null(coverage$concept_coverage)) save_csv(coverage$concept_coverage, "07_platform_coverage.csv")
if (!is.null(coverage$daily_coverage)) save_csv(coverage$daily_coverage, "07b_daily_platform_coverage.csv")

traces_std <- standardise_attention(traces, method = "robust_zscore", transform = "log1p")
save_csv(traces_std, "08_standardised_traces.csv")
idx <- attention_index(traces_std, min_platforms = 1)
save_csv(idx, "09_attention_index.csv")
save_csv(as.data.frame(table(idx$n_platforms)), "09b_attention_index_platform_counts.csv")

gap <- attention_gap(idx, conservation_status)
gap <- merge(gap, label_table[, c("concept_id", "label")], by = "concept_id", all.x = TRUE)
save_csv(gap, "10_attention_gap.csv")
gap_sens <- attention_gap_sensitivity(idx, conservation_status)
if ("concept_id" %in% names(gap_sens)) gap_sens <- merge(gap_sens, label_table[, c("concept_id", "label")], by = "concept_id", all.x = TRUE)
save_csv(gap_sens, "11_attention_gap_sensitivity.csv")
save_text(culturomic_report(gap), "12_attention_gap_report.md")

qs <- query_sensitivity(traces_std)
save_csv(qs, "13_query_sensitivity.csv")
save_text(culturomic_report(qs), "14_query_sensitivity_report.md")

seasonality <- decompose_seasonality(idx)
save_csv(seasonality, "15_seasonality.csv")
bursts <- detect_bursts(idx)
save_csv(bursts, "16_attention_bursts.csv")

campaign_target <- targets$concept_id[1]
campaign_date <- Sys.Date() - 35
ce <- tryCatch(run_campaign_effect(idx, campaign_target, campaign_date), error = function(e) {
  message("Campaign diagnostic failed: ", conditionMessage(e))
  NULL
})
if (!is.null(ce)) {
  capture.output(print(ce), file = file.path(out_dir, "17_campaign_effect_summary.txt"))
  if (!is.null(ce$daily_effects)) save_csv(ce$daily_effects, "18_campaign_daily_effects.csv")
  if (!is.null(ce$placebo_effects)) save_csv(ce$placebo_effects, "19_campaign_placebo_effects.csv")
  save_text(culturomic_report(ce), "20_campaign_report.md")
}

plot_and_save(call_plot(plot_attention_timeseries, idx, time_unit = "week", label_table = label_table), "21_attention_timeseries_weekly.png", width = 10, height = 7)
plot_and_save(call_plot(plot_attention_heatmap, idx, label_table = label_table), "22_attention_heatmap.png", width = 9, height = 6)
plot_and_save(call_plot(plot_platform_disagreement, idx, label_table = label_table), "23_platform_disagreement.png")
plot_and_save(call_plot(plot_attention_gap, gap, label_table = label_table), "24_attention_gap_scatter.png", width = 8, height = 6)
plot_and_save(call_plot(plot_attention_gap_rank, gap, label_table = label_table), "25_attention_gap_rank.png")
plot_and_save(call_plot(plot_attention_gap_sensitivity, gap_sens, label_table = label_table), "26_attention_gap_sensitivity.png")
plot_and_save(call_plot(plot_query_sensitivity, qs, metric = "query_sensitivity_sd", label_table = label_table), "27_query_sensitivity_sd.png")
plot_and_save(call_plot(plot_query_sensitivity, qs, metric = "max_leave_one_query_influence", label_table = label_table), "28_query_influence_leave_one_out.png")
plot_and_save(call_plot(plot_burst_counts, bursts, label_table = label_table), "29_attention_burst_counts.png")
if (!is.null(ce)) {
  plot_and_save(call_plot(plot_counterfactual_attention, ce), "30_campaign_counterfactual.png")
  plot_and_save(call_plot(plot_campaign_effect_size, ce), "31_campaign_effect_size.png")
  plot_and_save(call_plot(plot_campaign_placebo_distribution, ce), "32_campaign_placebo_distribution.png")
}
plot_and_save(call_plot(plot_query_ambiguity, query_audit), "33_query_ambiguity.png")
plot_and_save(call_plot(plot_query_ambiguity_top, query_audit, top_n = 15), "34_high_ambiguity_queries.png", height = 6)

cat("\n\n=============================\n")
cat("conCultuR real analysis done\n")
cat("=============================\n\n")
cat("Output folder\n")
cat(out_dir, "\n\n")
cat("Rows collected by platform\n")
print(table(traces_df$platform))
cat("\nAttention gap classes\n")
if ("attention_gap_class" %in% names(gap)) print(table(gap$attention_gap_class))
cat("\nHigh threat low attention concepts\n")
if ("attention_gap_class" %in% names(gap)) print(gap[gap$attention_gap_class == "high_threat_low_attention", intersect(c("concept_id", "scientific_name", "label", "attention_gap_score"), names(gap))])
cat("\nNear-threshold candidates\n")
if ("near_high_threat_low_attention" %in% names(gap)) print(gap[gap$near_high_threat_low_attention, intersect(c("concept_id", "scientific_name", "label", "attention_gap_score"), names(gap))])
cat("\nCampaign effect summary\n")
if (!is.null(ce)) print(ce[intersect(c("campaign_date", "average_post_effect", "standardised_average_post_effect", "placebo_p_value"), names(ce))])
cat("\nGenerated files\n")
print(list.files(out_dir))

############################################################
## Example 2 (conCultuR 0.9.0)
## Step 3: query sensitivity of the event diagnostic (runs offline)
##
## The Google Trends query "COP17" also matches the UNCCD COP17
## (Ulaanbaatar, 17-28 August 2026), whose opening coincides with the
## largest Google peak for the treated concept and falls inside the
## placebo windows. This script repeats the diagnostic of 02_analysis.R
## (a) without the treated concept's Google Trends trace and
## (b) without Google Trends for all concepts.
## Run from the repository root:  Rscript paper/example2/03_sensitivity.R
############################################################

library(conCultuR)
stopifnot(packageVersion("conCultuR") >= "0.9.0")

out_dir <- Sys.getenv("EX2_OUT", "paper/example2/outputs")
treated <- "biodiversity_convention"
raw <- read.csv(file.path(out_dir, "05_raw_traces.csv"), stringsAsFactors = FALSE)
raw$date <- as.Date(raw$date)
controls <- setdiff(unique(raw$concept_id), treated)

run <- function(label, keep) {
  tr <- as_culturomic_tbl(raw[keep, ])
  idx <- attention_index(standardise_attention(tr, method = "robust_zscore", transform = "log1p"), min_platforms = 1)
  ce <- suppressWarnings(campaign_effect(idx, campaign_date = as.Date("2026-10-16"), treated_concepts = treated,
                                         control_concepts = controls, pre_days = 14, post_days = 14,
                                         placebo_n = 100, placebo_seed = 1, alternative = "greater"))
  data.frame(
    specification = label, pre_fit_r2 = summary(ce$model)$r.squared, pre_rmse = ce$pre_rmse,
    average_post_effect = ce$average_post_effect, standardised_average_post_effect = ce$standardised_average_post_effect,
    placebo_n_used = ce$placebo_n_used, placebo_p_value = ce$placebo_p_value,
    placebo_p_value_two_sided = (1 + sum(abs(ce$placebo_effects$average_post_effect) >= abs(ce$average_post_effect), na.rm = TRUE)) /
      (1 + ce$placebo_n_used)
  )
}
google <- raw$platform == "google_trends"
res <- rbind(
  run("all three platforms", rep(TRUE, nrow(raw))),
  run("treated concept without Google Trends", !(google & raw$concept_id == treated)),
  run("all concepts without Google Trends", !google)
)
write.csv(res, file.path(out_dir, "13_event_query_sensitivity.csv"), row.names = FALSE)
print(res)

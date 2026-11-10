############################################################
## Example 1 (conCultuR 0.9.0)
## Step 1: live collection for 1 Aug - 30 Oct 2026
##
## Re-collects Wikimedia Pageviews, GDELT and Google Trends for the
## manuscript window. Google Trends keywords are batched with an
## anchor keyword so that all species share one search-interest scale.
## Run from the repository root:  Rscript paper/example1/01_collect_live.R
############################################################

library(conCultuR)
stopifnot(packageVersion("conCultuR") >= "0.9.0")

out_dir <- Sys.getenv("EX1_OUT", "paper/example1/outputs")
## CC_PLATFORMS lets a run skip a platform that is unavailable (e.g. "wikipedia,google").
platforms <- strsplit(Sys.getenv("CC_PLATFORMS", "wikipedia,gdelt,google"), ",")[[1]]
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

date_from <- "2026-08-01"
date_to <- "2026-10-30"
anchor <- "endangered species"

## Box 1. Dictionary and query audit -----------------------------------------
targets <- read.csv("paper/example1/inputs/00_targets.csv", stringsAsFactors = FALSE)

mk <- function(q, type, platform) data.frame(
  targets[c("concept_id", "scientific_name", "common_name")],
  query = q, query_type = type, platform = platform,
  language = "en", country = "global", stringsAsFactors = FALSE)

dict <- rbind(mk(targets$wikipedia_title, "wikipedia_title", "wikipedia"),
              mk(targets$gdelt_query, "gdelt_query", "gdelt"),
              mk(targets$google_query, "common", "google_trends"))
dict <- score_ambiguity(dict)
audit <- audit_queries(dict)

write.csv(targets, file.path(out_dir, "00_targets.csv"), row.names = FALSE)
write.csv(dict, file.path(out_dir, "01_concept_dictionary.csv"), row.names = FALSE)
write.csv(audit, file.path(out_dir, "02_query_audit.csv"), row.names = FALSE)

## Box 2. Readiness checks and live collection --------------------------------
deps <- check_live_dependencies(platforms)
gt_diag <- diagnose_google_trends(anchor, time = paste(date_from, date_to))
write.csv(deps, file.path(out_dir, "03_live_dependencies.csv"), row.names = FALSE)
write.csv(gt_diag, file.path(out_dir, "04_google_trends_diagnostic.csv"), row.names = FALSE)

warn_log <- character()
traces <- withCallingHandlers(
  collect_attention(
    dict,
    platforms = platforms,
    from = date_from, to = date_to,
    max_queries_per_concept = 1,
    platform_args = list(
      gdelt = list(mode = "timelinevolraw", fill_missing_zero = TRUE, sleep = 10, retries = 5),
      google_trends = list(batch = TRUE, batch_size = 5, anchor_keyword = anchor,
                           fallback_single = TRUE, sleep = 5,
                           geo = "", gprop = "web", verbose = TRUE)
    )
  ),
  warning = function(w) {
    warn_log <<- c(warn_log, conditionMessage(w))
    invokeRestart("muffleWarning")
  }
)

write.csv(as.data.frame(traces), file.path(out_dir, "05_raw_traces.csv"), row.names = FALSE)
write.csv(attr(traces, "platform_collection_status"), file.path(out_dir, "05b_platform_collection_status.csv"), row.names = FALSE)
gdelt_status <- attr(traces, "platform_diagnostics")$gdelt
if (!is.null(gdelt_status)) write.csv(gdelt_status, file.path(out_dir, "05e_gdelt_query_status.csv"), row.names = FALSE)
writeLines(if (length(warn_log)) warn_log else "no warnings", file.path(out_dir, "05c_collection_warnings.txt"))
writeLines(c(
  paste("collected", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
  paste("date_from", date_from), paste("date_to", date_to),
  paste("google_anchor_keyword", anchor),
  paste("conCultuR_version", packageVersion("conCultuR")),
  paste("gtrendsR_version", packageVersion("gtrendsR"))
), file.path(out_dir, "05d_collection_metadata.txt"))

print(table(traces$platform))
print(warn_log)

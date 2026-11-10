# conCultuR

conCultuR is an R package for measuring digital attention to biodiversity across platforms. It builds platform-specific query dictionaries and scores their ambiguity, collects daily traces from Wikipedia page views, the GDELT DOC API and Google Trends, standardises and combines them into a concept-level attention index, and reports platform coverage, platform disagreement and query sensitivity with every estimate. Two analyses build on the index: attention gaps relative to a user-supplied reference scale (e.g. IUCN Red List category), with threshold sensitivity, and an event diagnostic with a counterfactual trajectory and placebo dates.

Outputs measure digital visibility in the chosen platforms and period. They do not measure conservation value, extinction risk or priority.

## Installation

```r
# install.packages("remotes")
remotes::install_github("doyunho/conCultuR")
```

R >= 4.1.0 is required. Google Trends collection needs `gtrendsR`, and the plotting functions need `ggplot2`.

## Offline example

```r
library(conCultuR)

ex <- con_culturomics_example_data()
dict <- score_ambiguity(ex$dictionary)
traces <- collect_attention(dict, platforms = c("wikipedia", "gdelt", "google"),
                            from = "2024-01-01", to = "2024-03-31", mock = TRUE)
idx <- attention_index(standardise_attention(traces), min_platforms = 1)
gap <- attention_gap(idx, ex$conservation_status)
gap
```

## Live collection

```r
check_live_dependencies(c("wikipedia", "gdelt", "google"))

traces <- collect_attention(
  dict,
  platforms = c("wikipedia", "gdelt", "google"),
  from = "2026-08-01", to = "2026-10-30",
  platform_args = list(
    gdelt = list(mode = "timelinevolraw", sleep = 10, retries = 5),
    google_trends = list(batch = TRUE, batch_size = 5,
                         anchor_keyword = "endangered species",
                         fallback_single = TRUE)))

table(attr(traces, "platform_diagnostics")$gdelt$status)
platform_coverage(traces)
```

The GDELT DOC API keeps about three months of timelines and Google Trends re-samples values on each request, so save the raw traces returned by `collect_attention()`. Every later step runs offline from them. A complete live workflow is in `inst/examples/conCultuR_real_analysis_template.R`, and the vignettes describe each step.

## Paper materials

The `paper/` folder holds the scripts, collected traces and outputs for the two worked examples in the accompanying manuscript. It is not part of the installed package.

## License

MIT

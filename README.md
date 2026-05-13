# conCultuR
[![R-CMD-check](https://github.com/doyunho/conCultuR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/doyunho/conCultuR/actions/workflows/R-CMD-check.yaml)
`conCultuR` is an R package for **conservation culturomics**, the use of digital traces to study public attention to biodiversity, species, habitats, conservation issues, and campaigns. The package is designed as a **measurement framework** rather than a single API wrapper.

The central purpose is to make biodiversity-attention analysis reproducible, auditable, and interpretable. In practice, this means that `conCultuR` does not only retrieve Google Trends, Wikimedia, or news data. It also records how queries were defined, scores query ambiguity, standardises platform-specific signals, estimates concept-level attention, evaluates platform disagreement, quantifies attention gaps relative to conservation status, and diagnoses campaign-associated changes.

## What the package measures

`conCultuR` estimates **digital attention signals** for user-defined biodiversity concepts. A concept can be a species, taxon group, protected area, habitat, campaign, invasive species issue, or other conservation topic. For each concept, the user defines platform-specific queries such as a Wikipedia title, Google Trends keyword, or GDELT news query.

The package produces the following main outputs.

| Output | Meaning |
|---|---|
| concept dictionary | auditable mapping between biological concepts and search queries |
| query ambiguity audit | identification of terms that may mix biological and non-biological meanings |
| standardised traces | platform-specific values transformed to comparable attention scores |
| attention index | concept-level attention after query weighting and platform aggregation |
| platform disagreement | extent to which platforms produce inconsistent attention signals |
| query sensitivity | extent to which conclusions depend on query choice |
| attention gap | mismatch between threat status and public attention |
| threshold sensitivity | robustness of attention-gap classification to classification thresholds |
| campaign diagnostic | counterfactual deviation after a campaign or media event |
| narrative frames | summary of how biodiversity concepts are discussed in text |

## What the package does not measure

The package does **not** estimate ecological value, extinction risk, abundance, population trend, or conservation priority. A high attention gap indicates low digital visibility relative to the chosen conservation-status scale in the selected data sources. It should be interpreted as a **communication or engagement signal**, not as a replacement for ecological assessment or conservation prioritisation.

## Installation from a local source archive

```r
remotes::install_local("conCultuR_0.8.6.zip", upgrade = "never", force = TRUE)
```

After installation, load the package.

```r
library(conCultuR)
packageVersion("conCultuR")
```

## Minimal offline example

This example uses simulated data and does not require internet access. It is appropriate for testing the package workflow.

```r
library(conCultuR)

ex <- con_culturomics_example_data()
dict <- score_ambiguity(ex$dictionary)

traces <- collect_attention(
  dict,
  platforms = c("google", "wikipedia", "reddit"),
  from = "2024-01-01",
  to = "2024-03-31",
  mock = TRUE
)

traces_std <- standardise_attention(traces)
idx <- attention_index(traces_std, min_platforms = 1)
gap <- attention_gap(idx, ex$conservation_status)

gap
```

## Live-data example

Live data collection is possible for Wikimedia Pageviews, GDELT DOC API timelines, and Google Trends. Google Trends uses `gtrendsR` and can be affected by rate limits, query volume, and network conditions. Always run the diagnostic first.

```r
library(conCultuR)

real <- con_culturomics_real_example_data()
check_live_dependencies(c("wikipedia", "gdelt", "google"))
diagnose_google_trends("conservation", time = "today 1-m", gprop = "web")

traces <- collect_attention(
  real$dictionary,
  platforms = c("wikipedia", "gdelt", "google"),
  from = Sys.Date() - 75,
  to = Sys.Date() - 3,
  mock = FALSE,
  continue_on_error = TRUE,
  max_queries_per_concept = 1,
  platform_args = list(
    gdelt = list(mode = "timelinevolraw", fill_missing_zero = TRUE),
    google_trends = list(
      batch = FALSE,
      fallback_single = TRUE,
      sleep = 3,
      geo = "",
      time = "today 1-m",
      gprop = "web"
    )
  )
)

coverage <- platform_coverage(traces)
idx <- attention_index(standardise_attention(traces), min_platforms = 1)
gap <- attention_gap(idx, real$conservation_status)

gap
```

A complete live-data template is included in the package.

```r
source(system.file("examples/conCultuR_real_analysis_template.R", package = "conCultuR"))
```

## Recommended reporting language

When reporting results, use language such as the following.

> We analysed digital attention to selected conservation concepts using `conCultuR`. Platform-specific traces were standardised before concept-level aggregation. Search queries were audited for ambiguity, attention indices were estimated across platforms, and attention gaps were interpreted as communication signals rather than ecological priority scores.

Avoid language that implies direct inference about ecological status from digital attention alone.

## Methodological contribution

The package contributes three linked elements to conservation culturomics.

1. **Query ambiguity audit**. Common names, hashtags, translations, and short names can mix biological and non-biological meanings. `conCultuR` scores and reports this measurement risk.
2. **Platform triangulation**. Attention is treated as a heterogeneous digital signal. The package reports cross-platform agreement and disagreement instead of assuming that a single platform captures public attention.
3. **Attention-gap inference**. Threat status and attention are compared on a transparent percentile scale, with sensitivity checks to avoid over-interpreting threshold-dependent results.

Campaign diagnostics and narrative-frame summaries are additional interpretation layers rather than the core methodological claim.

## Reproducibility notes

For manuscripts, save the following alongside the analysis.

- concept dictionary and query audit
- raw platform traces and collection dates
- standardised traces and attention indices
- platform coverage summaries
- attention-gap thresholds and sensitivity grid
- campaign date, control concepts, placebo settings
- platform terms and data-redistribution checks

## Package status

This bundle is intended as a publication-readiness source package. Before journal or CRAN submission, run the following in a local R environment.

```r
devtools::test()
devtools::check(args = "--as-cran")
```

# Live data guide

This guide explains practical use of live data sources in `conCultuR`.

## Wikimedia Pageviews

Use Wikipedia article titles, not arbitrary keyword strings. Article-title matching is often less ambiguous than common-name keyword search. Check that the article exists in the target language.

Recommended query type: `wikipedia_title`.

## GDELT

GDELT queries should be more restrictive than generic common names. For ambiguous common names, include context terms such as `conservation`, `species`, `wildlife`, or taxon-specific phrases.

Examples:

- preferred: `"tiger conservation"`
- risky: `"tiger"`

## Google Trends

Google Trends values are relative search indices, not absolute search counts. They are sensitive to time window, geography, query batching, and rate limits. Use `diagnose_google_trends()` before live collection.

Recommended settings for initial use:

```r
diagnose_google_trends("conservation", time = "today 1-m", gprop = "web")
```

If a large query set fails, use single-query fallback:

```r
get_google_attention(
  dictionary,
  mock = FALSE,
  batch = FALSE,
  fallback_single = TRUE,
  time = "today 1-m",
  gprop = "web",
  geo = ""
)
```

## Platform coverage

When platforms return different date ranges, use `platform_coverage()` and report `n_platforms` in the attention index. Avoid interpreting dates with only one available platform as equivalent to dates with full platform coverage.

## Archiving

For reproducible studies, save raw traces immediately after collection. API responses can change over time.

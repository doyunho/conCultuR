# conCultuR 0.9.0

## Changes to defaults

* `standardise_attention(method = "robust_zscore")`: when the median absolute deviation is 0, as with zero-inflated daily news counts, scaling now falls back to 1.2533 × the mean absolute deviation (the modified z-score convention) instead of setting the whole group to 0. Groups with any spread are unaffected.
* `collect_attention()` now defaults to `platforms = c("wikipedia", "gdelt", "google")`, the platform set used in the package documentation and worked examples (previously `c("google", "wikipedia", "reddit")`).
* `collect_attention()` gains `trim_dates = TRUE`, which drops rows outside `from`–`to`. Google Trends preset windows (e.g. `"today 3-m"`) can return dates beyond the requested range.

## Bug fixes

* `campaign_effect()`: placebo dates are now drawn without replacement. Previously, when fewer dates were eligible than `placebo_n`, dates were resampled with replacement, so the same placebo was counted several times and the p-value was reported with spurious precision (e.g. 1/101 from about 25 distinct dates). Placebo dates whose window overlaps the real post-event period are excluded by default (`placebo_exclude_event = TRUE`). The result reports `placebo_n_used` and `min_attainable_p`.
* GDELT can answer a throttled request with HTTP 200 and an empty object, which is indistinguishable from a query without matches. `get_gdelt_attention()` now asks once more after an empty response and warns when every query comes back empty. It also gains a `user_agent` argument.
* `get_google_attention()` replaced recent custom windows with a Google preset (`today 3-m`) even when the preset began after `from`, so the first days of the window were silently missing. Presets are now used only when they reach `from`.
* `campaign_effect()` gains `alternative = c("two.sided", "greater", "less")` for one-sided placebo tests of a direction stated in advance. The chosen direction is stored as `placebo_alternative` and shown by `print()` and `plot_campaign_placebo_distribution()`.
* New `gdelt_query_urls()` and `read_gdelt_json()`: when the GDELT DOC API refuses scripted requests but still answers a browser, the request URLs can be opened in a browser, the responses saved, and the saved files read back in the same form as `get_gdelt_attention()`. Files are matched to queries by the query GDELT echoes in each response, so they can keep the browser's default names.
* `standardise_attention()` warns when a standardisation group has no spread (e.g. an all-zero platform), because such a group is set to 0 and adds nothing to the attention index.
* `get_google_attention()`: single-keyword retries after a failed batch (`fallback_single = TRUE`) now request the keyword together with `anchor_keyword` and rescale it to the common anchor scale. Previously retried keywords were stored on their own 0–100 scale.
* `get_google_attention()`: `anchor_keyword` is now honoured when `batch = FALSE` (each keyword is requested with the anchor). Without an anchor, `batch = FALSE` now warns that each series is scaled to its own maximum.
* `attention_gap()` warns when `high_threat` is below 0.5. Concepts below `1 - high_threat` are low threat, so such values make the high- and low-threat ranges overlap, and overlapping concepts with high attention were silently classed `low_threat_high_attention`.
* `score_ambiguity()`: scores are rounded to six decimals before classing, and `ambiguity_class` is `"high"` from 0.60, matching the `audit_queries()` default `max_ambiguity = 0.60`. Previously a score of exactly 0.60 could be classed either way depending on floating-point residue.

## New features

* `platform_coverage()` gains `level = c("concept", "daily")`; `level = "daily"` returns one row per concept and date.
* `get_gdelt_attention()` spaces requests by `sleep = 6` seconds and retries rate-limited or transient responses with backoff (`retries = 3`). Failed requests are skipped with a warning, and `attr(, "gdelt_status")` records the outcome of every query (ok, no_articles or failed).

## Documentation

* All help pages are now generated from roxygen comments, so usage lines and defaults match the code (several hand-written pages listed rounded or missing defaults, e.g. `high_threat = 0.67` instead of `2/3`).
* Function-specific runnable examples were added for the 21 core workflow functions.
* Added this `NEWS.md`.

## Plots

* `plot_attention_gap()` draws the threshold lines stored in the `attention_gap()` classification rule, shades the high-threat low-attention region, colours classes with the colour-blind-safe Okabe–Ito palette, and labels every classified and near-threshold concept (with ggrepel when installed). Previously `check_overlap = TRUE` could silently drop labels of the key concepts. `label_top_n` now defaults to 0 (label only classified concepts).
* `plot_attention_gap_rank()` uses the same class palette and readable class names.
* `plot_attention_gap_sensitivity()` distinguishes robust from threshold-dependent concepts, marks the robustness cut-off, and gains `show_zero` and `robust_cutoff`.
* `plot_query_ambiguity()` no longer draws duplicate boxplot outliers or jitters scores vertically, and marks the disambiguation threshold (`max_ambiguity`).
* ggrepel added to Suggests.
* `plot_counterfactual_attention()`, `plot_campaign_effect_size()` and `plot_campaign_placebo_distribution()` use readable axis labels and locale-independent date labels, shade the post-event window, separate pre- and post-event deviations by colour, and show each placebo date as a dot with the observed deviation and placebo p-value. The fixed 2-month date breaks, which left short event windows without labels, were removed.
* Mock collection (`mock = TRUE`) respects the dictionary `platform` column, so platform-specific dictionaries produce the same rows as live collection.

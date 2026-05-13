# Publication readiness checklist for conCultuR

This document summarises the software-readiness tasks required before a journal or CRAN-style release. It is written for maintainers and reviewers who need to assess whether the package is more than an exploratory script bundle.

## Completed in this source bundle

- Core package files are versioned consistently as `0.8.6`.
- `NEWS.md` has been removed to reduce stale release-history clutter. Release notes should be managed through GitHub releases or the manuscript supplement.
- Exported functions have `.Rd` documentation in `man/`.
- The README defines the measurement problem, not only installation instructions.
- Vignettes cover offline use, live data, attention-gap analysis, campaign diagnostics, and interpretation/ethics.
- Old versioned demo scripts were removed from `inst/examples` to avoid confusing users with obsolete API calls.
- A single general-use live analysis template is provided at `inst/examples/conCultuR_real_analysis_template.R`.
- Google Trends, GDELT, and Wikimedia workflows are described as live-data options with known failure modes.
- Platform coverage diagnostics are part of the recommended workflow.

## Required before manuscript submission

1. Run `devtools::document()` after any roxygen edits.
2. Run `devtools::test()`.
3. Run `devtools::check(args = "--as-cran")` and archive the full check log.
4. Execute the live-data template and save raw traces, standardised traces, and platform coverage summaries.
5. Prepare two reproducible case studies:
   - threatened-but-under-attended species attention gap
   - campaign or media-shock diagnostic
6. Write a Methods section that emphasises measurement rather than platform access.
7. Explicitly state that attention gaps are communication signals, not ecological priority scores.

## Required before CRAN submission

- Confirm that all examples are offline or use `\dontrun{}` / `eval = FALSE` for live API calls.
- Confirm that tests do not depend on network access.
- Confirm that no raw social-media user data are included.
- Confirm current terms of use for Wikimedia, GDELT, Google Trends, Reddit, and YouTube if live outputs are redistributed.
- Update `cran-comments.md` with actual check results.

## Recommended manuscript framing

Suggested one-sentence framing:

> `conCultuR` implements a reproducible measurement framework for estimating, auditing, and interpreting biodiversity attention across heterogeneous digital platforms.

Core methodological contributions:

1. query ambiguity audit
2. platform triangulation and disagreement reporting
3. attention-gap inference with threshold sensitivity

Campaign diagnostics, burst detection, sentiment, and narrative-frame summaries should be framed as interpretation layers rather than the central novelty claim.

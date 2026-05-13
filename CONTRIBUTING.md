# Contributing to conCultuR

Contributions should improve reproducibility, measurement validity, or usability for conservation culturomics.

## Preferred contribution types

- improved ambiguity dictionaries
- new platform adapters with clear terms-of-use notes
- better platform-coverage diagnostics
- additional offline test fixtures
- improved vignettes and case studies
- bug fixes that make workflows more robust to missing or partial live data

## Development checklist

Before opening a pull request, run:

```r
devtools::document()
devtools::test()
devtools::check(args = "--as-cran")
```

Live-data examples should not be required for tests. Use mock data or cached fixtures for automated checks.

## Interpretation standards

Do not add functions or examples that frame digital attention as ecological value or conservation priority. The package measures communication signals.

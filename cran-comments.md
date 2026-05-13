## conCultuR CRAN and journal submission notes

This file is a submission checklist and should be updated with local check results before CRAN or journal submission.

## Local check status

Run the following before submission:

```r
devtools::document()
devtools::test()
devtools::check(args = "--as-cran")
```

Record the resulting status here.

- R version: TODO
- Operating system: TODO
- `R CMD check --as-cran`: TODO
- Errors: TODO
- Warnings: TODO
- Notes: TODO

## External data and live API policy

- Package examples and tests use mock or built-in data only.
- Live API workflows are placed in `inst/examples` and are not executed by tests.
- Google Trends support is optional and depends on `gtrendsR` and the availability of Google Trends responses in the user's session.
- GDELT and Wikimedia live-data examples should be interpreted as reproducible workflow templates, not as guaranteed stable API outputs.

## Interpretation policy

`conCultuR` estimates digital-attention and communication signals. It does not estimate ecological status, extinction risk, or conservation priority. This limitation is stated in README and vignettes.

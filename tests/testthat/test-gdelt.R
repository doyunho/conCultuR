gdelt_dict <- function() {
  data.frame(concept_id = c("a", "b", "c"), query = c("\"a\"", "\"b\"", "\"c\""), query_type = "gdelt_query",
             platform = "gdelt", language = "en", country = "global", ambiguity_score = 0,
             stringsAsFactors = FALSE)
}

## Shape of a GDELT DOC timelinevolraw response after jsonlite::fromJSON(flatten = TRUE).
gdelt_response <- function(dates, values) {
  list(query_details = list(title = "q"),
       timeline = data.frame(series = "Article Count",
                             data = I(list(data.frame(date = format(dates, "%Y%m%dT000000Z"), value = values, norm = 1000)))))
}

test_that("GDELT nested timelines are parsed and missing days zero-filled", {
  d <- as.Date("2026-10-01") + 0:4
  local_mocked_bindings(.cc_fetch_json = function(url, ...) gdelt_response(d[c(1, 3)], c(7, 2)))
  out <- as.data.frame(get_gdelt_attention(gdelt_dict()[1, ], from = d[1], to = d[5], sleep = 0))
  expect_equal(out$raw_value, c(7, 0, 2, 0, 0))
  expect_equal(attr(get_gdelt_attention(gdelt_dict()[1, ], from = d[1], to = d[5], sleep = 0), "gdelt_status")$status, "ok")
})

test_that("failed GDELT requests are skipped with a warning, not zero-filled", {
  d <- as.Date("2026-10-01") + 0:4
  local_mocked_bindings(.cc_fetch_json = function(url, ...) {
    if (grepl("%22b%22", url)) stop("HTTP request failed with status 429")
    gdelt_response(d, c(1, 2, 3, 4, 5))
  })
  expect_warning(out <- get_gdelt_attention(gdelt_dict(), from = d[1], to = d[5], sleep = 0), "skipped")
  expect_false("b" %in% out$concept_id)
  st <- attr(out, "gdelt_status")
  expect_equal(st$status[st$concept_id == "b"], "failed")
})

test_that("a valid response without a timeline is recorded as no articles", {
  d <- as.Date("2026-10-01") + 0:2
  local_mocked_bindings(.cc_fetch_json = function(url, ...) list())
  out <- get_gdelt_attention(gdelt_dict()[1, ], from = d[1], to = d[3], sleep = 0)
  expect_equal(as.data.frame(out)$raw_value, c(0, 0, 0))
  expect_equal(attr(out, "gdelt_status")$status, "no_articles")
})

test_that("empty responses are retried and an all-empty run warns", {
  d <- as.Date("2026-10-01") + 0:2
  calls <- 0
  local_mocked_bindings(.cc_fetch_json = function(url, ...) { calls <<- calls + 1; list() },
                        .cc_sleep = function(...) NULL)
  expect_warning(out <- get_gdelt_attention(gdelt_dict(), from = d[1], to = d[3], sleep = 1), "throttling")
  expect_equal(calls, 6)
  expect_true(all(attr(out, "gdelt_status")$status == "no_articles"))
})

test_that("a throttled empty response is replaced by the retried timeline", {
  d <- as.Date("2026-10-01") + 0:2
  n <- 0
  local_mocked_bindings(.cc_fetch_json = function(url, ...) { n <<- n + 1; if (n == 1) list() else gdelt_response(d, c(4, 5, 6)) },
                        .cc_sleep = function(...) NULL)
  out <- get_gdelt_attention(gdelt_dict()[1, ], from = d[1], to = d[3], sleep = 1)
  expect_equal(as.data.frame(out)$raw_value, c(4, 5, 6))
})

test_that("standardisation warns when a platform group has no spread", {
  x <- data.frame(date = as.Date("2024-01-01") + 0:5, country = "global", language = "en",
                  platform = rep(c("gdelt", "wikipedia"), each = 3), metric = rep(c("n", "views"), each = 3),
                  concept_id = "a", query = "a", raw_value = c(0, 0, 0, 10, 20, 15))
  expect_warning(standardise_attention(x), "gdelt / n")
})

test_that("robust z-scores keep information in zero-inflated counts", {
  v <- log1p(c(0, 0, 0, 0, 0, 0, 3, 5, 1))
  z <- conCultuR:::.cc_robust_zscore(v)
  expect_equal(stats::mad(v), 0)
  expect_true(all(z[1:6] == 0))
  expect_true(all(z[7:9] > 0))
  expect_equal(order(z[7:9]), order(v[7:9]))
})

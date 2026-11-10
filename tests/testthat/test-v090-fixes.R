test_that("ambiguity class matches the audit threshold at exactly 0.60", {
  dict <- data.frame(concept_id = "tiger", scientific_name = "Panthera tigris",
                     query = "Tiger", query_type = "wikipedia_title",
                     language = "en", country = "global", stringsAsFactors = FALSE)
  dict <- score_ambiguity(dict)
  expect_equal(dict$ambiguity_score, 0.6)
  expect_equal(dict$ambiguity_class, "high")
  expect_equal(audit_queries(dict)$query_status, "needs_disambiguation")
})

test_that("platform_coverage returns concept and daily levels", {
  ex <- con_culturomics_example_data()
  tr <- collect_attention(ex$dictionary, from = "2024-01-01", to = "2024-01-10", mock = TRUE)
  conc <- platform_coverage(tr)
  daily <- platform_coverage(tr, level = "daily")
  expect_true(all(c("concept_id", "mean_platforms_per_date") %in% names(conc)))
  expect_true(all(c("concept_id", "date", "n_platforms") %in% names(daily)))
  expect_equal(nrow(daily), nrow(attr(conc, "daily_coverage")))
})

test_that("collect_attention defaults to wikipedia, gdelt and google and trims dates", {
  ex <- con_culturomics_example_data()
  tr <- as.data.frame(collect_attention(ex$dictionary, from = "2024-01-01", to = "2024-01-10", mock = TRUE))
  expect_setequal(unique(tr$platform), c("wikipedia", "gdelt", "google_trends"))
  expect_true(all(as.Date(tr$date) >= as.Date("2024-01-01") & as.Date(tr$date) <= as.Date("2024-01-10")))
})

fake_gtrends <- function(levels, anchor = "anchor", fail_batches = FALSE) {
  force(levels)
  function(keywords, ...) {
    if (fail_batches && length(setdiff(keywords, anchor)) > 1) return(structure("blocked", class = "try-error"))
    ## Google Trends scales every request so its maximum keyword = 100.
    top <- max(levels[keywords])
    dates <- as.Date("2024-01-01") + 0:2
    list(interest_over_time = do.call(rbind, lapply(keywords, function(k) {
      data.frame(date = dates, hits = round(100 * levels[[k]] / top, 4), keyword = k, stringsAsFactors = FALSE)
    })))
  }
}

google_dict <- function() {
  data.frame(concept_id = c("a", "b", "c"), query = c("a", "b", "c"), query_type = "common",
             platform = "google_trends", language = "en", country = "global",
             ambiguity_score = 0, stringsAsFactors = FALSE)
}

test_that("anchor_keyword keeps keywords on one scale across batches", {
  lv <- c(a = 10, b = 40, c = 80, anchor = 20)
  local_mocked_bindings(.cc_gtrends_call = fake_gtrends(lv), google_trends_ready = function(...) TRUE)
  out <- get_google_attention(google_dict(), from = "2024-01-01", to = "2024-01-03",
                              batch = TRUE, batch_size = 2, anchor_keyword = "anchor", sleep = 0)
  m <- tapply(out$raw_value, out$query, mean)
  expect_equal(unname(m["b"] / m["a"]), 4, tolerance = 1e-6)
  expect_equal(unname(m["c"] / m["a"]), 8, tolerance = 1e-6)
})

test_that("single-keyword retries keep the anchor scale", {
  lv <- c(a = 10, b = 40, c = 80, anchor = 20)
  local_mocked_bindings(.cc_gtrends_call = fake_gtrends(lv, fail_batches = TRUE), google_trends_ready = function(...) TRUE)
  out <- get_google_attention(google_dict(), from = "2024-01-01", to = "2024-01-03",
                              batch = TRUE, batch_size = 3, anchor_keyword = "anchor",
                              fallback_single = TRUE, sleep = 0)
  m <- tapply(out$raw_value, out$query, mean)
  expect_equal(unname(m["c"] / m["a"]), 8, tolerance = 1e-6)
})

test_that("batch = FALSE without an anchor warns about self-scaled series", {
  lv <- c(a = 10, b = 40, c = 80)
  local_mocked_bindings(.cc_gtrends_call = fake_gtrends(lv), google_trends_ready = function(...) TRUE)
  expect_warning(
    get_google_attention(google_dict(), from = "2024-01-01", to = "2024-01-03", batch = FALSE, sleep = 0),
    "own maximum"
  )
})

test_that("Google preset windows are used only when they reach the start date", {
  tw <- conCultuR:::.cc_google_time_window
  expect_equal(tw(Sys.Date() - 20, Sys.Date() - 1), "today 1-m")
  expect_equal(tw(Sys.Date() - 60, Sys.Date() - 1), "today 3-m")
  ## A 91-day window ending recently but starting before the 3-month preset.
  from <- Sys.Date() - 98
  expect_equal(tw(from, Sys.Date() - 8), paste(from, Sys.Date() - 8))
  expect_equal(tw(Sys.Date() - 300, Sys.Date() - 1), "today 12-m")
})

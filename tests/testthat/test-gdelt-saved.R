saved_dict <- function() {
  data.frame(concept_id = c("a", "b", "c"), query = c("\"alpha beta\"", "(\"b\" OR \"bee\")", "\"c\""),
             query_type = "gdelt_query", platform = "gdelt", language = "en", country = "global",
             ambiguity_score = 0, stringsAsFactors = FALSE)
}

saved_json <- function(query, dates, values) {
  data <- paste0('{ "date": "', format(dates, "%Y%m%dT000000Z"), '", "value": ', values, ', "norm": 1000}', collapse = ",")
  paste0('{"query_details": {"title": ', jsonlite::toJSON(query, auto_unbox = TRUE),
         ', "date_resolution": "day"}, "timeline": [ { "series": "Article Count", "data": [ ', data, ' ] } ] }')
}

test_that("gdelt_query_urls lists one encoded URL per query", {
  u <- gdelt_query_urls(saved_dict(), from = "2026-08-01", to = "2026-08-03")
  expect_equal(u$concept_id, c("a", "b", "c"))
  expect_equal(u$file_name, c("1_a.json", "2_b.json", "3_c.json"))
  expect_match(u$url[1], "query=%22alpha%20beta%22&mode=timelinevolraw&format=json&startdatetime=20260801000000&enddatetime=20260803235959", fixed = TRUE)
})

test_that("saved responses are matched by echoed query, HTML wrappers and errors handled", {
  d <- as.Date("2026-08-01") + 0:2
  dir <- tempfile(); dir.create(dir)
  ## Raw JSON under the browser's default name, matched by query_details$title.
  writeLines(saved_json("\"alpha beta\"", d[c(1, 3)], c(5, 2)), file.path(dir, "doc.json"))
  ## JSON wrapped in an HTML page, matched by title as well.
  html <- paste0('<html><head></head><body><pre>', gsub('"', "&quot;", saved_json("(\"b\" OR \"bee\")", d, c(1, 0, 4))), '</pre></body></html>')
  writeLines(html, file.path(dir, "doc (1).html"))
  ## Rate-limit notice saved instead of JSON: no title, matched by file name.
  writeLines("Please limit requests to one every 5 seconds", file.path(dir, "3_c.json"))
  expect_warning(out <- read_gdelt_json(saved_dict(), dir, from = d[1], to = d[3]), "holds no JSON")
  x <- as.data.frame(out)
  expect_equal(x$raw_value[x$concept_id == "a"], c(5, 0, 2))
  expect_equal(x$raw_value[x$concept_id == "b"], c(1, 0, 4))
  st <- attr(out, "gdelt_status")
  expect_equal(st$status, c("ok", "ok", "failed"))
  expect_true(all(x$platform == "gdelt"))
  expect_match(unique(x$source), "gdelt_doc_api_saved")
})

test_that("queries without a saved file are reported as missing", {
  d <- as.Date("2026-08-01") + 0:1
  dir <- tempfile(); dir.create(dir)
  writeLines(saved_json("\"alpha beta\"", d, c(1, 1)), file.path(dir, "1_a.json"))
  out <- read_gdelt_json(saved_dict(), dir, from = d[1], to = d[2])
  expect_equal(attr(out, "gdelt_status")$status, c("ok", "missing", "missing"))
})

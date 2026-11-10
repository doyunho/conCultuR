test_that("mock collectors share required columns and bind across platforms", {
  ex <- con_culturomics_example_data()
  dict <- score_ambiguity(ex$dictionary)
  tr <- collect_attention(dict, platforms = c("google", "wikipedia", "reddit"), from = "2024-01-01", to = "2024-01-10", mock = TRUE)
  d <- as.data.frame(tr)
  expect_true(nrow(d) > 0)
  expect_true(all(c("date", "platform", "concept_id", "query", "raw_value") %in% names(d)))
  expect_true(length(unique(d$platform)) >= 3)
})

test_that("row binding handles heterogeneous live-style columns", {
  a <- data.frame(date = as.Date("2024-01-01"), platform = "wikipedia", concept_id = "a", raw_value = 1)
  b <- data.frame(date = as.Date("2024-01-01"), platform = "google_trends", concept_id = "a", raw_value = 1, google_trends_batch_id = 1)
  out <- conCultuR:::.cc_bind_rows_fill(list(a, b))
  expect_true(all(c("google_trends_batch_id", "platform") %in% names(out)))
  expect_equal(nrow(out), 2)
})

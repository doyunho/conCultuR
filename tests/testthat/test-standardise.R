test_that("standardisation and attention index return expected columns", {
  ex <- con_culturomics_example_data()
  tr <- collect_attention(score_ambiguity(ex$dictionary), platforms = c("google", "wikipedia"), from = "2024-01-01", to = "2024-01-20", mock = TRUE)
  st <- standardise_attention(tr, method = "robust_zscore", transform = "log1p")
  expect_true("scaled_value" %in% names(as.data.frame(st)))
  idx <- attention_index(st, min_platforms = 1)
  expect_true(all(c("date", "concept_id", "attention_index", "platform_disagreement") %in% names(idx)))
})

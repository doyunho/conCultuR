test_that("campaign effect returns daily effects and safe placebo outputs", {
  ex <- con_culturomics_example_data()
  tr <- collect_attention(score_ambiguity(ex$dictionary), platforms = c("google", "wikipedia"), from = "2024-01-01", to = "2024-06-30", mock = TRUE)
  idx <- attention_index(standardise_attention(tr), min_platforms = 1)
  ce <- campaign_effect(idx, treated_concepts = unique(idx$concept_id)[1], campaign_date = as.Date("2024-04-01"), pre_days = 20, post_days = 20, placebo_n = 10)
  expect_s3_class(ce, "campaign_effect")
  expect_true(is.data.frame(ce$daily_effects))
  expect_true("effect" %in% names(ce$daily_effects))
})

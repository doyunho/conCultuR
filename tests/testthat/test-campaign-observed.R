test_that("campaign_effect daily_effects exposes plotting columns", {
  set.seed(1)
  dates <- seq(as.Date("2024-01-01"), as.Date("2024-04-30"), by = "day")
  idx <- rbind(
    data.frame(date = dates, country = "global", language = "en", concept_id = "treated", attention_index = sin(seq_along(dates)/8)),
    data.frame(date = dates, country = "global", language = "en", concept_id = "control", attention_index = cos(seq_along(dates)/9))
  )
  ce <- campaign_effect(idx, campaign_date = as.Date("2024-03-01"), treated_concepts = "treated", control_concepts = "control", pre_days = 30, post_days = 20, placebo_n = 5)
  expect_true(all(c("date", "observed", "counterfactual", "effect", "period") %in% names(ce$daily_effects)))
  expect_s3_class(plot_counterfactual_attention(ce), "ggplot")
})

placebo_fixture <- function(n_days = 60) {
  dates <- as.Date("2024-01-01") + seq_len(n_days) - 1
  set.seed(2)
  do.call(rbind, lapply(c("t", "c1", "c2"), function(cid) {
    data.frame(date = dates, concept_id = cid, attention_index = stats::rnorm(n_days))
  }))
}

test_that("placebo dates are distinct and never overlap the real post-event window", {
  idx <- placebo_fixture(60)
  event <- as.Date("2024-02-10")
  expect_warning(
    ce <- campaign_effect(idx, campaign_date = event, treated_concepts = "t",
                          pre_days = 10, post_days = 7, placebo_n = 100),
    "distinct placebo dates"
  )
  pd <- ce$placebo_effects$placebo_date
  expect_equal(length(pd), length(unique(pd)))
  expect_true(all(pd - 10 > event + 7 | pd + 7 < event))
  expect_equal(ce$placebo_n_used, length(pd))
  expect_equal(ce$min_attainable_p, 1 / (1 + length(pd)))
  expect_gte(ce$placebo_p_value, ce$min_attainable_p)
})

test_that("placebo_exclude_event = FALSE keeps overlapping dates", {
  idx <- placebo_fixture(60)
  ce <- suppressWarnings(campaign_effect(idx, campaign_date = as.Date("2024-02-10"), treated_concepts = "t",
                                         pre_days = 10, post_days = 7, placebo_n = 100,
                                         placebo_exclude_event = FALSE))
  expect_true(any(abs(ce$placebo_effects$placebo_date - as.Date("2024-02-10")) < 7))
})

test_that("one-sided placebo tests count only the stated direction", {
  set.seed(3)
  d <- seq(as.Date("2024-01-01"), by = "day", length.out = 200)
  x <- rbind(data.frame(date = d, concept_id = "t", attention_index = rnorm(200)),
             data.frame(date = d, concept_id = "c", attention_index = rnorm(200)))
  x$attention_index[x$concept_id == "t" & x$date >= d[170]] <- x$attention_index[x$concept_id == "t" & x$date >= d[170]] + 1
  args <- list(x, campaign_date = d[170], treated_concepts = "t", control_concepts = "c",
               pre_days = 14, post_days = 14, placebo_n = 50)
  two <- do.call(campaign_effect, args)
  gr <- do.call(campaign_effect, c(args, alternative = "greater"))
  ls <- do.call(campaign_effect, c(args, alternative = "less"))
  pe <- gr$placebo_effects$average_post_effect
  n <- sum(is.finite(pe))
  expect_equal(gr$placebo_p_value, (1 + sum(pe >= gr$average_post_effect)) / (1 + n))
  expect_equal(ls$placebo_p_value, (1 + sum(pe <= ls$average_post_effect)) / (1 + n))
  expect_equal(two$placebo_alternative, "two.sided")
  expect_equal(gr$placebo_alternative, "greater")
  expect_lte(gr$placebo_p_value, two$placebo_p_value)
  expect_error(do.call(campaign_effect, c(args, alternative = "up")))
})

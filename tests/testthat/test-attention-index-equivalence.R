index_fixture <- function() {
  ex <- con_culturomics_example_data()
  dict <- score_ambiguity(ex$dictionary)
  tr <- collect_attention(dict, platforms = c("wikipedia", "gdelt", "google"),
                          from = "2024-01-01", to = "2024-01-20", mock = TRUE)
  std <- as.data.frame(standardise_attention(tr))
  ## Missing values and a dropped platform-date exercise the edge cases.
  set.seed(3)
  std$scaled_value[sample(nrow(std), 40)] <- NA
  std <- std[!(std$platform == "gdelt" & std$date == as.Date("2024-01-05")), ]
  std
}

expect_same_index <- function(new, old) {
  expect_equal(nrow(new), nrow(old))
  for (nm in names(old)) {
    expect_equal(new[[nm]], old[[nm]], tolerance = 1e-10, info = nm)
  }
}

test_that("vectorised attention_index matches the 0.8.6 implementation", {
  std <- index_fixture()
  args <- list(
    list(),
    list(aggregate = "flat"),
    list(weights = c(gdelt = 2, wikipedia = 0.5)),
    list(aggregate = "flat", weights = c(google_trends = 3)),
    list(min_platforms = 3),
    list(use_ambiguity = FALSE, query_type_weights = c(common = 1, hashtag = 0.2))
  )
  for (a in args) {
    new <- do.call(attention_index, c(list(std), a))
    old <- do.call(legacy_attention_index, c(list(std), a))
    expect_same_index(new, old)
  }
})

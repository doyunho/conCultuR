test_that("mock attention index works with missing country", {
  taxa <- data.frame(
    scientific_name = c("Panthera onca", "Ailuropoda melanoleuca", "Orcinus orca"),
    common_name = c("jaguar", "giant panda", "orca"),
    stringsAsFactors = FALSE
  )
  dict <- make_concept_dictionary(taxa, languages = "en")
  dict <- expand_common_names(dict, hashtags = TRUE)
  dict <- score_ambiguity(dict)
  traces <- collect_attention(dict, platforms = c("google", "wikipedia", "reddit"), from = "2024-01-01", to = "2024-01-10", mock = TRUE)
  traces_std <- standardise_attention(traces)
  idx <- attention_index(traces_std)
  expect_true(nrow(idx) > 0)
  expect_true(all(c("concept_id", "attention_index") %in% names(idx)))
})

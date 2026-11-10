test_that("dictionary and mock attention workflow works", {
  taxa <- data.frame(
    scientific_name = c("Panthera onca", "Ailuropoda melanoleuca"),
    common_name = c("jaguar", "giant panda")
  )
  dict <- make_concept_dictionary(taxa)
  dict <- score_ambiguity(dict)
  expect_true(nrow(dict) >= 2)
  expect_true("ambiguity_score" %in% names(dict))

  tr <- get_google_attention(dict, from = "2024-01-01", to = "2024-01-10", mock = TRUE)
  tr <- standardise_attention(tr)
  idx <- attention_index(tr)
  expect_true(nrow(idx) > 0)
})

test_that("attention gap workflow works", {
  taxa <- data.frame(
    scientific_name = c("Panthera onca", "Ailuropoda melanoleuca"),
    common_name = c("jaguar", "giant panda")
  )
  dict <- make_concept_dictionary(taxa)
  tr <- get_google_attention(dict, from = "2024-01-01", to = "2024-03-01", mock = TRUE)
  idx <- attention_index(standardise_attention(tr))
  status <- data.frame(concept_id = unique(dict$concept_id), status = c("NT", "VU"))
  gap <- attention_gap(idx, status)
  expect_s3_class(gap, "culturomic_gap")
})

## Reference implementation of attention_index() from conCultuR 0.8.6 (row-wise loop),
## kept to test that the vectorised 0.9.0 version gives identical results.
legacy_attention_index <- function(x,
                            weights = NULL,
                            min_platforms = 1,
                            query_weight_col = NULL,
                            use_ambiguity = TRUE,
                            query_type_weights = NULL,
                            aggregate = c("two_stage", "flat")) {
  aggregate <- match.arg(aggregate)
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  conCultuR:::.cc_required_cols(x, c("date", "country", "language", "platform", "concept_id", "scaled_value"), "x")
  if (all(is.na(x$scaled_value))) {
    x <- as.data.frame(standardise_attention(x), stringsAsFactors = FALSE)
  }
  if (!"query" %in% names(x)) x$query <- "<query>"
  if (!"query_type" %in% names(x)) x$query_type <- NA_character_
  if (!"ambiguity_score" %in% names(x)) x$ambiguity_score <- 0
  x$scaled_value <- as.numeric(x$scaled_value)

  qtw <- conCultuR:::.cc_query_type_weight(x$query_type)
  if (!is.null(query_type_weights)) {
    hit <- match(tolower(as.character(x$query_type)), tolower(names(query_type_weights)))
    qtw[!is.na(hit)] <- as.numeric(query_type_weights[hit[!is.na(hit)]])
  }
  aw <- if (isTRUE(use_ambiguity)) pmax(0.05, 1 - pmin(1, pmax(0, as.numeric(x$ambiguity_score)))) else rep(1, nrow(x))
  cw <- if (!is.null(query_weight_col) && query_weight_col %in% names(x)) as.numeric(x[[query_weight_col]]) else rep(1, nrow(x))
  x$.query_weight <- qtw * aw * cw

  key_cols <- c("date", "country", "language", "concept_id")
  key <- conCultuR:::.cc_make_key(x, key_cols)
  res <- lapply(split(seq_len(nrow(x)), key), function(idx) {
    dat <- x[idx, , drop = FALSE]
    platforms <- unique(dat$platform)
    if (length(platforms) < min_platforms) return(NULL)

    if (aggregate == "flat") {
      if (is.null(weights)) {
        pw_obs <- rep(1, nrow(dat))
      } else {
        pw_obs <- weights[dat$platform]
        pw_obs[is.na(pw_obs)] <- 1
      }
      ai <- conCultuR:::.cc_weighted_mean(dat$scaled_value, dat$.query_weight * pw_obs)
      platform_means <- tapply(dat$scaled_value, dat$platform, mean, na.rm = TRUE)
    } else {
      qp_key <- conCultuR:::.cc_make_key(dat, c("platform", "query"))
      query_rows <- lapply(split(seq_len(nrow(dat)), qp_key), function(j) {
        sub <- dat[j, , drop = FALSE]
        data.frame(
          platform = sub$platform[1],
          query = sub$query[1],
          query_value = conCultuR:::.cc_weighted_mean(sub$scaled_value, sub$.query_weight),
          query_weight = mean(sub$.query_weight, na.rm = TRUE),
          stringsAsFactors = FALSE
        )
      })
      qdat <- conCultuR:::.cc_bind_rows_fill(query_rows)
      p_rows <- lapply(split(seq_len(nrow(qdat)), qdat$platform), function(j) {
        sub <- qdat[j, , drop = FALSE]
        data.frame(
          platform = sub$platform[1],
          platform_value = conCultuR:::.cc_weighted_mean(sub$query_value, sub$query_weight),
          n_queries_platform = length(unique(sub$query)),
          stringsAsFactors = FALSE
        )
      })
      pdat <- conCultuR:::.cc_bind_rows_fill(p_rows)
      if (is.null(weights)) {
        pdat$platform_weight <- 1
      } else {
        pdat$platform_weight <- weights[pdat$platform]
        pdat$platform_weight[is.na(pdat$platform_weight)] <- 1
      }
      ai <- conCultuR:::.cc_weighted_mean(pdat$platform_value, pdat$platform_weight)
      platform_means <- stats::setNames(pdat$platform_value, pdat$platform)
    }

    data.frame(
      date = dat$date[1],
      country = dat$country[1],
      language = dat$language[1],
      concept_id = dat$concept_id[1],
      attention_index = ai,
      platform_disagreement = conCultuR:::.cc_safe_sd(platform_means),
      n_platforms = length(platforms),
      n_queries = length(unique(dat$query)),
      aggregation_method = aggregate,
      stringsAsFactors = FALSE
    )
  })
  out <- conCultuR:::.cc_bind_rows_fill(res[!vapply(res, is.null, logical(1))])
  if (is.null(out)) out <- data.frame()
  out
}

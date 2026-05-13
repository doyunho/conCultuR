#' Create a conservation concept dictionary
#'
#' @param taxa Data frame or character vector of scientific names.
#' @param scientific_name Column name for scientific names when taxa is a data frame.
#' @param common_name Column name for common names when taxa is a data frame.
#' @param concept_id Optional column name or vector of concept identifiers.
#' @param languages Languages to attach to generated queries.
#' @param countries Countries to attach to generated queries.
#' @param synonyms Optional synonym table with concept_id, query, and optionally language and country.
#' @param include_scientific Include scientific names as queries.
#' @return A culturomic_dictionary object.
#' @export
make_concept_dictionary <- function(taxa,
                                    scientific_name = "scientific_name",
                                    common_name = "common_name",
                                    concept_id = NULL,
                                    languages = "en",
                                    countries = "global",
                                    synonyms = NULL,
                                    include_scientific = TRUE) {
  if (is.character(taxa)) {
    taxa <- data.frame(scientific_name = taxa, common_name = NA_character_, stringsAsFactors = FALSE)
    scientific_name <- "scientific_name"
    common_name <- "common_name"
  } else {
    taxa <- as.data.frame(taxa, stringsAsFactors = FALSE)
  }
  .cc_required_cols(taxa, scientific_name, "taxa")
  if (!common_name %in% names(taxa)) taxa[[common_name]] <- NA_character_

  if (is.null(concept_id)) {
    ids <- paste0("concept_", seq_len(nrow(taxa)))
  } else if (length(concept_id) == 1 && concept_id %in% names(taxa)) {
    ids <- as.character(taxa[[concept_id]])
  } else if (length(concept_id) == nrow(taxa)) {
    ids <- as.character(concept_id)
  } else {
    stop("concept_id must be NULL, a column name, or a vector with one value per taxon.", call. = FALSE)
  }

  rows <- list()
  k <- 1
  for (i in seq_len(nrow(taxa))) {
    names_i <- character(0)
    types_i <- character(0)
    if (include_scientific) {
      names_i <- c(names_i, taxa[[scientific_name]][i])
      types_i <- c(types_i, "scientific")
    }
    common_i <- taxa[[common_name]][i]
    if (!is.na(common_i) && nzchar(common_i)) {
      common_parts <- trimws(unlist(strsplit(common_i, "[|;]")))
      common_parts <- common_parts[nzchar(common_parts)]
      names_i <- c(names_i, common_parts)
      types_i <- c(types_i, rep("common", length(common_parts)))
    }
    if (length(names_i) == 0) next
    for (lang in languages) {
      for (ctry in countries) {
        rows[[k]] <- data.frame(
          concept_id = ids[i],
          scientific_name = taxa[[scientific_name]][i],
          query = names_i,
          query_type = types_i,
          language = lang,
          country = ctry,
          ambiguity_score = NA_real_,
          ambiguity_class = NA_character_,
          disambiguation_rule = NA_character_,
          platform = NA_character_,
          stringsAsFactors = FALSE
        )
        k <- k + 1
      }
    }
  }
  out <- .cc_bind_rows_fill(rows)

  if (!is.null(synonyms)) {
    out <- expand_common_names(out, synonyms = synonyms, hashtags = FALSE)
  }
  out <- unique(out)
  rownames(out) <- NULL
  class(out) <- c("culturomic_dictionary", class(out))
  out
}

#' @export
print.culturomic_dictionary <- function(x, ...) {
  cat("culturomic_dictionary\n")
  cat("  rows: ", nrow(x), "\n", sep = "")
  cat("  concepts: ", length(unique(x$concept_id)), "\n", sep = "")
  cat("  languages: ", paste(unique(x$language), collapse = ", "), "\n", sep = "")
  invisible(x)
}

#' Expand dictionary with synonyms and hashtags
#'
#' @param dictionary A culturomic_dictionary.
#' @param synonyms Optional synonym data frame.
#' @param hashtags Add hashtag queries for common names and synonyms.
#' @param lowercase Convert all queries to lower case.
#' @return Expanded dictionary.
#' @export
expand_common_names <- function(dictionary, synonyms = NULL, hashtags = TRUE, lowercase = FALSE) {
  dictionary <- as.data.frame(dictionary, stringsAsFactors = FALSE)
  .cc_required_cols(dictionary, c("concept_id", "scientific_name", "query", "query_type", "language", "country"), "dictionary")
  out <- dictionary

  if (!is.null(synonyms)) {
    synonyms <- as.data.frame(synonyms, stringsAsFactors = FALSE)

    # Accept both package-native synonym tables
    #   concept_id, query
    # and user-friendly tables often written as
    #   scientific_name, synonym
    # or
    #   scientific_name, common_name.
    # This makes tests, vignettes, and interactive use less brittle.
    if (!"query" %in% names(synonyms)) {
      if ("synonym" %in% names(synonyms)) {
        synonyms$query <- synonyms$synonym
      } else if ("common_name" %in% names(synonyms)) {
        synonyms$query <- synonyms$common_name
      } else if ("translated_name" %in% names(synonyms)) {
        synonyms$query <- synonyms$translated_name
      }
    }

    if (!"concept_id" %in% names(synonyms)) {
      if (!"scientific_name" %in% names(synonyms)) {
        stop("synonyms must contain concept_id or scientific_name, and a query/synonym column.", call. = FALSE)
      }
      synonyms <- merge(synonyms, unique(dictionary[c("concept_id", "scientific_name")]),
                        by = "scientific_name", all.x = TRUE)
    }

    .cc_required_cols(synonyms, c("concept_id", "query"), "synonyms")
    if (!"language" %in% names(synonyms)) synonyms$language <- NA_character_
    if (!"country" %in% names(synonyms)) synonyms$country <- NA_character_
    if (!"query_type" %in% names(synonyms)) synonyms$query_type <- "synonym"

    syn <- merge(
      unique(dictionary[c("concept_id", "scientific_name")]),
      synonyms,
      by = "concept_id",
      all.y = TRUE,
      suffixes = c(".dictionary", ".synonym")
    )

    if (!"scientific_name" %in% names(syn)) {
      sci_cols <- intersect(c("scientific_name.dictionary", "scientific_name.synonym"), names(syn))
      if (length(sci_cols) > 0) {
        syn$scientific_name <- syn[[sci_cols[1]]]
        if (length(sci_cols) > 1) {
          miss <- is.na(syn$scientific_name) | !nzchar(as.character(syn$scientific_name))
          syn$scientific_name[miss] <- syn[[sci_cols[2]]][miss]
        }
      } else {
        syn$scientific_name <- NA_character_
      }
    }

    syn$ambiguity_score <- NA_real_
    syn$ambiguity_class <- NA_character_
    syn$disambiguation_rule <- NA_character_
    if (!"platform" %in% names(syn)) syn$platform <- NA_character_

    for (nm in names(dictionary)) {
      if (!nm %in% names(syn)) syn[[nm]] <- NA
    }
    syn <- syn[names(dictionary)]
    out <- .cc_bind_rows_fill(list(out, syn))
  }

  if (isTRUE(hashtags)) {
    tag_base <- out[out$query_type %in% c("common", "synonym", "translated_common"), , drop = FALSE]
    if (nrow(tag_base) > 0) {
      tag_base$query <- paste0("#", gsub("\\s+", "", tag_base$query))
      tag_base$query_type <- "hashtag"
      out <- rbind(out, tag_base)
    }
  }

  if (isTRUE(lowercase)) out$query <- tolower(out$query)
  out <- unique(out)
  rownames(out) <- NULL
  class(out) <- c("culturomic_dictionary", setdiff(class(out), "culturomic_dictionary"))
  out
}

.cc_default_ambiguity_weights <- function() {
  data.frame(
    term = c(
      "jaguar", "seal", "crane", "puma", "cougar", "mustang", "falcon", "condor",
      "panda", "orca", "python", "cobra", "swift", "kiwi", "turkey", "buffalo",
      "lotus", "iris", "sage", "bass", "mole", "ray", "skate", "robin", "lynx",
      "fox", "turtle", "rhino", "tiger", "monarch", "bear", "wolf", "eagle", "panther", "orca"
    ),
    extra_score = c(
      0.65, 0.65, 0.60, 0.60, 0.60, 0.55, 0.45, 0.35,
      0.30, 0.35, 0.65, 0.55, 0.50, 0.55, 0.45, 0.35,
      0.35, 0.30, 0.45, 0.55, 0.40, 0.40, 0.30, 0.35, 0.45,
      0.25, 0.20, 0.20, 0.40, 0.45, 0.30, 0.30, 0.35, 0.35, 0.35
    ),
    reason = c(
      "animal, car brand, sports and media uses", "animal, action verb, object and organisations", "bird, construction machine and place names",
      "animal, brand and sports uses", "animal, demographics and brands", "animal and vehicle/brand uses", "bird and brand/sports uses", "bird and place/brand uses",
      "animal and software/media uses", "animal and software/brand uses", "animal and programming language", "snake and brand/security uses",
      "bird and programming/product uses", "bird, fruit and nationality", "bird and country/food", "animal and place/brand uses",
      "plant and vehicle/software uses", "plant and human name", "plant and adjective", "fish and music term", "animal and unit/skin mark",
      "fish and geometry/technology term", "fish and sport/object", "bird and human name", "animal and software/brand uses",
      "animal and political/media uses", "animal and cartoon/product uses", "animal and product/brand uses",
      "animal and sports/brand/geopolitical uses", "butterfly and monarchy/political/media uses", "animal and place/brand uses",
      "animal and sports/brand uses", "bird and sports/brand/national symbol uses", "animal and brand/media uses", "animal and software/brand uses"
    ),
    stringsAsFactors = FALSE
  )
}

#' Score query ambiguity
#'
#' @param dictionary A culturomic_dictionary.
#' @param ambiguous_terms Optional character vector of known ambiguous terms.
#' @param ambiguity_weights Optional data frame with term and extra_score columns.
#' @return Dictionary with ambiguity_score, ambiguity_class, and ambiguity_reason.
#' @export
score_ambiguity <- function(dictionary, ambiguous_terms = NULL, ambiguity_weights = NULL) {
  dictionary <- as.data.frame(dictionary, stringsAsFactors = FALSE)
  .cc_required_cols(dictionary, c("query", "query_type"), "dictionary")
  if (is.null(ambiguity_weights)) ambiguity_weights <- .cc_default_ambiguity_weights()
  if (!is.null(ambiguous_terms)) {
    ambiguity_weights <- data.frame(term = ambiguous_terms, extra_score = 0.55, reason = "user supplied ambiguous term", stringsAsFactors = FALSE)
  }
  .cc_required_cols(ambiguity_weights, c("term", "extra_score"), "ambiguity_weights")
  if (!"reason" %in% names(ambiguity_weights)) ambiguity_weights$reason <- "ambiguous term"

  q_raw <- as.character(dictionary$query)
  q <- tolower(gsub("^#", "", q_raw))
  q_plain <- gsub("[^[:alnum:] ]+", " ", q)
  q_plain <- trimws(gsub("\\s+", " ", q_plain))
  word_count <- lengths(strsplit(q_plain, "\\s+"))
  single <- word_count == 1
  short <- nchar(q_plain) <= 4
  scientific <- dictionary$query_type == "scientific" | grepl("^[A-Z][a-z]+\\s+[a-z-]+$", q_raw)
  hashtag <- dictionary$query_type == "hashtag" | grepl("^#", q_raw)

  weight_map <- stats::setNames(as.numeric(ambiguity_weights$extra_score), tolower(ambiguity_weights$term))
  reason_map <- stats::setNames(as.character(ambiguity_weights$reason), tolower(ambiguity_weights$term))
  extra <- numeric(length(q_plain))
  matched_reason <- rep(NA_character_, length(q_plain))
  for (ii in seq_along(q_plain)) {
    tokens <- unique(unlist(strsplit(q_plain[ii], "\\s+")))
    tokens <- tokens[nzchar(tokens)]
    candidates <- unique(c(q_plain[ii], tokens))
    hits <- candidates[candidates %in% names(weight_map)]
    if (length(hits) > 0) {
      vals <- as.numeric(weight_map[hits])
      best <- hits[which.max(vals)]
      extra[ii] <- max(vals, na.rm = TRUE)
      matched_reason[ii] <- as.character(reason_map[best])
    }
  }

  score <- rep(0.08, length(q_plain))
  score <- score + ifelse(single, 0.12, 0)
  score <- score + ifelse(short, 0.08, 0)
  score <- score + extra
  score <- score + ifelse(hashtag, 0.08, 0)
  score <- score - ifelse(scientific, 0.45, 0)
  score <- pmax(0, pmin(1, score))

  dictionary$ambiguity_score <- score
  dictionary$ambiguity_class <- as.character(cut(score, breaks = c(-Inf, 0.25, 0.60, Inf), labels = c("low", "medium", "high"), right = TRUE))
  dictionary$ambiguity_reason <- matched_reason
  dictionary$ambiguity_reason[is.na(dictionary$ambiguity_reason) & single] <- "short or single-word query"
  dictionary$ambiguity_reason[is.na(dictionary$ambiguity_reason)] <- "no known ambiguity flag"
  class(dictionary) <- c("culturomic_dictionary", setdiff(class(dictionary), "culturomic_dictionary"))
  dictionary
}

#' Audit query risks
#'
#' @param dictionary A culturomic_dictionary.
#' @param max_ambiguity Threshold for high risk.
#' @return Data frame of query diagnostics.
#' @export
audit_queries <- function(dictionary, max_ambiguity = 0.60) {
  if (!"ambiguity_score" %in% names(dictionary) || all(is.na(dictionary$ambiguity_score))) {
    dictionary <- score_ambiguity(dictionary)
  }
  out <- as.data.frame(dictionary, stringsAsFactors = FALSE)
  if (!"ambiguity_reason" %in% names(out)) out$ambiguity_reason <- NA_character_
  out$query_status <- ifelse(out$ambiguity_score >= max_ambiguity, "needs_disambiguation", "usable")
  out$suggestion <- ifelse(
    out$query_status == "needs_disambiguation",
    "use scientific name, add exclusion terms, split by language/geography, or manually validate a sample",
    ifelse(out$ambiguity_class == "medium", "inspect if this query dominates the attention index", "no immediate action")
  )
  out[c("concept_id", "scientific_name", "query", "query_type", "language", "country", "ambiguity_score", "ambiguity_class", "ambiguity_reason", "query_status", "suggestion")]
}

#' Validate query table
#'
#' @param dictionary A culturomic_dictionary.
#' @param max_ambiguity Threshold for warnings.
#' @return Dictionary with validation attributes.
#' @export
validate_queries <- function(dictionary, max_ambiguity = 0.60) {
  audit <- audit_queries(dictionary, max_ambiguity = max_ambiguity)
  attr(dictionary, "query_audit") <- audit
  attr(dictionary, "n_high_risk_queries") <- sum(audit$query_status == "needs_disambiguation", na.rm = TRUE)
  dictionary
}

#' Translate concepts using supplied, built-in mock, or simple generated translations
#'
#' @param dictionary A culturomic_dictionary.
#' @param translations Optional data frame with concept_id or scientific_name, language, and translated_name.
#' @param languages Optional languages used when translations are not supplied.
#' @param mock When TRUE, generate deterministic placeholder translations for demonstration.
#' @param add_hashtags Add hashtags for translated names.
#' @return Expanded dictionary.
#' @export
translate_concepts <- function(dictionary,
                               translations = NULL,
                               languages = NULL,
                               mock = FALSE,
                               add_hashtags = TRUE) {
  dictionary <- as.data.frame(dictionary, stringsAsFactors = FALSE)
  .cc_required_cols(dictionary, c("concept_id", "scientific_name", "query", "query_type", "language", "country"), "dictionary")
  lookup <- unique(dictionary[c("concept_id", "scientific_name")])

  if (is.null(translations)) {
    if (is.null(languages)) {
      stop("Provide translations or languages. Set mock = TRUE for generated demonstration translations.", call. = FALSE)
    }
    languages <- setdiff(languages, unique(dictionary$language))
    if (length(languages) == 0) return(dictionary)
    translations <- .cc_bind_rows_fill(lapply(languages, function(lang) {
      data.frame(
        concept_id = lookup$concept_id,
        language = lang,
        translated_name = if (isTRUE(mock)) paste0(lang, "_", gsub("\\s+", "_", lookup$scientific_name)) else lookup$scientific_name,
        stringsAsFactors = FALSE
      )
    }))
  }

  translations <- as.data.frame(translations, stringsAsFactors = FALSE)
  if (!"concept_id" %in% names(translations) && !"scientific_name" %in% names(translations)) {
    stop("translations must contain concept_id or scientific_name.", call. = FALSE)
  }
  .cc_required_cols(translations, c("language", "translated_name"), "translations")
  if (!"concept_id" %in% names(translations)) {
    translations <- merge(translations, lookup, by = "scientific_name", all.x = TRUE)
  }
  if (!"country" %in% names(translations)) translations$country <- "global"
  translations$country[is.na(translations$country) | !nzchar(as.character(translations$country))] <- "global"
  translations <- translations[!is.na(translations$concept_id) & !is.na(translations$translated_name) & nzchar(translations$translated_name), , drop = FALSE]
  add <- merge(lookup, translations, by = "concept_id", all.y = TRUE, suffixes = c(".lookup", ".translation"))
  if (!"scientific_name" %in% names(add)) {
    left <- if ("scientific_name.lookup" %in% names(add)) add$scientific_name.lookup else NA_character_
    right <- if ("scientific_name.translation" %in% names(add)) add$scientific_name.translation else NA_character_
    add$scientific_name <- ifelse(!is.na(left) & nzchar(as.character(left)), as.character(left), as.character(right))
  }
  add$query <- add$translated_name
  add$query_type <- "translated_common"
  add$ambiguity_score <- NA_real_
  add$ambiguity_class <- NA_character_
  add$disambiguation_rule <- NA_character_
  add$platform <- NA_character_
  keep <- c("concept_id", "scientific_name", "query", "query_type", "language", "country", "ambiguity_score", "ambiguity_class", "disambiguation_rule", "platform")
  add <- add[intersect(keep, names(add))]
  for (nm in setdiff(keep, names(add))) add[[nm]] <- NA
  add <- add[keep]
  out <- rbind(dictionary[keep], add)
  if (isTRUE(add_hashtags)) out <- expand_common_names(out, hashtags = TRUE)
  out <- unique(out)
  rownames(out) <- NULL
  class(out) <- c("culturomic_dictionary", setdiff(class(out), "culturomic_dictionary"))
  out
}

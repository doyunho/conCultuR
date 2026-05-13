.cc_default_frame_lexicon <- function() {
  list(
    extinction = c("extinct", "extinction", "endangered", "threatened", "red list", "critically endangered", "vulnerable", "decline", "risk"),
    protection = c("protect", "protection", "conserve", "conservation", "restore", "restoration", "recovery", "recover", "sanctuary", "reserve", "corridor", "anti poaching"),
    conflict = c("conflict", "damage", "crop", "livestock", "attack", "hunting", "cull", "controversy", "nuisance"),
    charisma = c("cute", "beautiful", "charismatic", "viral", "amazing", "iconic", "popular", "celebrated"),
    tourism = c("tourism", "tourist", "watching", "sighting", "safari", "diving", "ecotourism", "travel"),
    trade = c("trade", "trafficking", "pet", "market", "illegal", "poaching", "horn", "ivory"),
    habitat = c("habitat", "deforestation", "forest loss", "wetland", "nesting", "corridor", "land use", "fragmentation"),
    pollution = c("pollution", "plastic", "chemical", "oil spill", "contamination"),
    climate = c("climate", "warming", "heat", "drought", "sex ratio", "phenology"),
    welfare = c("captivity", "welfare", "aquarium", "zoo", "cruel", "debate"),
    policy = c("policy", "law", "regulation", "ban", "agreement", "campaign", "management", "community based"),
    disease = c("disease", "pathogen", "transmission", "infection", "virus", "fungus")
  )
}

#' Classify conservation narrative frames in text
#'
#' @param text Data frame or character vector.
#' @param text_col Text column.
#' @param concept_col Concept column.
#' @param frame_lexicon Optional named list of frame keywords.
#' @return Data frame with frame indicators and dominant frame.
#' @export
narrative_frame <- function(text, text_col = "text", concept_col = "concept_id", frame_lexicon = NULL) {
  if (is.character(text)) {
    dat <- data.frame(text = text, concept_id = NA_character_, stringsAsFactors = FALSE)
    text_col <- "text"
    concept_col <- "concept_id"
  } else {
    dat <- as.data.frame(text, stringsAsFactors = FALSE)
  }
  .cc_required_cols(dat, text_col, "text")
  if (!concept_col %in% names(dat)) dat[[concept_col]] <- NA_character_
  if (is.null(frame_lexicon)) frame_lexicon <- .cc_default_frame_lexicon()
  txt <- tolower(dat[[text_col]])
  out <- data.frame(
    row_id = seq_len(nrow(dat)),
    concept_id = dat[[concept_col]],
    stringsAsFactors = FALSE
  )
  for (frame in names(frame_lexicon)) {
    terms <- unique(tolower(frame_lexicon[[frame]]))
    terms <- gsub(" ", "\\\\s+", terms)
    pattern <- paste0("\\b(", paste(terms, collapse = "|"), ")\\b")
    hits <- regmatches(txt, gregexpr(pattern, txt, perl = TRUE))
    out[[paste0("frame_", frame)]] <- lengths(hits)
  }
  frame_cols <- grep("^frame_", names(out), value = TRUE)
  out$n_frame_hits <- rowSums(out[frame_cols], na.rm = TRUE)
  out$dominant_frame <- apply(out[frame_cols], 1, function(z) {
    if (all(z == 0)) return("unclassified")
    sub("^frame_", "", frame_cols[which.max(z)])
  })
  out$frame_confidence <- apply(out[frame_cols], 1, function(z) {
    if (all(z == 0)) return(0)
    max(z) / sum(z)
  })
  out
}

#' Extract narratives
#'
#' @param text Data frame or character vector.
#' @param text_col Text column.
#' @param concept_col Concept column.
#' @param frame_lexicon Optional named list of frame keywords.
#' @return Data frame with frame indicators and dominant frame.
#' @export
extract_narratives <- function(text, text_col = "text", concept_col = "concept_id", frame_lexicon = NULL) {
  narrative_frame(text = text, text_col = text_col, concept_col = concept_col, frame_lexicon = frame_lexicon)
}

.cc_default_sentiment_lexicon <- function() {
  list(
    positive = c("protect", "recovery", "recover", "success", "beautiful", "amazing", "restore", "restoration", "saved", "hope", "benefit", "celebrated", "support", "promote", "charismatic"),
    negative = c("decline", "loss", "threat", "threaten", "extinct", "extinction", "danger", "damage", "kill", "crisis", "illegal", "poaching", "trafficking", "pollution", "deforestation", "captivity", "controversy")
  )
}

#' Summarise sentiment by conservation concept
#'
#' @param text Data frame or character vector.
#' @param text_col Text column.
#' @param concept_col Concept column.
#' @param lexicon Optional list with positive and negative vectors.
#' @return Data frame with sentiment scores.
#' @export
sentiment_by_concept <- function(text, text_col = "text", concept_col = "concept_id", lexicon = NULL) {
  if (is.character(text)) {
    dat <- data.frame(text = text, concept_id = "all", stringsAsFactors = FALSE)
    text_col <- "text"
    concept_col <- "concept_id"
  } else {
    dat <- as.data.frame(text, stringsAsFactors = FALSE)
  }
  .cc_required_cols(dat, c(text_col, concept_col), "text")
  if (is.null(lexicon)) lexicon <- .cc_default_sentiment_lexicon()
  txt <- tolower(dat[[text_col]])
  pos_pattern <- paste0("\\b(", paste(tolower(lexicon$positive), collapse = "|"), ")\\b")
  neg_pattern <- paste0("\\b(", paste(tolower(lexicon$negative), collapse = "|"), ")\\b")
  dat$positive_hits <- lengths(regmatches(txt, gregexpr(pos_pattern, txt, perl = TRUE)))
  dat$negative_hits <- lengths(regmatches(txt, gregexpr(neg_pattern, txt, perl = TRUE)))
  dat$sentiment_balance <- dat$positive_hits - dat$negative_hits
  denom <- dat$positive_hits + dat$negative_hits
  dat$sentiment_index_row <- ifelse(denom > 0, dat$sentiment_balance / denom, 0)
  out <- stats::aggregate(dat[c("positive_hits", "negative_hits", "sentiment_balance", "sentiment_index_row")], dat[concept_col], mean, na.rm = TRUE)
  names(out)[1] <- "concept_id"
  names(out)[names(out) == "sentiment_balance"] <- "sentiment_score"
  names(out)[names(out) == "sentiment_index_row"] <- "sentiment_index"
  out$n_texts <- as.numeric(table(dat[[concept_col]])[out$concept_id])
  out
}

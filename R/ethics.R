#' Check ethics and governance risks for a culturomics workflow
#'
#' @param data Optional culturomic table or data frame. Used only to infer whether raw text or user-level identifiers are present when not supplied explicitly.
#' @param platforms Platforms used. If omitted and data has a platform column, platforms are inferred from data.
#' @param personal_data Whether user-level personal data are stored.
#' @param stores_text Whether raw text is stored.
#' @param vulnerable_groups Whether the analysis targets vulnerable groups.
#' @return Data frame of ethics checks.
#' @export
ethics_check <- function(data = NULL,
                         platforms = NULL,
                         personal_data = FALSE,
                         stores_text = FALSE,
                         vulnerable_groups = FALSE) {
  if (is.null(platforms)) {
    if (!is.null(data) && "platform" %in% names(as.data.frame(data))) {
      platforms <- unique(as.character(as.data.frame(data)$platform))
    } else {
      platforms <- character(0)
    }
  }

  if (!is.logical(personal_data) || length(personal_data) != 1L || is.na(personal_data)) {
    personal_data <- FALSE
  }
  if (!is.logical(stores_text) || length(stores_text) != 1L || is.na(stores_text)) {
    stores_text <- FALSE
  }
  if (!is.logical(vulnerable_groups) || length(vulnerable_groups) != 1L || is.na(vulnerable_groups)) {
    vulnerable_groups <- FALSE
  }

  if (!is.null(data)) {
    dat <- as.data.frame(data, stringsAsFactors = FALSE)
    user_cols <- intersect(c("user", "username", "author", "user_id", "account_id", "profile_id"), names(dat))
    text_cols <- intersect(c("text", "body", "comment", "post_text", "raw_text", "title", "description"), names(dat))
    if (length(user_cols) > 0L) personal_data <- TRUE
    if (length(text_cols) > 0L) stores_text <- TRUE
  }

  platform_label <- if (length(platforms) == 0L) "unspecified platforms" else paste(platforms, collapse = ", ")

  checks <- data.frame(
    check = c("platform_terms", "personal_data", "raw_text_storage", "vulnerable_groups", "aggregation", "query_ambiguity", "geographic_sensitivity"),
    status = c(
      "review",
      ifelse(isTRUE(personal_data), "high_risk", "low_risk"),
      ifelse(isTRUE(stores_text), "review", "low_risk"),
      ifelse(isTRUE(vulnerable_groups), "high_risk", "low_risk"),
      "recommended",
      "recommended",
      "review"
    ),
    recommendation = c(
      paste("Review current terms for", platform_label, "before publication or data release."),
      "Avoid user-level identifiers unless there is ethics approval and a clear public interest basis.",
      "Prefer aggregate counts or derived features over redistributing raw posts.",
      "Avoid targeting vulnerable communities, private locations, or small user groups without explicit safeguards.",
      "Report platform, query, date, geography, language, and collection time metadata.",
      "Publish query dictionaries, ambiguity scores, and query sensitivity results alongside findings.",
      "For sensitive species or illegal trade contexts, aggregate spatial results and avoid revealing precise localities."
    ),
    stringsAsFactors = FALSE
  )
  checks
}

#' Remove common user identifiers from social data
#'
#' @param x Data frame.
#' @param id_cols Columns to remove or hash.
#' @param method remove or hash.
#' @return Data frame.
#' @export
anonymise_social_data <- function(x, id_cols = c("user", "username", "author", "user_id"), method = c("remove", "hash")) {
  method <- match.arg(method)
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  cols <- intersect(id_cols, names(x))
  if (length(cols) == 0) return(x)
  if (method == "remove") {
    x[cols] <- NULL
  } else {
    for (col in cols) x[[col]] <- vapply(x[[col]], function(z) paste0("id_", abs(sum(utf8ToInt(as.character(z))))), character(1))
  }
  x
}

#' Create a platform terms report scaffold
#'
#' @param platforms Platforms used.
#' @return Data frame.
#' @export
platform_terms_report <- function(platforms) {
  template <- data.frame(
    platform = c("google", "google_trends", "wikipedia", "reddit", "youtube", "gdelt"),
    data_type = c("relative search interest", "relative search interest", "pageviews", "public posts/search", "video search metadata", "news/document metadata"),
    typical_redist_guidance = c(
      "share derived aggregate indices, not raw API payloads unless terms allow",
      "share derived aggregate indices, not raw API payloads unless terms allow",
      "Wikimedia REST outputs are generally suitable for citation with attribution, but verify current terms",
      "avoid redistributing raw posts; prefer aggregate counts and IDs only when allowed",
      "avoid redistributing raw video metadata unless API terms allow; prefer aggregate summaries",
      "share derived news volume summaries and cite GDELT where applicable"
    ),
    terms_checked = FALSE,
    redistribution_allowed = NA,
    notes = "Complete current terms check before publication or data release.",
    stringsAsFactors = FALSE
  )
  out <- template[match(platforms, template$platform), , drop = FALSE]
  missing <- is.na(out$platform)
  if (any(missing)) {
    out[missing, ] <- data.frame(
      platform = platforms[missing],
      data_type = NA_character_,
      typical_redist_guidance = "unknown platform; complete manual terms review",
      terms_checked = FALSE,
      redistribution_allowed = NA,
      notes = "Complete current terms check before publication or data release.",
      stringsAsFactors = FALSE
    )
  }
  rownames(out) <- NULL
  out
}

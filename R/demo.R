#' Example conservation culturomics data
#'
#' Creates a richer demonstration dataset for package examples. The values are
#' illustrative and are not intended to be authoritative conservation statuses.
#'
#' @return A list with taxa, conservation_status, translations, texts, events,
#' and attention_profile tables.
#' @export
con_culturomics_example_data <- function() {
  taxa <- data.frame(
    concept_id = c(
      "vaquita", "saola", "black_rhino", "hawksbill_turtle",
      "giant_panda", "tiger", "pangolin", "kakapo",
      "snow_leopard", "red_panda", "axolotl", "monarch_butterfly"
    ),
    scientific_name = c(
      "Phocoena sinus", "Pseudoryx nghetinhensis", "Diceros bicornis", "Eretmochelys imbricata",
      "Ailuropoda melanoleuca", "Panthera tigris", "Manis javanica", "Strigops habroptilus",
      "Panthera uncia", "Ailurus fulgens", "Ambystoma mexicanum", "Danaus plexippus"
    ),
    common_name = c(
      "vaquita", "saola", "black rhino", "hawksbill turtle",
      "giant panda", "tiger", "pangolin", "kakapo",
      "snow leopard", "red panda", "axolotl", "monarch butterfly"
    ),
    stringsAsFactors = FALSE
  )

  conservation_status <- data.frame(
    concept_id = taxa$concept_id,
    scientific_name = taxa$scientific_name,
    status = c("CR", "CR", "CR", "CR", "VU", "EN", "CR", "CR", "VU", "EN", "CR", "EN"),
    body_mass_kg = c(43, 90, 900, 80, 100, 180, 7, 2.1, 38, 4.5, 0.08, 0.0005),
    range_size_km2 = c(4000, 50000, 25000, 25000000, 30000, 1000000, 500000, 10000, 1800000, 150000, 400, 6000000),
    charisma_score = c(0.45, 0.25, 0.88, 0.70, 0.98, 0.97, 0.60, 0.70, 0.92, 0.89, 0.93, 0.72),
    communication_priority = c("neglected", "neglected", "trade_crisis", "marine_campaign", "charismatic", "charismatic", "trade_crisis", "recovery_story", "charismatic", "charismatic", "internet_popular", "citizen_science"),
    stringsAsFactors = FALSE
  )

  translations <- data.frame(
    concept_id = rep(taxa$concept_id, each = 2),
    scientific_name = rep(taxa$scientific_name, each = 2),
    language = rep(c("ko", "es"), times = nrow(taxa)),
    translated_name = c(
      "vaquita-ko", "vaquita",
      "saola-ko", "saola",
      "black rhino-ko", "rinoceronte negro",
      "hawksbill turtle-ko", "tortuga carey",
      "giant panda-ko", "panda gigante",
      "tiger-ko", "tigre",
      "pangolin-ko", "pangolin",
      "kakapo-ko", "kakapo",
      "snow leopard-ko", "leopardo de las nieves",
      "red panda-ko", "panda rojo",
      "axolotl-ko", "ajolote",
      "monarch butterfly-ko", "mariposa monarca"
    ),
    stringsAsFactors = FALSE
  )


  synonyms <- data.frame(
    concept_id = c(
      "giant_panda", "red_panda", "tiger", "monarch_butterfly",
      "black_rhino", "hawksbill_turtle", "pangolin", "snow_leopard",
      "axolotl", "vaquita"
    ),
    query = c(
      "panda", "panda", "tiger", "monarch",
      "rhino", "turtle", "scaly anteater", "leopard",
      "mexican walking fish", "porpoise"
    ),
    query_type = "synonym",
    language = "en",
    country = "global",
    stringsAsFactors = FALSE
  )

  attention_profile <- data.frame(
    concept_id = taxa$concept_id,
    attention_base = c(16, 8, 36, 42, 86, 92, 34, 28, 55, 62, 74, 48),
    seasonal_amplitude = c(8, 5, 7, 20, 9, 10, 12, 10, 14, 10, 18, 26),
    peak_month = c(7, 5, 9, 6, 3, 7, 2, 4, 12, 2, 10, 9),
    ambiguity_noise = c(0.05, 0.05, 0.12, 0.10, 0.18, 0.28, 0.10, 0.08, 0.20, 0.18, 0.15, 0.10),
    campaign_lift = c(42, 18, 14, 30, 8, 8, 20, 24, 10, 8, 18, 24),
    campaign_decay_days = c(55, 35, 30, 60, 25, 25, 45, 45, 30, 30, 40, 60),
    event_month = c(7, 5, 9, 6, 3, 7, 2, 4, 12, 2, 10, 9),
    stringsAsFactors = FALSE
  )

  texts <- data.frame(
    concept_id = rep(taxa$concept_id, each = 4),
    scientific_name = rep(taxa$scientific_name, each = 4),
    text = c(
      "Vaquita extinction risk is linked to bycatch and urgent protection campaigns",
      "Rare vaquita sighting creates a short burst of public attention",
      "Policy debate focuses on fishing regulation and enforcement in the Gulf of California",
      "Conservation groups call for emergency action to save the vaquita",

      "Saola is called the Asian unicorn but remains almost invisible in public searches",
      "Habitat protection and forest patrols are central to saola recovery",
      "Low media coverage makes saola an example of a neglected threatened species",
      "International conservation organisations promote awareness for saola habitat",

      "Black rhino poaching and horn trafficking remain major conservation threats",
      "Anti poaching campaigns promote protection and community based conservation",
      "Rhino recovery stories receive positive news coverage in some reserves",
      "Illegal wildlife trade frames black rhino attention around crisis and enforcement",

      "Hawksbill turtle nesting beaches need protection from tourism pressure",
      "Plastic pollution and climate change threaten marine turtle recovery",
      "World Ocean Day campaign increases turtle conservation attention",
      "Illegal shell trade remains a continuing risk for hawksbill turtles",

      "Giant panda conservation success is celebrated worldwide",
      "Cute panda videos are popular on social media",
      "Panda diplomacy receives international news coverage",
      "Habitat restoration supports panda recovery",

      "Tiger conservation campaigns are iconic but can crowd out lesser known species",
      "Illegal trade and habitat loss threaten tiger populations",
      "Tiger tourism creates both benefits and conflict around protected areas",
      "Viral tiger videos increase public attention even outside conservation contexts",

      "Pangolin trafficking and illegal wildlife trade receive increasing media coverage",
      "Pet and market demand create concern about pangolin conservation",
      "Protection campaigns link pangolins to zoonotic disease narratives",
      "Habitat loss and poaching threaten pangolin recovery",

      "Kakapo recovery is often described as a conservation success story",
      "Community support and intensive management help protect kakapo",
      "Cute kakapo videos become viral and generate charismatic attention",
      "Disease and low genetic diversity remain conservation concerns",

      "Snow leopard conflict with livestock creates challenges for conservation",
      "Beautiful snow leopard images drive tourism and charismatic attention",
      "Community based conservation can reduce conflict and support protection",
      "Climate warming threatens mountain habitat for snow leopards",

      "Red panda videos are popular and often framed as cute wildlife content",
      "Habitat fragmentation and forest loss threaten red panda recovery",
      "Tourism and education campaigns promote red panda conservation",
      "Illegal pet trade appears in some red panda narratives",

      "Axolotl became internet famous while wild populations remain threatened",
      "Urban pollution and habitat loss threaten axolotl survival",
      "Captive pet trade creates a gap between popularity and conservation awareness",
      "Education campaigns use axolotl charisma to discuss wetland restoration",

      "Monarch butterfly migration is a visible seasonal conservation event",
      "Citizen science campaigns promote monarch monitoring and habitat restoration",
      "Climate change and pesticide concerns shape monarch butterfly narratives",
      "Public attention peaks during migration and World Wildlife Day campaigns"
    ),
    stringsAsFactors = FALSE
  )

  events <- data.frame(
    event_id = c("world_wildlife_day", "world_ocean_day", "species_action_week"),
    event_name = c("World Wildlife Day", "World Ocean Day", "Species Action Week"),
    event_date = as.Date(c("2024-03-03", "2024-06-08", "2024-07-15")),
    target_concepts = c("tiger;pangolin;giant_panda", "vaquita;hawksbill_turtle", "vaquita;saola;black_rhino"),
    stringsAsFactors = FALSE
  )

  dictionary <- make_concept_dictionary(
    taxa,
    concept_id = "concept_id",
    languages = "en",
    countries = "global"
  )
  dictionary <- expand_common_names(
    dictionary,
    synonyms = synonyms,
    hashtags = TRUE
  )
  dictionary <- translate_concepts(
    dictionary,
    translations = translations,
    add_hashtags = TRUE
  )
  dictionary <- score_ambiguity(dictionary)

  list(
    taxa = taxa,
    dictionary = dictionary,
    conservation_status = conservation_status,
    translations = translations,
    synonyms = synonyms,
    attention_profile = attention_profile,
    texts = texts,
    events = events
  )
}

.cc_metric_for_platform <- function(platform) {
  switch(platform,
    google = "relative_search_interest",
    google_trends = "relative_search_interest",
    wikipedia = "pageviews",
    wiki = "pageviews",
    reddit = "mentions",
    youtube = "video_search_hits",
    gdelt = "news_volume",
    platform
  )
}

.cc_platform_label <- function(platform) {
  switch(platform,
    google = "google_trends",
    google_trends = "google_trends",
    wiki = "wikipedia",
    wikipedia = "wikipedia",
    reddit = "reddit",
    youtube = "youtube",
    gdelt = "gdelt",
    platform
  )
}

#' Simulate scenario-based digital attention traces
#'
#' The simulator is designed for demonstrations and vignettes. It creates
#' interpretable platform-specific digital traces with seasonality, campaign
#' pulses, query-type effects, and ambiguity-related non-biological noise.
#'
#' @param dictionary Concept dictionary.
#' @param from Start date.
#' @param to End date.
#' @param platforms Platforms to simulate.
#' @param profile Optional table with concept_id, attention_base,
#' seasonal_amplitude, peak_month, campaign_lift, and campaign_decay_days.
#' @param campaign_date Optional date for a campaign pulse.
#' @param campaign_concepts Optional concept identifiers receiving campaign pulse.
#' @param seed Random seed.
#' @return culturomic_tbl.
#' @export
simulate_culturomic_traces <- function(dictionary,
                                       from,
                                       to,
                                       platforms = c("google", "wikipedia", "reddit", "youtube", "gdelt"),
                                       profile = NULL,
                                       campaign_date = NULL,
                                       campaign_concepts = NULL,
                                       seed = 1) {
  dictionary <- as.data.frame(dictionary, stringsAsFactors = FALSE)
  .cc_required_cols(dictionary, c("concept_id", "query", "query_type", "language", "country"), "dictionary")
  if (!"ambiguity_score" %in% names(dictionary)) dictionary$ambiguity_score <- 0
  dates <- .cc_date_sequence(from, to, "day")
  if (is.null(profile)) {
    ids <- unique(dictionary$concept_id)
    profile <- data.frame(
      concept_id = ids,
      attention_base = seq(20, 70, length.out = length(ids)),
      seasonal_amplitude = 10,
      peak_month = rep(c(3, 6, 9, 12), length.out = length(ids)),
      ambiguity_noise = 0.10,
      campaign_lift = 20,
      campaign_decay_days = 45,
      stringsAsFactors = FALSE
    )
  }
  profile <- as.data.frame(profile, stringsAsFactors = FALSE)
  .cc_required_cols(profile, c("concept_id", "attention_base"), "profile")
  if (!"seasonal_amplitude" %in% names(profile)) profile$seasonal_amplitude <- 10
  if (!"peak_month" %in% names(profile)) profile$peak_month <- 6
  if (!"ambiguity_noise" %in% names(profile)) profile$ambiguity_noise <- 0.1
  if (!"campaign_lift" %in% names(profile)) profile$campaign_lift <- 20
  if (!"campaign_decay_days" %in% names(profile)) profile$campaign_decay_days <- 45

  if (is.null(campaign_date)) campaign_date <- as.Date(NA)
  campaign_date <- .cc_as_date(campaign_date)
  if (is.null(campaign_concepts)) campaign_concepts <- character(0)
  platform_mult <- c(google_trends = 1.00, wikipedia = 0.85, reddit = 0.55, youtube = 1.25, gdelt = 0.70)
  language_mult <- c(en = 1.00, ko = 0.36, es = 0.52)
  query_mult <- c(scientific = 0.42, common = 1.00, translated_common = 0.70, synonym = 0.78, hashtag = 0.95)

  set.seed(seed)
  rows <- list()
  k <- 1
  for (p in platforms) {
    pp <- .cc_platform_label(p)
    metric <- .cc_metric_for_platform(p)
    pmult <- if (pp %in% names(platform_mult)) platform_mult[[pp]] else 1
    for (i in seq_len(nrow(dictionary))) {
      drow <- dictionary[i, , drop = FALSE]
      prof <- profile[match(drow$concept_id, profile$concept_id), , drop = FALSE]
      if (nrow(prof) == 0 || is.na(prof$attention_base[1])) next
      n <- length(dates)
      months <- as.integer(format(dates, "%m"))
      base <- as.numeric(prof$attention_base[1])
      amp <- as.numeric(prof$seasonal_amplitude[1])
      peak <- as.numeric(prof$peak_month[1])
      season <- amp * cos(2 * pi * (months - peak) / 12)
      long_trend <- seq(0, base * 0.15, length.out = n)
      qtype <- as.character(drow$query_type)
      qmult <- if (qtype %in% names(query_mult)) query_mult[[qtype]] else 0.75
      lang <- as.character(drow$language)
      lmult <- if (lang %in% names(language_mult)) language_mult[[lang]] else 0.40
      ambiguity <- suppressWarnings(as.numeric(drow$ambiguity_score))
      if (!is.finite(ambiguity)) ambiguity <- 0
      amb_noise <- as.numeric(prof$ambiguity_noise[1]) * ambiguity * base
      if (pp %in% c("google_trends", "youtube", "reddit")) amb_noise <- amb_noise * 1.8
      if (qtype == "scientific") amb_noise <- amb_noise * 0.2
      campaign <- rep(0, n)
      if (!is.na(campaign_date) && drow$concept_id %in% campaign_concepts) {
        days_after <- as.numeric(dates - campaign_date)
        active <- days_after >= 0
        campaign[active] <- as.numeric(prof$campaign_lift[1]) * exp(-days_after[active] / as.numeric(prof$campaign_decay_days[1]))
      }
      event <- rep(0, n)
      if ("event_month" %in% names(prof) && is.finite(as.numeric(prof$event_month[1]))) {
        event <- ifelse(months == as.numeric(prof$event_month[1]), amp * 0.75, 0)
      }
      noise_sd <- 4 + sqrt(base) * 0.30
      raw <- base * pmult * qmult * lmult + season * pmult * lmult + long_trend * lmult + campaign * pmult + event * pmult + amb_noise + stats::rnorm(n, 0, noise_sd)
      raw <- pmax(0, raw)
      rows[[k]] <- data.frame(
        date = dates,
        country = ifelse(is.na(drow$country) | !nzchar(as.character(drow$country)), "global", as.character(drow$country)),
        language = drow$language,
        platform = pp,
        concept_id = drow$concept_id,
        query = drow$query,
        query_type = drow$query_type,
        ambiguity_score = ambiguity,
        metric = metric,
        raw_value = raw,
        value = raw,
        source = "scenario_mock",
        collection_time = as.character(Sys.time()),
        stringsAsFactors = FALSE
      )
      k <- k + 1
    }
  }
  as_culturomic_tbl(.cc_bind_rows_fill(rows), dictionary = dictionary)
}

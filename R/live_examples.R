#' Real-data starter set for conservation culturomics
#'
#' This helper returns a small platform-specific dictionary and conservation
#' status table that can be used for live Wikipedia and GDELT examples. The
#' entries are intentionally conservative and use well-known English Wikipedia
#' page titles and quoted GDELT phrases.
#'
#' @return A list with dictionary, conservation_status, events, and notes.
#' @export
con_culturomics_real_example_data <- function() {
  taxa <- data.frame(
    concept_id = c("vaquita", "saola", "black_rhino", "hawksbill_turtle", "giant_panda", "tiger"),
    scientific_name = c("Phocoena sinus", "Pseudoryx nghetinhensis", "Diceros bicornis", "Eretmochelys imbricata", "Ailuropoda melanoleuca", "Panthera tigris"),
    common_name = c("vaquita", "saola", "black rhino", "hawksbill turtle", "giant panda", "tiger"),
    status = c("CR", "CR", "CR", "CR", "VU", "EN"),
    wiki_title = c("Vaquita", "Saola", "Black rhinoceros", "Hawksbill sea turtle", "Giant panda", "Tiger"),
    gdelt_query = c('"vaquita"', '"saola"', '("black rhino" OR "black rhinoceros")', '("hawksbill turtle" OR "hawksbill sea turtle")', '"giant panda"', '"tiger conservation"'),
    google_query = c("vaquita", "saola", "black rhino", "hawksbill turtle", "giant panda", "tiger conservation"),
    stringsAsFactors = FALSE
  )

  wiki <- data.frame(
    concept_id = taxa$concept_id,
    scientific_name = taxa$scientific_name,
    common_name = taxa$common_name,
    query = taxa$wiki_title,
    query_type = "wikipedia_title",
    language = "en",
    country = "global",
    platform = "wikipedia",
    wiki_project = "en.wikipedia.org",
    ambiguity_score = 0,
    ambiguity_class = "low",
    ambiguity_reason = "curated English Wikipedia article title",
    disambiguation_rule = "curated title",
    stringsAsFactors = FALSE
  )
  gdelt <- data.frame(
    concept_id = taxa$concept_id,
    scientific_name = taxa$scientific_name,
    common_name = taxa$common_name,
    query = taxa$gdelt_query,
    query_type = "gdelt_query",
    language = "en",
    country = "global",
    platform = "gdelt",
    wiki_project = NA_character_,
    ambiguity_score = c(0.05, 0.05, 0.10, 0.10, 0.12, 0.35),
    ambiguity_class = c("low", "low", "low", "low", "low", "medium"),
    ambiguity_reason = c("quoted species common name", "quoted species common name", "quoted conservation phrase", "quoted conservation phrase", "quoted common name", "uses conservation qualifier to reduce ambiguity"),
    disambiguation_rule = "quoted phrase or conservation qualifier",
    stringsAsFactors = FALSE
  )
  google <- data.frame(
    concept_id = taxa$concept_id,
    scientific_name = taxa$scientific_name,
    common_name = taxa$common_name,
    query = taxa$google_query,
    query_type = "google_query",
    language = "en",
    country = "global",
    platform = "google_trends",
    wiki_project = NA_character_,
    ambiguity_score = c(0.05, 0.05, 0.12, 0.12, 0.25, 0.35),
    ambiguity_class = c("low", "low", "low", "low", "medium", "medium"),
    ambiguity_reason = "curated Google Trends query",
    disambiguation_rule = "curated search phrase",
    stringsAsFactors = FALSE
  )
  dictionary <- rbind(wiki, gdelt, google)
  class(dictionary) <- c("culturomic_dictionary", class(dictionary))

  conservation_status <- taxa[c("concept_id", "scientific_name", "common_name", "status")]
  events <- data.frame(
    event_id = c("world_wildlife_day", "world_ocean_day"),
    event_name = c("World Wildlife Day", "World Ocean Day"),
    event_month_day = c("03-03", "06-08"),
    target_concepts = c("tiger;black_rhino;giant_panda", "vaquita;hawksbill_turtle"),
    stringsAsFactors = FALSE
  )
  notes <- data.frame(
    item = c("wikipedia", "gdelt", "google_trends", "date_window"),
    note = c(
      "Uses Wikimedia Pageviews API with curated English article titles.",
      "Uses GDELT DOC API timelinevolraw with recent date windows. Older windows may return no data.",
      "Google Trends is optional and requires gtrendsR. It can be unstable because Google Trends is relative and rate limited.",
      "The live demo chooses a recent window automatically so that GDELT can return data."
    ),
    stringsAsFactors = FALSE
  )
  list(dictionary = dictionary, conservation_status = conservation_status, events = events, notes = notes)
}

# Methods guide

This guide describes the methodological logic of `conCultuR` for use in manuscripts.

## 1. Concept dictionary

A concept is the unit of interpretation. It can be a species, taxon group, habitat, protected area, campaign, or conservation issue. Each concept can have multiple platform-specific queries.

Recommended reporting:

- scientific name or formal concept label
- common names and synonyms
- language and country scope
- platform-specific query strings
- query type, such as `wikipedia_title`, `gdelt_query`, `common`, `scientific`, or `hashtag`

## 2. Query ambiguity

Common names and hashtags may refer to non-biological topics. `conCultuR` scores this risk as `ambiguity_score` and classifies queries into usability categories.

Recommended reporting:

- number of queries per concept
- number of high-ambiguity queries
- whether high-ambiguity queries were excluded, downweighted, or retained with sensitivity checks

## 3. Platform traces

The package can analyse simulated traces or live traces from supported platforms. Different platforms measure different behaviours.

- Wikimedia Pageviews: information seeking
- Google Trends: search interest
- GDELT: news/media coverage
- Reddit/YouTube modules: public discussion or video-oriented attention where available

Do not treat these as equivalent raw counts.

## 4. Standardisation

Platform-specific raw values are transformed before aggregation. The recommended default is `log1p` transformation followed by robust z-score scaling. This reduces the influence of heavy-tailed digital traces and sudden spikes.

## 5. Attention index

The attention index is a concept-level signal. Queries are aggregated with query-type and ambiguity-aware weights, then platform-level signals are combined. Always report platform coverage and platform disagreement.

## 6. Attention gap

The attention gap compares attention percentile and threat-status percentile.

A positive gap indicates that a concept has lower public attention than expected from its threat rank. This is a communication signal, not a conservation-priority score.

Recommended reporting:

- attention percentile
- threat percentile
- attention-gap score
- strict class
- near-threshold status
- threshold sensitivity fraction

## 7. Campaign diagnostic

Campaign diagnostics estimate whether a treated concept deviates from a counterfactual attention trajectory after a campaign or media event. The result should be described as a campaign-associated deviation unless the study design justifies a causal claim.

Recommended reporting:

- campaign date
- treated concept and control pool
- pre- and post-window lengths
- average post-event effect
- standardised post-event effect
- placebo p-value or percentile

## 8. Interpretation limits

Digital attention may reflect language, media infrastructure, internet penetration, platform policy, automated traffic, cultural salience, or current events. It should never be interpreted as direct evidence of ecological condition.

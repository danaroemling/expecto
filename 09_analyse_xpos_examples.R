# ============================================================
# COMPLETE LEXICAL ANALYSIS OF SELECTED XPOS N-GRAMS
#
# Uses all existing top-10 author-feature pairs.
# No new feature selection or filtering.
# Includes punctuation and missing-tag placeholder XY.
# Training data only.
#
# Outputs:
# 01_pair_statistics.csv
# 02_story_statistics.csv
# 03_lexical_realisations.csv
# 04_lexical_story_frequencies.csv
# 05_concordance_examples.csv
# 06_summary_by_group.csv
# 07_slide_statistics.csv
# 08_validation.csv
# ============================================================

library(data.table)

BASE <- paste0(
  "PATH/Misc/FF_Corpus/",
  "HP_Liebesgeschichte_1000word_chunks_FINAL/POS_tagged"
)

EXP_DIR <- file.path(BASE, "experiment_xpos")
PREF_DIR <- file.path(EXP_DIR, "xpos_preferences")
OUT_DIR <- file.path(EXP_DIR, "xpos_complete_analysis")

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

INPUTS <- c(
  file.path(PREF_DIR, "author_feature_top10_n2_df3.csv"),
  file.path(PREF_DIR, "author_feature_top10_n3_df5.csv"),
  file.path(BASE, "tagged_tokens.rds")
)

if (!all(file.exists(INPUTS))) {
  stop(
    "Missing input files:\n",
    paste(INPUTS[!file.exists(INPUTS)], collapse = "\n")
  )
}

cat("Loading existing preferences...\n")

prefs <- rbindlist(
  list(
    fread(INPUTS[1]),
    fread(INPUTS[2])
  ),
  use.names = TRUE
)

stopifnot(
  nrow(prefs) == 5040L,
  uniqueN(prefs$author_id) == 252L,
  nrow(prefs[ngram == 2L]) == 2520L,
  nrow(prefs[ngram == 3L]) == 2520L,
  !anyDuplicated(prefs[, .(author_id, ngram, feature)])
)

prefs[, pair_id := sprintf("P%04d", .I)]
prefs[, has_punctuation :=
        grepl("(^|__)\\$", feature)]

# Create author-specific feature lookup tables.
lookup <- split(
  prefs[, .(pair_id, author_id, ngram, feature)],
  by = "author_id",
  keep.by = FALSE
)

cat("Loading tagged tokens...\n")

tokens <- as.data.table(readRDS(INPUTS[3]))

required <- c(
  "author_id", "set", "text_id",
  "final_filename", "token_position",
  "token", "xpos"
)

if (length(setdiff(required, names(tokens))) > 0L) {
  stop("Tagged corpus is missing required columns.")
}

tokens <- tokens[
  set == "train" &
    author_id %in% prefs$author_id
]

setorder(tokens, author_id, final_filename, token_position)

# Match the placeholder used for missing XPOS tags.
tokens[
  is.na(xpos) | xpos == "",
  xpos := "XY"
]

# Each chunk is processed independently.
# N-grams never cross chunk boundaries.
chunk_indices <- split(
  seq_len(nrow(tokens)),
  tokens$final_filename
)

occurrence_parts <- vector("list", length(chunk_indices))
story_denominator_parts <- vector("list", length(chunk_indices))

cat("Extracting all selected n-gram occurrences...\n")

counter <- 0L

for (idx in chunk_indices) {
  
  counter <- counter + 1L
  
  chunk <- tokens[idx]
  
  author <- chunk$author_id[1]
  story <- chunk$text_id[1]
  filename <- chunk$final_filename[1]
  
  selected <- lookup[[author]]
  
  found_parts <- list()
  denominator_parts <- list()
  
  for (n in c(2L, 3L)) {
    
    if (nrow(chunk) < n) next
    
    starts <- seq_len(nrow(chunk) - n + 1L)
    
    tag_columns <- lapply(
      seq_len(n),
      function(offset) {
        chunk$xpos[starts + offset - 1L]
      }
    )
    
    feature_strings <- do.call(
      paste,
      c(tag_columns, sep = "__")
    )
    
    denominator_parts[[as.character(n)]] <- data.table(
      author_id = author,
      text_id = story,
      ngram = n,
      available_ngrams = length(starts)
    )
    
    selected_n <- selected[ngram == n]
    
    matched <- match(
      feature_strings,
      selected_n$feature
    )
    
    hits <- which(!is.na(matched))
    
    if (length(hits) == 0L) next
    
    lexical <- vapply(
      hits,
      function(i) {
        paste(
          chunk$token[i:(i + n - 1L)],
          collapse = " "
        )
      },
      character(1)
    )
    
    left <- vapply(
      hits,
      function(i) {
        from <- max(1L, i - 8L)
        if (i == 1L) return("")
        paste(chunk$token[from:(i - 1L)], collapse = " ")
      },
      character(1)
    )
    
    right <- vapply(
      hits,
      function(i) {
        from <- i + n
        to <- min(nrow(chunk), from + 7L)
        if (from > nrow(chunk)) return("")
        paste(chunk$token[from:to], collapse = " ")
      },
      character(1)
    )
    
    found_parts[[as.character(n)]] <- data.table(
      pair_id = selected_n$pair_id[matched[hits]],
      author_id = author,
      text_id = story,
      final_filename = filename,
      token_position = chunk$token_position[hits],
      ngram = n,
      feature = feature_strings[hits],
      lexical_realisation = lexical,
      left_context = left,
      right_context = right
    )
  }
  
  occurrence_parts[[counter]] <- rbindlist(
    found_parts,
    use.names = TRUE,
    fill = TRUE
  )
  
  story_denominator_parts[[counter]] <- rbindlist(
    denominator_parts,
    use.names = TRUE
  )
  
  if (counter %% 500L == 0L) {
    cat("Processed", counter, "chunks\n")
  }
}

occ <- rbindlist(occurrence_parts, use.names = TRUE)
denoms <- rbindlist(story_denominator_parts)

rm(occurrence_parts, story_denominator_parts, tokens)
gc()

# ============================================================
# VALIDATE AGAINST ORIGINAL FEATURE COUNTS
# ============================================================

cat("Validating extracted counts...\n")

counts <- occ[, .(extracted_count = .N), by = pair_id]

validation <- merge(
  prefs[, .(pair_id, author_id, ngram,
            feature, expected_count = author_count)],
  counts,
  by = "pair_id",
  all.x = TRUE
)

validation[
  is.na(extracted_count),
  extracted_count := 0L
]

validation[, matches := expected_count == extracted_count]

fwrite(
  validation,
  file.path(OUT_DIR, "08_validation.csv")
)

if (!all(validation$matches)) {
  stop(
    "Extraction counts do not match the original analysis. ",
    "Inspect 08_validation.csv. No summary statistics written."
  )
}

cat("All 5,040 feature counts validated.\n")

# ============================================================
# STORY-LEVEL FREQUENCIES
# ============================================================

denoms <- denoms[
  ,
  .(available_ngrams = sum(available_ngrams)),
  by = .(author_id, text_id, ngram)
]

story_counts <- occ[
  ,
  .(count = .N),
  by = .(pair_id, author_id, text_id, ngram)
]

# Include stories in which the feature does not occur.
story_grid <- merge(
  prefs[, .(pair_id, author_id, ngram)],
  denoms,
  by = c("author_id", "ngram"),
  allow.cartesian = TRUE
)

story_stats <- merge(
  story_grid,
  story_counts,
  by = c("pair_id", "author_id", "text_id", "ngram"),
  all.x = TRUE
)

story_stats[is.na(count), count := 0L]

story_stats[
  ,
  rate_per_10000 :=
    10000 * count / available_ngrams
]

fwrite(
  story_stats,
  file.path(OUT_DIR, "02_story_statistics.csv")
)

# ============================================================
# LEXICAL REALISATIONS
# ============================================================

lexical <- occ[
  ,
  .(
    frequency = .N,
    stories_with_realisation = uniqueN(text_id)
  ),
  by = .(
    pair_id, author_id, ngram,
    feature, lexical_realisation
  )
]

setorder(lexical, pair_id, -frequency)

fwrite(
  lexical,
  file.path(OUT_DIR, "03_lexical_realisations.csv")
)

lexical_stories <- occ[
  ,
  .(frequency = .N),
  by = .(
    pair_id, author_id, text_id,
    ngram, feature, lexical_realisation
  )
]

fwrite(
  lexical_stories,
  file.path(OUT_DIR, "04_lexical_story_frequencies.csv")
)

# ============================================================
# PAIR-LEVEL STATISTICS
# ============================================================

lexical_summary <- lexical[
  ,
  .(
    distinct_realisations = .N,
    most_common_frequency = max(frequency),
    most_common_realisation =
      lexical_realisation[which.max(frequency)],
    realisations_in_multiple_stories =
      sum(stories_with_realisation >= 2L),
    occurrences_in_repeated_realisations =
      sum(frequency[stories_with_realisation >= 2L])
  ),
  by = pair_id
]

story_summary <- story_stats[
  ,
  .(
    observed_stories = sum(count > 0L),
    total_stories = .N,
    mean_story_rate = mean(rate_per_10000),
    sd_story_rate = sd(rate_per_10000),
    min_story_rate = min(rate_per_10000),
    max_story_rate = max(rate_per_10000)
  ),
  by = pair_id
]

pair_stats <- merge(
  prefs,
  lexical_summary,
  by = "pair_id"
)

pair_stats <- merge(
  pair_stats,
  story_summary,
  by = "pair_id"
)

pair_stats[
  ,
  dominant_realisation_share :=
    most_common_frequency / author_count
]

pair_stats[
  ,
  lexical_type_token_ratio :=
    distinct_realisations / author_count
]

pair_stats[
  ,
  repeated_realisation_occurrence_share :=
    occurrences_in_repeated_realisations / author_count
]

pair_stats[
  ,
  lexical_repetition_across_stories :=
    realisations_in_multiple_stories > 0L
]

pair_stats[
  ,
  story_coverage := observed_stories / total_stories
]

pair_stats[
  ,
  story_rate_cv := fifelse(
    !is.na(sd_story_rate) & mean_story_rate > 0,
    sd_story_rate / mean_story_rate,
    NA_real_
  )
]

fwrite(
  pair_stats,
  file.path(OUT_DIR, "01_pair_statistics.csv")
)

# ============================================================
# CONCORDANCES
#
# Keep a bounded set for manual inspection.
# All occurrences remain counted in the statistics.
# ============================================================

setorder(
  occ,
  pair_id,
  text_id,
  final_filename,
  token_position
)

occ[
  ,
  example_number := seq_len(.N),
  by = .(pair_id, text_id, lexical_realisation)
]

examples <- occ[example_number <= 3L]

examples[, example_number := NULL]

fwrite(
  examples,
  file.path(OUT_DIR, "05_concordance_examples.csv")
)

# ============================================================
# GROUP SUMMARIES
# ============================================================

pair_stats[
  ,
  group := fifelse(
    has_punctuation,
    "With punctuation",
    "Without punctuation"
  )
]

summary_groups <- pair_stats[
  ,
  .(
    pairs = .N,
    authors = uniqueN(author_id),
    
    median_story_coverage = median(story_coverage),
    pct_all_stories =
      100 * mean(story_coverage == 1),
    
    median_story_rate_cv =
      median(story_rate_cv, na.rm = TRUE),
    
    median_distinct_realisations =
      median(distinct_realisations),
    
    median_dominant_share =
      median(dominant_realisation_share),
    
    pct_dominant_50 =
      100 * mean(dominant_realisation_share >= 0.50),
    
    pct_dominant_75 =
      100 * mean(dominant_realisation_share >= 0.75),
    
    pct_dominant_90 =
      100 * mean(dominant_realisation_share >= 0.90),
    
    pct_any_lexical_repetition_across_stories =
      100 * mean(lexical_repetition_across_stories),
    
    median_repeated_occurrence_share =
      median(repeated_realisation_occurrence_share)
  ),
  by = .(ngram, group)
]

fwrite(
  summary_groups,
  file.path(OUT_DIR, "06_summary_by_group.csv")
)

slide_stats <- pair_stats[
  ,
  .(
    pairs = .N,
    authors = uniqueN(author_id),
    
    pct_all_stories =
      100 * mean(story_coverage == 1),
    
    median_story_rate_cv =
      median(story_rate_cv, na.rm = TRUE),
    
    q1_story_rate_cv =
      quantile(story_rate_cv, 0.25, na.rm = TRUE),
    
    q3_story_rate_cv =
      quantile(story_rate_cv, 0.75, na.rm = TRUE),
    
    median_distinct_realisations =
      median(distinct_realisations),
    
    q1_distinct_realisations =
      quantile(distinct_realisations, 0.25),
    
    q3_distinct_realisations =
      quantile(distinct_realisations, 0.75),
    
    median_dominant_share =
      median(dominant_realisation_share),
    
    pct_dominant_50 =
      100 * mean(dominant_realisation_share >= 0.50),
    
    pct_dominant_75 =
      100 * mean(dominant_realisation_share >= 0.75),
    
    pct_dominant_90 =
      100 * mean(dominant_realisation_share >= 0.90),
    
    pct_lexical_repetition_across_stories =
      100 * mean(lexical_repetition_across_stories),
    
    median_repeated_occurrence_share =
      median(repeated_realisation_occurrence_share)
  ),
  by = ngram
]

fwrite(
  slide_stats,
  file.path(OUT_DIR, "07_slide_statistics.csv")
)

cat("\n========================================\n")
cat("COMPLETE ANALYSIS FINISHED\n")
cat("========================================\n")
cat("Author-feature pairs:", nrow(pair_stats), "\n")
cat("Extracted occurrences:", nrow(occ), "\n")
cat("Distinct lexical realisations:", nrow(lexical), "\n")
cat("Concordance examples:", nrow(examples), "\n")
cat("Output:", OUT_DIR, "\n\n")

print(slide_stats)
print(summary_groups)

#!/usr/bin/env Rscript

# ============================================================
# 10_lexical_author_specificity.R
#
# Lexical author specificity and cross-story recurrence.
#
# Inputs from earlier pipeline stages are read-only.
# Scripts 01-09 and their outputs are never modified.
#
# Outputs:
# 00_validation.csv
# 01_all_lexical_comparisons.csv
# 02_pair_specificity.csv
# 03_summary.csv
# 04_ranked_examples.csv
# 05_lexical_story_counts.csv
# 06_lexical_recurrence.csv
# 07_pair_recurrence.csv
# 08_recurrence_summary.csv
# 09_cross_story_examples.csv
# ============================================================

suppressPackageStartupMessages(library(data.table))

# ------------------------------------------------------------
# 1. Paths
# ------------------------------------------------------------

base <- Sys.getenv(
  "FF_CORPUS_DIR",
  unset = "PATH/Misc/FF_Corpus"
)

pos_dir <- file.path(
  base,
  "HP_Liebesgeschichte_1000word_chunks_FINAL",
  "POS_tagged"
)

root <- file.path(pos_dir, "experiment_xpos")

tagged_path <- file.path(pos_dir, "tagged_tokens.rds")

selection_dir <- file.path(root, "xpos_preferences")

out <- file.path(root, "lexical_author_specificity")

# ------------------------------------------------------------
# 2. Read selected POS patterns
# ------------------------------------------------------------

cat("Loading selected POS features...\n")

selected <- rbindlist(
  list(
    fread(file.path(
      selection_dir,
      "author_feature_top10_n2_df3.csv"
    )),
    fread(file.path(
      selection_dir,
      "author_feature_top10_n3_df5.csv"
    ))
  ),
  use.names = TRUE,
  fill = TRUE
)

selected[, author_id := as.character(author_id)]
selected[, ngram := as.integer(ngram)]
selected[, feature := as.character(feature)]

stopifnot(
  nrow(selected) == 5040L,
  uniqueN(selected$author_id) == 252L,
  !anyDuplicated(
    selected[, .(author_id, ngram, feature)]
  ),
  all(selected$ngram %in% c(2L, 3L))
)

selected[, pair_id := sprintf("P%04d", .I)]

# ------------------------------------------------------------
# 3. Read tagged training corpus
# ------------------------------------------------------------

cat("Loading tagged training corpus...\n")

tok <- as.data.table(readRDS(tagged_path))

required <- c(
  "author_id",
  "set",
  "final_filename",
  "text_id",
  "token_position",
  "token",
  "xpos"
)

stopifnot(all(required %in% names(tok)))

tok <- tok[
  tolower(trimws(as.character(set))) == "train",
  ..required
]

tok[, author_id := as.character(author_id)]

stopifnot(
  uniqueN(tok$author_id) == 409L,
  uniqueN(
    tok[, .(author_id, final_filename)]
  ) == 6135L
)

setorder(
  tok,
  author_id,
  final_filename,
  token_position
)

tok[
  ,
  chunk_id := .GRP,
  by = .(author_id, final_filename)
]

# Preserve the original story identifier for each chunk.

story_map <- tok[
  ,
  .(
    text_id = first(text_id),
    n_story_ids = uniqueN(text_id, na.rm = FALSE)
  ),
  by = .(author_id, chunk_id)
]

stopifnot(
  all(story_map$n_story_ids == 1L),
  !anyNA(story_map$text_id)
)

story_map <- story_map[
  ,
  .(author_id, chunk_id, text_id)
]

# ------------------------------------------------------------
# 4. Token and XPOS preprocessing
# ------------------------------------------------------------

# Match the POS preprocessing used in feature selection.
#
# Missing, empty, and literal "NA" XPOS values are represented
# as XY. This is essential for recovering the 20 selected
# author-POS pairs containing XY.

tok[
  ,
  token_clean := tolower(
    trimws(as.character(token))
  )
]

tok[
  ,
  xpos_clean := trimws(as.character(xpos))
]

missing_xpos <- is.na(tok$xpos_clean) |
  tok$xpos_clean == "" |
  tok$xpos_clean == "NA"

cat(
  "Missing XPOS tags represented as XY:",
  sum(missing_xpos),
  "\n"
)

tok[missing_xpos, xpos_clean := "XY"]

# ------------------------------------------------------------
# 5. Extract lexical realisations
# ------------------------------------------------------------

# The POS selection is fixed.
# Extract all occurrences of selected patterns from all
# 409 authors, without crossing chunk boundaries.

chunk_parts <- vector("list", 2L)

for (n in 2:3) {
  
  cat("Extracting", n, "-grams...\n")
  
  wanted <- unique(
    selected[ngram == n, feature]
  )
  
  part <- tok[
    ,
    .(
      author_id,
      chunk_id,
      token_clean,
      xpos_clean
    )
  ]
  
  for (k in seq_len(n - 1L)) {
    
    part[
      ,
      paste0("t", k) := shift(
        token_clean,
        k,
        type = "lead"
      ),
      by = chunk_id
    ]
    
    part[
      ,
      paste0("p", k) := shift(
        xpos_clean,
        k,
        type = "lead"
      ),
      by = chunk_id
    ]
  }
  
  tagcols <- c(
    "xpos_clean",
    paste0("p", seq_len(n - 1L))
  )
  
  lexcols <- c(
    "token_clean",
    paste0("t", seq_len(n - 1L))
  )
  
  valid <- complete.cases(part[, ..tagcols]) &
    complete.cases(part[, ..lexcols])
  
  part <- part[valid]
  
  part[
    ,
    feature := do.call(
      paste,
      c(.SD, sep = "__")
    ),
    .SDcols = tagcols
  ]
  
  part <- part[feature %chin% wanted]
  
  part[
    ,
    lexical_realisation := do.call(
      paste,
      c(.SD, sep = " ")
    ),
    .SDcols = lexcols
  ]
  
  part[, ngram := n]
  
  chunk_parts[[n - 1L]] <- part[
    ,
    .(N = .N),
    by = .(
      author_id,
      chunk_id,
      ngram,
      feature,
      lexical_realisation
    )
  ]
  
  rm(part)
  invisible(gc(verbose = FALSE))
}

rm(tok)
invisible(gc(verbose = FALSE))

chunk_lex <- rbindlist(
  chunk_parts,
  use.names = TRUE
)

rm(chunk_parts)
invisible(gc(verbose = FALSE))

stopifnot(
  nrow(chunk_lex) > 0L,
  all(chunk_lex$N > 0L)
)

# ------------------------------------------------------------
# 6. Validate against the original POS frequencies
# ------------------------------------------------------------

cat("Validating all 5,040 selected POS pairs...\n")

pos_author <- chunk_lex[
  ,
  .(pos_count = sum(N)),
  by = .(ngram, feature, author_id)
]

validation <- merge(
  selected[
    ,
    .(
      pair_id,
      author_id,
      ngram,
      feature,
      original_count = author_count
    )
  ],
  pos_author[
    ,
    .(
      author_id,
      ngram,
      feature,
      reconstructed_count = pos_count
    )
  ],
  by = c("author_id", "ngram", "feature"),
  all.x = TRUE,
  sort = FALSE
)

validation[
  ,
  count_matches :=
    !is.na(reconstructed_count) &
    original_count == reconstructed_count
]

setcolorder(
  validation,
  c(
    "pair_id",
    "author_id",
    "ngram",
    "feature",
    "original_count",
    "reconstructed_count",
    "count_matches"
  )
)

setorder(validation, pair_id)

cat(
  "Matching pairs:",
  sum(validation$count_matches),
  "/",
  nrow(validation),
  "\n"
)

if (
  nrow(validation) != 5040L ||
  !all(validation$count_matches)
) {
  
  print(
    validation[count_matches == FALSE][1:20]
  )
  
  stop(
    "POS frequency validation failed. ",
    "No output files have been written."
  )
}

# ------------------------------------------------------------
# 7. Original lexical specificity analysis
# ------------------------------------------------------------

cat("Calculating lexical author specificity...\n")

lex <- chunk_lex[
  ,
  .(N = sum(N)),
  by = .(
    ngram,
    feature,
    lexical_realisation,
    author_id
  )
]

pos_global <- pos_author[
  ,
  .(global_pos_count = sum(pos_count)),
  by = .(ngram, feature)
]

lex_global <- lex[
  ,
  .(
    global_lex_count = sum(N),
    authors_using_lex = .N
  ),
  by = .(
    ngram,
    feature,
    lexical_realisation
  )
]

setkey(
  lex_global,
  ngram,
  feature,
  lexical_realisation
)

lex <- lex_global[
  lex,
  on = .(
    ngram,
    feature,
    lexical_realisation
  )
]

lex[
  ,
  other_lex_count := global_lex_count - N
]

# Select focal pairs after computing reference counts
# from all 409 authors.

focal <- merge(
  lex,
  selected[
    ,
    .(
      pair_id,
      author_id,
      ngram,
      feature
    )
  ],
  by = c("author_id", "ngram", "feature")
)

focal <- merge(
  focal,
  pos_author,
  by = c("author_id", "ngram", "feature")
)

focal <- merge(
  focal,
  pos_global,
  by = c("ngram", "feature")
)

focal[
  ,
  other_pos_count := global_pos_count - pos_count
]

stopifnot(
  uniqueN(focal$pair_id) == 5040L
)

# Compare lexical choices conditional on the POS pattern.

focal[
  ,
  author_conditional_share := N / pos_count
]

focal[
  ,
  others_conditional_share := fifelse(
    other_pos_count > 0,
    other_lex_count / other_pos_count,
    NA_real_
  )
]

focal[
  ,
  conditional_share_difference :=
    author_conditional_share -
    others_conditional_share
]

focal[
  ,
  log2_conditional_enrichment := log2(
    ((N + 0.5) / (pos_count + 1)) /
      ((other_lex_count + 0.5) /
         (other_pos_count + 1))
  )
]

focal[
  ,
  other_authors_mean_count :=
    other_lex_count / 408
]

focal[
  ,
  count_vs_others_mean :=
    N / (other_authors_mean_count + 0.5)
]

focal[
  ,
  absent_from_other_authors :=
    other_lex_count == 0
]

# ------------------------------------------------------------
# 8. Original pair-level results
# ------------------------------------------------------------

pair <- focal[
  ,
  .(
    pos_count = unique(pos_count),
    
    global_pos_count = unique(global_pos_count),
    
    distinct_lexical_realisations = .N,
    
    proportion_pos_occurrences_lexically_enriched =
      sum(
        N[conditional_share_difference > 0],
        na.rm = TRUE
      ) / unique(pos_count),
    
    proportion_pos_occurrences_strongly_enriched =
      sum(
        N[log2_conditional_enrichment >= 1],
        na.rm = TRUE
      ) / unique(pos_count),
    
    proportion_pos_occurrences_unique_to_author =
      sum(N[absent_from_other_authors]) /
      unique(pos_count),
    
    distinct_realisations_unique_to_author =
      sum(absent_from_other_authors),
    
    dominant_realisation =
      lexical_realisation[which.max(N)],
    
    dominant_count = max(N),
    
    dominant_other_authors_count =
      other_lex_count[which.max(N)],
    
    dominant_author_share =
      author_conditional_share[which.max(N)],
    
    dominant_others_share =
      others_conditional_share[which.max(N)],
    
    dominant_conditional_enrichment =
      log2_conditional_enrichment[which.max(N)]
  ),
  by = .(
    pair_id,
    author_id,
    ngram,
    feature
  )
]

stopifnot(
  nrow(pair) == 5040L,
  !anyNA(pair$pos_count)
)

summary <- pair[
  ,
  .(
    pairs = .N,
    
    authors = uniqueN(author_id),
    
    mean_share_occurrences_lexically_enriched =
      mean(
        proportion_pos_occurrences_lexically_enriched
      ),
    
    mean_share_occurrences_strongly_enriched =
      mean(
        proportion_pos_occurrences_strongly_enriched
      ),
    
    mean_share_occurrences_unique_to_author =
      mean(
        proportion_pos_occurrences_unique_to_author
      ),
    
    pairs_with_any_author_unique_realisation =
      sum(
        distinct_realisations_unique_to_author > 0
      ),
    
    pairs_dominant_form_more_common_conditionally =
      sum(
        dominant_conditional_enrichment > 0,
        na.rm = TRUE
      )
  ),
  by = ngram
]

examples <- pair[
  order(
    -dominant_count,
    -dominant_conditional_enrichment
  )
]

# ------------------------------------------------------------
# 9. Assign lexical occurrences to stories
# ------------------------------------------------------------

cat("Calculating cross-story recurrence...\n")

# No second n-gram extraction is performed.

chunk_lex <- merge(
  chunk_lex,
  story_map,
  by = c("author_id", "chunk_id"),
  all.x = TRUE,
  sort = FALSE
)

stopifnot(
  !anyNA(chunk_lex$text_id)
)

story_lex <- chunk_lex[
  ,
  .(story_count = sum(N)),
  by = .(
    author_id,
    ngram,
    feature,
    lexical_realisation,
    text_id
  )
]

focal_story <- merge(
  story_lex,
  selected[
    ,
    .(
      pair_id,
      author_id,
      ngram,
      feature
    )
  ],
  by = c("author_id", "ngram", "feature"),
  sort = FALSE
)

story_agg <- focal_story[
  ,
  .(
    story_total_count = sum(story_count),
    
    stories_with_realisation = uniqueN(text_id),
    
    max_occurrences_in_one_story =
      max(story_count),
    
    stories_with_repeated_occurrences =
      sum(story_count >= 2L)
  ),
  by = .(
    pair_id,
    author_id,
    ngram,
    feature,
    lexical_realisation
  )
]

# ------------------------------------------------------------
# 10. Combine recurrence and specificity
# ------------------------------------------------------------

recurrence <- merge(
  focal[
    ,
    .(
      pair_id,
      author_id,
      ngram,
      feature,
      lexical_realisation,
      N,
      pos_count,
      other_lex_count,
      author_conditional_share,
      others_conditional_share,
      conditional_share_difference,
      log2_conditional_enrichment,
      absent_from_other_authors
    )
  ],
  story_agg,
  by = c(
    "pair_id",
    "author_id",
    "ngram",
    "feature",
    "lexical_realisation"
  ),
  all.x = TRUE,
  sort = FALSE
)

stopifnot(
  nrow(recurrence) == nrow(focal),
  !anyNA(recurrence$story_total_count),
  all(recurrence$N == recurrence$story_total_count)
)

recurrence[
  ,
  repeated := N >= 2L
]

recurrence[
  ,
  cross_story_repeated :=
    stories_with_realisation >= 2L
]

recurrence[
  ,
  conditionally_preferred :=
    !is.na(conditional_share_difference) &
    conditional_share_difference > 0
]

recurrence[
  ,
  cross_story_preference :=
    cross_story_repeated &
    conditionally_preferred
]

recurrence[
  ,
  strong_cross_story_preference :=
    cross_story_preference &
    log2_conditional_enrichment >= 1
]

# ------------------------------------------------------------
# 11. Pair-level recurrence statistics
# ------------------------------------------------------------

recurrence_pair <- recurrence[
  ,
  .(
    pos_count = unique(pos_count),
    
    lexical_types = .N,
    
    singleton_types = sum(N == 1L),
    
    repeated_types = sum(repeated),
    
    cross_story_repeated_types =
      sum(cross_story_repeated),
    
    conditionally_preferred_types =
      sum(conditionally_preferred),
    
    cross_story_preference_types =
      sum(cross_story_preference),
    
    strong_cross_story_preference_types =
      sum(strong_cross_story_preference),
    
    repeated_occurrences =
      sum(N[repeated]),
    
    cross_story_repeated_occurrences =
      sum(N[cross_story_repeated]),
    
    cross_story_preference_occurrences =
      sum(N[cross_story_preference]),
    
    strong_cross_story_preference_occurrences =
      sum(N[strong_cross_story_preference])
  ),
  by = .(
    pair_id,
    author_id,
    ngram,
    feature
  )
]

recurrence_pair[
  ,
  cross_story_preference_occurrence_share :=
    cross_story_preference_occurrences / pos_count
]

stopifnot(
  nrow(recurrence_pair) == 5040L
)

# ------------------------------------------------------------
# 12. Recurrence summary
# ------------------------------------------------------------

recurrence_summary <- recurrence_pair[
  ,
  .(
    pairs = .N,
    
    authors = uniqueN(author_id),
    
    pairs_with_repeated_forms =
      sum(repeated_types > 0),
    
    pairs_with_cross_story_repeated_forms =
      sum(cross_story_repeated_types > 0),
    
    pairs_with_cross_story_preferences =
      sum(cross_story_preference_types > 0),
    
    pairs_with_strong_cross_story_preferences =
      sum(strong_cross_story_preference_types > 0),
    
    mean_cross_story_preference_occurrence_share =
      mean(
        cross_story_preference_occurrence_share
      )
  ),
  by = ngram
]

# ------------------------------------------------------------
# 13. Final validation
# ------------------------------------------------------------

cat("Running final validation...\n")

focal_counts <- focal[
  ,
  .(lexical_total = sum(N)),
  by = pair_id
]

focal_counts <- merge(
  focal_counts,
  selected[
    ,
    .(
      pair_id,
      author_count
    )
  ],
  by = "pair_id"
)

stopifnot(
  nrow(focal_counts) == 5040L,
  all(
    focal_counts$lexical_total ==
      focal_counts$author_count
  ),
  sum(focal_story$story_count) == sum(focal$N),
  all(validation$count_matches),
  nrow(pair) == 5040L,
  nrow(recurrence_pair) == 5040L
)

# ------------------------------------------------------------
# 14. Write outputs
# ------------------------------------------------------------

# All writes are restricted to the analysis-10 directory.

dir.create(
  out,
  recursive = TRUE,
  showWarnings = FALSE
)

setcolorder(
  focal,
  c(
    "pair_id",
    "author_id",
    "ngram",
    "feature",
    "lexical_realisation",
    "N",
    "global_lex_count",
    "authors_using_lex",
    "other_lex_count",
    "pos_count",
    "global_pos_count",
    "other_pos_count",
    "author_conditional_share",
    "others_conditional_share",
    "conditional_share_difference",
    "log2_conditional_enrichment",
    "other_authors_mean_count",
    "count_vs_others_mean",
    "absent_from_other_authors"
  )
)

setorder(focal, pair_id, lexical_realisation)
setorder(pair, pair_id)

fwrite(
  validation,
  file.path(out, "00_validation.csv")
)

fwrite(
  focal,
  file.path(out, "01_all_lexical_comparisons.csv")
)

fwrite(
  pair,
  file.path(out, "02_pair_specificity.csv")
)

fwrite(
  summary,
  file.path(out, "03_summary.csv")
)

fwrite(
  examples,
  file.path(out, "04_ranked_examples.csv")
)

fwrite(
  focal_story,
  file.path(out, "05_lexical_story_counts.csv")
)

fwrite(
  recurrence,
  file.path(out, "06_lexical_recurrence.csv")
)

fwrite(
  recurrence_pair,
  file.path(out, "07_pair_recurrence.csv")
)

fwrite(
  recurrence_summary,
  file.path(out, "08_recurrence_summary.csv")
)

cross_story_examples <- recurrence[
  cross_story_preference == TRUE
]

setorder(
  cross_story_examples,
  -stories_with_realisation,
  -N
)

fwrite(
  cross_story_examples,
  file.path(out, "09_cross_story_examples.csv")
)

# ------------------------------------------------------------
# 15. Report
# ------------------------------------------------------------

cat("\n====================================\n")
cat("ANALYSIS 10 COMPLETED\n")
cat("====================================\n")

cat("Training authors: 409\n")
cat("Focal authors: 252\n")
cat("Validated POS pairs: 5040\n")

cat("\nOriginal lexical specificity summary:\n")
print(summary)

cat("\nCross-story recurrence summary:\n")
print(recurrence_summary)

cat("\nOutputs written to:\n")
cat(out, "\n")


# 08_analyse_xpos_preferences.R
# Descriptive analysis of author-specific XPOS bigram/trigram preferences.
# No model fitting; held-out test chunks are excluded throughout.

.libPaths(c("~/R/library", .libPaths()))
suppressPackageStartupMessages(library(Matrix))

root <- "PATH/Misc/FF_Corpus/HP_Liebesgeschichte_1000word_chunks_FINAL/POS_tagged/experiment_xpos"
out <- file.path(root, "xpos_preferences")
dir.create(out, recursive = TRUE, showWarnings = FALSE)

# Conservative screening thresholds. Rankings retain all eligible features.
min_author_count <- 10L
min_author_stories <- 2L
min_global_count <- 30L
pseudocount <- 0.5

write_csv <- function(x, name) {
  utils::write.csv(x, file.path(out, name), row.names = FALSE, na = "")
}

for (n in c(2L, 3L)) {
  min_df <- if (n == 2L) 3L else 5L
  message("Processing n=", n, ", DF=", min_df)
  obj <- readRDS(file.path(root, sprintf("features_n%d_df3plus.rds", n)))
  x <- obj$x
  meta <- as.data.frame(obj$metadata, stringsAsFactors = FALSE)
  stopifnot(nrow(x) == nrow(meta), ncol(x) == length(obj$feature_names))
  stopifnot(all(c("author_id", "text_id", "set") %in% names(meta)))
  stopifnot(!anyNA(meta[, c("author_id", "text_id", "set")]))
  
  train <- which(tolower(trimws(as.character(meta$set))) == "train")
  stopifnot(length(train) == 6135L)
  keep <- which(obj$feature_df >= min_df)
  stopifnot(length(keep) > 0L)
  X <- x[train, keep, drop = FALSE]
  M <- meta[train, , drop = FALSE]
  features <- obj$feature_names[keep]
  stopifnot(!anyDuplicated(features))
  rm(x, obj)
  gc(verbose = FALSE)
  
  # Each row is one 1,000-word training chunk. Aggregate sparse counts by author.
  author_ids <- sort(unique(as.character(M$author_id)))
  author_index <- match(M$author_id, author_ids)
  A <- sparseMatrix(i = author_index, j = seq_len(nrow(X)), x = 1,
                    dims = c(length(author_ids), nrow(X)))
  author_counts <- as(A %*% X, "dgCMatrix")
  author_total <- as.numeric(rowSums(author_counts))
  global_counts <- as.numeric(colSums(author_counts))
  total_all <- sum(global_counts)
  n_author_chunks <- tabulate(author_index, nbins = length(author_ids))
  stopifnot(all(n_author_chunks == 15L))
  
  # Number of distinct training stories containing each feature for each author.
  # text_id is treated as the story identifier, nested within author_id.
  story_key <- paste(M$author_id, M$text_id, sep = "\r")
  story_ids <- unique(story_key)
  story_index <- match(story_key, story_ids)
  story_author_index <- match(M$author_id[match(story_ids, story_key)], author_ids)
  S <- sparseMatrix(i = story_index, j = seq_len(nrow(X)), x = 1,
                    dims = c(length(story_ids), nrow(X)))
  story_counts <- as(S %*% X, "dgCMatrix")
  story_presence <- story_counts
  story_presence@x[] <- 1
  SA <- sparseMatrix(i = story_author_index, j = seq_along(story_ids), x = 1,
                     dims = c(length(author_ids), length(story_ids)))
  author_story_presence <- as(SA %*% story_presence, "dgCMatrix")
  author_n_stories <- tabulate(story_author_index, nbins = length(author_ids))
  
  # Long-form table of observed author-feature combinations, no dense 409 x 21k matrix.
  trip <- summary(author_counts)
  story_trip <- summary(author_story_presence)
  story_keys_numeric <- (as.double(story_trip$i) - 1) * ncol(X) + story_trip$j
  count_keys_numeric <- (as.double(trip$i) - 1) * ncol(X) + trip$j
  story_matches <- match(count_keys_numeric, story_keys_numeric)
  stopifnot(!anyNA(story_matches))
  stories_present <- as.integer(story_trip$x[story_matches])
  
  a <- trip$i
  f <- trip$j
  count <- as.numeric(trip$x)
  other_count <- global_counts[f] - count
  other_total <- total_all - author_total[a]
  rate_author <- count / author_total[a]
  rate_other <- other_count / other_total
  
  # Smoothed rate ratio: guards against zero background counts.
  log2_ratio <- log2(((count + pseudocount) / (author_total[a] + pseudocount * 2)) /
                       ((other_count + pseudocount) / (other_total + pseudocount * 2)))
  
  detail <- data.frame(
    author_id = author_ids[a],
    feature = features[f],
    ngram = n,
    min_df = min_df,
    author_count = count,
    author_total_ngrams = author_total[a],
    author_rate_per_10000 = rate_author * 10000,
    other_authors_rate_per_10000 = rate_other * 10000,
    log2_author_vs_others = log2_ratio,
    stories_with_feature = stories_present,
    author_training_stories = author_n_stories[a],
    proportion_stories_with_feature = stories_present / author_n_stories[a],
    global_training_count = global_counts[f],
    stringsAsFactors = FALSE
  )
  detail <- detail[order(detail$author_id, -detail$log2_author_vs_others,
                         -detail$author_count), , drop = FALSE]
  write_csv(detail, sprintf("author_feature_all_n%d_df%d.csv", n, min_df))
  
  # Candidates: repeated across at least two different training stories.
  # Authors with one training story are excluded from cross-story claims.
  candidates <- subset(detail,
                       author_count >= min_author_count &
                         stories_with_feature >= min_author_stories &
                         author_training_stories >= min_author_stories &
                         global_training_count >= min_global_count &
                         log2_author_vs_others > 0)
  candidates <- candidates[order(candidates$author_id,
                                 -candidates$log2_author_vs_others,
                                 -candidates$proportion_stories_with_feature,
                                 -candidates$author_count), , drop = FALSE]
  write_csv(candidates, sprintf("author_feature_candidates_n%d_df%d.csv", n, min_df))
  
  # Compact top 10 per author for manual linguistic inspection.
  if (nrow(candidates)) {
    rank_in_author <- ave(seq_len(nrow(candidates)), candidates$author_id,
                          FUN = seq_along)
    top <- candidates[rank_in_author <= 10L, , drop = FALSE]
  } else {
    top <- candidates
  }
  write_csv(top, sprintf("author_feature_top10_n%d_df%d.csv", n, min_df))
  
  # Feature-level summary: how widespread a sequence is across authors.
  author_feature_n <- tabulate(f, nbins = ncol(X))
  candidate_feature_n <- tabulate(match(candidates$feature, features), nbins = ncol(X))
  feature_summary <- data.frame(
    feature = features,
    ngram = n,
    min_df = min_df,
    training_count = global_counts,
    authors_using_feature = author_feature_n,
    authors_with_candidate_preference = candidate_feature_n,
    stringsAsFactors = FALSE
  )
  feature_summary <- feature_summary[order(-feature_summary$authors_with_candidate_preference,
                                           -feature_summary$training_count), , drop = FALSE]
  write_csv(feature_summary, sprintf("feature_summary_n%d_df%d.csv", n, min_df))
  
  author_summary <- data.frame(
    author_id = author_ids,
    training_chunks = n_author_chunks,
    training_stories = author_n_stories,
    observed_features = tabulate(a, nbins = length(author_ids)),
    candidate_preferences = tabulate(match(candidates$author_id, author_ids),
                                     nbins = length(author_ids)),
    stringsAsFactors = FALSE
  )
  write_csv(author_summary, sprintf("author_summary_n%d_df%d.csv", n, min_df))
  
  message("n=", n, ": ", nrow(detail), " observed author-feature pairs; ",
          nrow(candidates), " candidate preferences; ",
          sum(author_n_stories >= 2L), " authors with >=2 training stories")
  rm(X, M, A, S, SA, story_counts, story_presence, author_counts,
     author_story_presence, detail, candidates, top, feature_summary, author_summary)
  gc(verbose = FALSE)
}

message("Finished. CSV outputs in: ", out)

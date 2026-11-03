
#!/usr/bin/env Rscript

# Inspect SVM weights for selected author-specific POS n-grams.
#
# Six conditions:
#   n = 2, 3
#   minimum document frequency = 3, 5, 10
#
# Refit final models using the original training data,
# selected C values and classifier settings.
#
# No new hyperparameter tuning.
# No use of held-out test chunks.
# Original results and selections remain unchanged.

.libPaths(c("~/R/library", .libPaths()))

suppressPackageStartupMessages({
  library(Matrix)
  library(LiblineaR)
})

BASE <- "PATH/Misc/FF_Corpus"

ROOT <- file.path(
  BASE,
  "HP_Liebesgeschichte_1000word_chunks_FINAL",
  "POS_tagged",
  "experiment_xpos"
)

PREF <- file.path(ROOT, "xpos_preferences")
OUT <- file.path(ROOT, "svm_feature_inspection")

dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

write_csv <- function(x, filename) {
  write.csv(
    x,
    file.path(OUT, filename),
    row.names = FALSE,
    na = ""
  )
}

# Extract feature weights as a features x authors matrix.
# LiblineaR may store the bias as an additional row.
# Stop if the structure cannot be established unambiguously.

get_weights <- function(model, p, k) {
  
  W <- model$W
  
  if (is.null(W) || is.null(dim(W))) {
    stop("Model does not contain a weight matrix.")
  }
  
  d <- dim(W)
  
  if (identical(as.integer(d), c(p + 1L, k))) {
    return(W[seq_len(p), , drop = FALSE])
  }
  
  if (identical(as.integer(d), c(p, k))) {
    return(W)
  }
  
  if (identical(as.integer(d), c(k, p + 1L))) {
    return(t(W[, seq_len(p), drop = FALSE]))
  }
  
  if (identical(as.integer(d), c(k, p))) {
    return(t(W))
  }
  
  stop(
    "Unexpected weight dimensions: ",
    paste(d, collapse = " x ")
  )
}

all_summaries <- list()

for (n in c(2L, 3L)) {
  
  cat("\n====================================\n")
  cat("PROCESSING", n, "-GRAMS\n")
  cat("====================================\n")
  
  feature_path <- file.path(
    ROOT,
    sprintf("features_n%d_df3plus.rds", n)
  )
  
  result_path <- file.path(
    ROOT,
    sprintf("results_n%d.rds", n)
  )
  
  # Preserve the original linguistic selection:
  # bigrams DF >= 3; trigrams DF >= 5.
  
  selection_df <- if (n == 2L) 3L else 5L
  
  preference_path <- file.path(
    PREF,
    sprintf(
      "author_feature_top10_n%d_df%d.csv",
      n, selection_df
    )
  )
  
  for (path in c(
    feature_path,
    result_path,
    preference_path
  )) {
    if (!file.exists(path)) {
      stop("Missing input: ", path)
    }
  }
  
  features <- readRDS(feature_path)
  original <- readRDS(result_path)
  
  preferences <- read.csv(
    preference_path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
  stopifnot(
    inherits(features$x, "dgCMatrix"),
    nrow(features$x) == 8180L,
    nrow(features$metadata) == 8180L,
    ncol(features$x) == length(features$feature_names),
    ncol(features$x) == length(features$feature_df),
    all(c("author_id", "set") %in%
          names(features$metadata)),
    all(c("author_id", "feature") %in%
          names(preferences)),
    nrow(preferences) == 2520L,
    length(unique(preferences$author_id)) == 252L,
    !anyDuplicated(
      preferences[c("author_id", "feature")]
    )
  )
  
  meta <- features$metadata
  
  train <- which(
    as.character(meta$set) == "train"
  )
  
  stopifnot(length(train) == 6135L)
  
  authors <- sort(
    unique(as.character(meta$author_id))
  )
  
  stopifnot(length(authors) == 409L)
  
  y <- match(
    as.character(meta$author_id[train]),
    authors
  )
  
  stopifnot(
    !anyNA(y),
    setequal(y, seq_along(authors))
  )
  
  preferences$author_id <- as.character(
    preferences$author_id
  )
  
  author_index <- match(
    preferences$author_id,
    authors
  )
  
  if (anyNA(author_index)) {
    stop("Selected preference contains unknown author.")
  }
  
  # Reconstruct all three original final models.
  
  for (df_min in c(3L, 5L, 10L)) {
    
    cat(
      "\nN-GRAM:", n,
      " MINIMUM DF:", df_min,
      "\n"
    )
    
    columns <- which(
      features$feature_df >= df_min
    )
    
    feature_names <- as.character(
      features$feature_names[columns]
    )
    
    stopifnot(
      length(columns) > 0L,
      !anyDuplicated(feature_names)
    )
    
    X <- features$x[
      train,
      columns,
      drop = FALSE
    ]
    
    # Retrieve the original cross-validated C.
    
    original_row <- original$results[
      original$results$ngram == n &
        original$results$min_df == df_min,
      ,
      drop = FALSE
    ]
    
    if (nrow(original_row) != 1L) {
      stop("Original condition not uniquely identified.")
    }
    
    C <- as.numeric(original_row$chosen_C[1])
    
    if (
      !is.finite(C) ||
      !(C %in% c(0.01, 0.1, 1, 10))
    ) {
      stop("Invalid original C value.")
    }
    
    if (ncol(X) != original_row$features[1]) {
      stop("Feature count differs from original run.")
    }
    
    cat(
      "Features:", ncol(X),
      "\nOriginal C:", C,
      "\n"
    )
    
    # Identical final training configuration.
    
    model <- LiblineaR::LiblineaR(
      data = X,
      target = y,
      type = 1,
      cost = C,
      bias = 1,
      verbose = FALSE
    )
    
    W <- get_weights(
      model,
      ncol(X),
      length(authors)
    )
    
    if (
      anyNA(W) ||
      any(!is.finite(W))
    ) {
      stop("Non-finite SVM weights.")
    }
    
    # Align columns to the original author labels.
    
    class_names <- suppressWarnings(
      as.integer(as.character(model$ClassNames))
    )
    
    if (
      length(class_names) != length(authors) ||
      anyNA(class_names) ||
      !setequal(class_names, seq_along(authors))
    ) {
      stop("Cannot establish model class alignment.")
    }
    
    # Validate the refitted model's predictions
    # on training data only.
    
    predicted <- as.integer(
      predict(
        model,
        newx = X,
        decisionValues = FALSE
      )$predictions
    )
    
    if (
      length(predicted) != length(y) ||
      anyNA(predicted) ||
      any(!predicted %in% seq_along(authors))
    ) {
      stop("Invalid training predictions.")
    }
    
    train_accuracy <- mean(predicted == y)
    
    # Match original selected patterns to model features.
    
    feature_index <- match(
      preferences$feature,
      feature_names
    )
    
    present <- !is.na(feature_index)
    
    weight <- rep(
      NA_real_,
      nrow(preferences)
    )
    
    weight_rank <- rep(
      NA_integer_,
      nrow(preferences)
    )
    
    top_features <- vector(
      "list",
      length(authors)
    )
    
    # Inspect all 409 author-specific weight vectors.
    
    for (i in seq_along(authors)) {
      
      class_column <- match(i, class_names)
      
      w <- W[, class_column]
      
      # Higher positive weights favour this author's
      # one-vs-rest decision score.
      # Rank among all retained model features.
      
      ranks <- rank(
        -w,
        ties.method = "min"
      )
      
      selected_rows <- which(
        author_index == i & present
      )
      
      if (length(selected_rows) > 0L) {
        
        positions <- feature_index[selected_rows]
        
        weight[selected_rows] <- w[positions]
        
        weight_rank[selected_rows] <- as.integer(
          ranks[positions]
        )
      }
      
      # Export 20 highest-weighted features per author.
      
      order_w <- head(
        order(w, decreasing = TRUE),
        20L
      )
      
      top_features[[i]] <- data.frame(
        author_id = authors[i],
        ngram = n,
        min_df = df_min,
        feature = feature_names[order_w],
        svm_weight = w[order_w],
        svm_rank = as.integer(ranks[order_w]),
        stringsAsFactors = FALSE
      )
    }
    
    # Detailed comparison with original linguistic
    # preference selection.
    
    result <- preferences
    
    result$model_min_df <- df_min
    result$selection_min_df <- selection_df
    result$present_in_model <- present
    
    result$svm_weight <- weight
    
    result$svm_positive_weight <- ifelse(
      present,
      weight > 0,
      NA
    )
    
    result$svm_rank <- weight_rank
    
    result$svm_top_1_percent <- ifelse(
      present,
      weight_rank <= ceiling(ncol(X) * 0.01),
      NA
    )
    
    result$svm_top_5_percent <- ifelse(
      present,
      weight_rank <= ceiling(ncol(X) * 0.05),
      NA
    )
    
    result$svm_top_10_percent <- ifelse(
      present,
      weight_rank <= ceiling(ncol(X) * 0.10),
      NA
    )
    
    result$svm_rank_percentile <- ifelse(
      present,
      100 * (1 - (weight_rank - 1) / ncol(X)),
      NA
    )
    
    result$model_features <- ncol(X)
    result$chosen_C <- C
    
    result <- result[
      order(
        result$author_id,
        result$svm_rank,
        na.last = TRUE
      ),
      ,
      drop = FALSE
    ]
    
    write_csv(
      result,
      sprintf(
        "selected_preferences_svm_n%d_df%d.csv",
        n, df_min
      )
    )
    
    write_csv(
      do.call(rbind, top_features),
      sprintf(
        "svm_top20_per_author_n%d_df%d.csv",
        n, df_min
      )
    )
    
    # Summary for each experimental condition.
    
    summary_row <- data.frame(
      ngram = n,
      min_df = df_min,
      selection_min_df = selection_df,
      chosen_C = C,
      model_features = ncol(X),
      authors = length(authors),
      selected_pairs = nrow(result),
      selected_present = sum(present),
      selected_absent = sum(!present),
      selected_positive = sum(weight[present] > 0),
      selected_top_1_percent = sum(
        result$svm_top_1_percent,
        na.rm = TRUE
      ),
      selected_top_5_percent = sum(
        result$svm_top_5_percent,
        na.rm = TRUE
      ),
      selected_top_10_percent = sum(
        result$svm_top_10_percent,
        na.rm = TRUE
      ),
      median_rank_percentile = if (any(present)) {
        median(
          result$svm_rank_percentile,
          na.rm = TRUE
        )
      } else {
        NA_real_
      },
      refit_training_accuracy = train_accuracy
    )
    
    write_csv(
      summary_row,
      sprintf(
        "svm_selected_summary_n%d_df%d.csv",
        n, df_min
      )
    )
    
    all_summaries[[length(all_summaries) + 1L]] <-
      summary_row
    
    print(summary_row)
    
    rm(
      X, model, W, predicted,
      result, top_features, weight_rank, weight
    )
    
    gc(verbose = FALSE)
  }
  
  rm(features, original, preferences)
  gc(verbose = FALSE)
}

# Consolidated summary of all six models.

final_summary <- do.call(
  rbind,
  all_summaries
)

stopifnot(
  nrow(final_summary) == 6L,
  all(final_summary$selected_pairs == 2520L)
)

write_csv(
  final_summary,
  "svm_all_six_conditions_summary.csv"
)

cat("\nALL SIX CONDITIONS COMPLETED\n")
print(final_summary)
cat("\nOutput directory:", OUT, "\n")

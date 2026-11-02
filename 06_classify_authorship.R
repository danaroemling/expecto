
#!/usr/bin/env Rscript
# Run as array job NGRAM_N=1,...,5 AFTER feature extraction completes.
# Linear SVM with story-grouped cross-validation, raw counts, no L2 scaling.

.libPaths(c("~/R/library", .libPaths()))

suppressPackageStartupMessages({
  library(Matrix)
  library(LiblineaR)
})

BASE <- 'PATH/Misc/FF_Corpus'
OUT <- file.path(
  BASE,
  'HP_Liebesgeschichte_1000word_chunks_FINAL',
  'POS_tagged',
  'experiment_xpos'
)

n <- as.integer(Sys.getenv(
  'NGRAM_N',
  unset = Sys.getenv('SLURM_ARRAY_TASK_ID', unset = '')
))

if (is.na(n) || !(n %in% 1:5)) {
  stop('Set NGRAM_N or SLURM_ARRAY_TASK_ID to 1,...,5')
}

a <- readRDS(file.path(
  OUT,
  sprintf('features_n%d_df3plus.rds', n)
))

x <- a$x
meta <- a$metadata

if (
  !inherits(x, 'dgCMatrix') ||
  nrow(x) != 8180L ||
  length(a$feature_df) != ncol(x)
) {
  stop('Invalid feature object')
}

train <- which(meta$set == 'train')
test <- which(meta$set == 'test')

if (length(train) != 6135L || length(test) != 2045L) {
  stop('Wrong train/test split')
}

if (anyNA(meta$author_id) || anyNA(meta$text_id)) {
  stop('Missing author or story identifiers')
}

authors <- sort(unique(as.character(meta$author_id)))

if (
  length(authors) != 409L ||
  !setequal(authors, as.character(meta$author_id[test]))
) {
  stop('Expected 409 authors in both sets')
}

y <- match(as.character(meta$author_id), authors)

cat(
  'NGRAM:', n,
  '\nChunks:', nrow(x),
  '\nExtracted features:', ncol(x),
  '\nSparse matrix size:',
  round(as.numeric(object.size(x)) / 1024^3, 3),
  'GiB\n'
)

# Story-grouped CV, deterministic across all 15 conditions.
# Stories are assigned per author.
# Single-story authors are training-only in CV.
# With two stories, one is held out in fold 1, the other in fold 2.
# With three or more, stories cycle over three folds.
# This ensures each author remains represented in every fold's training data.

set.seed(20261031)

fold <- integer(nrow(meta))

for (id in seq_along(authors)) {
  idx <- train[y[train] == id]
  story <- as.character(meta$text_id[idx])
  stories <- sample(unique(story))
  
  if (length(stories) == 1L) next
  
  assignments <- rep(
    seq_len(min(3L, length(stories))),
    length.out = length(stories)
  )
  
  fold[idx] <- assignments[match(story, stories)]
}

if (any(fold[test] != 0L)) {
  stop('Test rows entered cross-validation')
}

# Each story within each author must be assigned to one fold only.

key <- paste(y[train], meta$text_id[train], sep = '::')

if (any(vapply(
  split(fold[train], key),
  function(z) length(unique(z)) != 1L,
  logical(1)
))) {
  stop('Story leakage')
}

fold_info <- data.frame(
  fold = 1:3,
  validation_chunks = vapply(
    1:3,
    function(k) sum(fold[train] == k),
    integer(1)
  ),
  validation_authors = vapply(
    1:3,
    function(k) length(unique(y[train][fold[train] == k])),
    integer(1)
  )
)

print(fold_info)

if (any(fold_info$validation_chunks == 0)) {
  stop('A CV fold has no validation chunks')
}

# LiblineaR type=1:
# L2-regularised, L2-loss linear SVC (multi-class one-vs-rest).
# All feature values are original within-chunk occurrence counts.

C_GRID <- c(0.01, 0.1, 1, 10)
thresholds <- c(3L, 5L, 10L)

results <- list()
predictions <- list()
tuning <- list()

for (df_min in thresholds) {
  
  cat(
    '\nNGRAM:', n,
    ' MIN TRAIN DF:', df_min, '\n'
  )
  
  columns <- which(a$feature_df >= df_min)
  
  if (!length(columns)) {
    stop('No features at threshold ', df_min)
  }
  
  z <- x[, columns, drop = FALSE]
  
  cat(
    'Features retained:', ncol(z),
    '\nSparse matrix size:',
    round(as.numeric(object.size(z)) / 1024^3, 3),
    'GiB\n'
  )
  
  scores <- matrix(
    NA_real_,
    nrow = length(C_GRID),
    ncol = 3,
    dimnames = list(
      as.character(C_GRID),
      paste0('fold', 1:3)
    )
  )
  
  for (ci in seq_along(C_GRID)) {
    
    for (k in 1:3) {
      
      valid <- train[fold[train] == k]
      fitrows <- train[fold[train] != k]
      
      if (
        !length(valid) ||
        length(unique(y[fitrows])) != 409L
      ) {
        stop('Invalid CV split')
      }
      
      cat(
        'Training: C =', C_GRID[ci],
        'fold =', k, '\n'
      )
      
      model <- LiblineaR::LiblineaR(
        data = z[fitrows, , drop = FALSE],
        target = y[fitrows],
        type = 1,
        cost = C_GRID[ci],
        bias = 1,
        verbose = FALSE
      )
      
      # Use the generic predict() function.
      pred <- predict(
        model,
        newx = z[valid, , drop = FALSE],
        decisionValues = FALSE
      )$predictions
      
      scores[ci, k] <- mean(
        as.integer(pred) == y[valid]
      )
      
      rm(model, pred)
      gc(verbose = FALSE)
    }
    
    cat(
      'C =', C_GRID[ci],
      ' CV accuracy =', mean(scores[ci, ]),
      '\n'
    )
  }
  
  # Macro-average across folds.
  # Deterministic first-max tie-break.
  
  best <- which.max(rowMeans(scores))
  chosen_C <- C_GRID[best]
  
  tuning[[as.character(df_min)]] <- data.frame(
    ngram = n,
    min_df = df_min,
    C = C_GRID,
    fold1 = scores[, 1],
    fold2 = scores[, 2],
    fold3 = scores[, 3],
    mean_cv_accuracy = rowMeans(scores)
  )
  
  cat(
    'Selected C:', chosen_C,
    '\nTraining final model\n'
  )
  
  model <- LiblineaR::LiblineaR(
    data = z[train, , drop = FALSE],
    target = y[train],
    type = 1,
    cost = chosen_C,
    bias = 1,
    verbose = FALSE
  )
  
  predicted <- as.integer(
    predict(
      model,
      newx = z[test, , drop = FALSE],
      decisionValues = FALSE
    )$predictions
  )
  
  if (
    length(predicted) != length(test) ||
    anyNA(predicted) ||
    any(!predicted %in% seq_along(authors))
  ) {
    stop('Invalid predictions')
  }
  
  actual <- y[test]
  acc <- mean(predicted == actual)
  
  # Each author has 5 test chunks.
  # Compute per-class F1 without a dense confusion matrix.
  
  tp <- tabulate(
    actual[predicted == actual],
    nbins = length(authors)
  )
  
  fp <- tabulate(
    predicted[predicted != actual],
    nbins = length(authors)
  )
  
  fn <- tabulate(
    actual[predicted != actual],
    nbins = length(authors)
  )
  
  f1 <- ifelse(
    2 * tp + fp + fn > 0,
    2 * tp / (2 * tp + fp + fn),
    0
  )
  
  results[[as.character(df_min)]] <- data.frame(
    ngram = n,
    min_df = df_min,
    features = ncol(z),
    chosen_C = chosen_C,
    cv_accuracy = mean(scores[best, ]),
    test_accuracy = acc,
    macro_f1 = mean(f1),
    test_chunks = length(test),
    correct = sum(predicted == actual)
  )
  
  predictions[[as.character(df_min)]] <- data.frame(
    ngram = n,
    min_df = df_min,
    final_filename = meta$final_filename[test],
    actual_author = authors[actual],
    predicted_author = authors[predicted],
    correct = predicted == actual
  )
  
  cat(
    'TEST accuracy:', acc,
    ' macro F1:', mean(f1),
    '\n'
  )
  
  rm(model, z)
  gc(verbose = FALSE)
}

output <- list(
  results = do.call(rbind, results),
  predictions = do.call(rbind, predictions),
  tuning = do.call(rbind, tuning),
  cv_fold_info = fold_info,
  cv_single_story_authors = sum(vapply(
    split(as.character(meta$text_id[train]), y[train]),
    function(z) length(unique(z)) == 1L,
    logical(1)
  )),
  classifier = 'LiblineaR type=1 linear SVC, raw counts, bias=1',
  C_grid = C_GRID
)

file <- file.path(
  OUT,
  sprintf('results_n%d.rds', n)
)

tmp <- paste0(file, '.tmp')

saveRDS(output, tmp)

if (!file.rename(tmp, file)) {
  stop('Cannot publish results')
}

cat('\nSaved', file, '\n')
print(output$results)

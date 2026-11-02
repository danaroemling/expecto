
#!/usr/bin/env Rscript

# Collect and evaluate the results of the XPOS n-gram
# authorship attribution experiment.
#
# Five n-gram lengths (1–5)
# Three minimum document frequencies (3, 5, 10)
# 15 experimental conditions in total.
#
# This script does not retrain any classifiers.

options(bitmapType = "cairo")

BASE <- "PATH/Misc/FF_Corpus"

OUT <- file.path(
  BASE,
  "HP_Liebesgeschichte_1000word_chunks_FINAL",
  "POS_tagged",
  "experiment_xpos"
)

RESULTS_DIR <- file.path(OUT, "evaluation")

dir.create(
  RESULTS_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

# ------------------------------------------------------------
# 1. Load and validate all five result files
# ------------------------------------------------------------

files <- file.path(
  OUT,
  sprintf("results_n%d.rds", 1:5)
)

missing_files <- files[!file.exists(files)]

if (length(missing_files) > 0L) {
  stop(
    "Missing classification results:\n",
    paste(missing_files, collapse = "\n")
  )
}

runs <- lapply(files, readRDS)

required_components <- c(
  "results",
  "predictions",
  "tuning",
  "cv_fold_info"
)

for (i in seq_along(runs)) {
  
  run <- runs[[i]]
  
  if (!all(required_components %in% names(run))) {
    stop("Missing components in ", basename(files[i]))
  }
  
  if (nrow(run$results) != 3L) {
    stop("Expected three conditions in ", basename(files[i]))
  }
  
  if (!setequal(run$results$min_df, c(3L, 5L, 10L))) {
    stop("Unexpected minimum DF in ", basename(files[i]))
  }
  
  if (!all(run$results$ngram == i)) {
    stop("Incorrect n-gram identifier in ", basename(files[i]))
  }
}

cat("All five classification result files loaded.\n")

# ------------------------------------------------------------
# 2. Combine results
# ------------------------------------------------------------

results <- do.call(
  rbind,
  lapply(runs, function(z) z$results)
)

rownames(results) <- NULL

results <- results[
  order(results$ngram, results$min_df),
]

if (nrow(results) != 15L) {
  stop("Expected exactly 15 experimental conditions")
}

if (anyDuplicated(results[c("ngram", "min_df")])) {
  stop("Duplicate experimental conditions")
}

if (anyNA(results)) {
  stop("Missing values in classification results")
}

if (any(results$test_chunks != 2045L)) {
  stop("Unexpected number of test chunks")
}

if (any(
  abs(
    results$test_accuracy -
    results$correct / results$test_chunks
  ) > 1e-10
)) {
  stop("Test accuracy does not match correct predictions")
}

write.csv(
  results,
  file.path(RESULTS_DIR, "all_results.csv"),
  row.names = FALSE
)

cat("\nALL EXPERIMENTAL CONDITIONS\n")
print(results, row.names = FALSE)

# ------------------------------------------------------------
# 3. Combine chunk-level predictions
# ------------------------------------------------------------

predictions <- do.call(
  rbind,
  lapply(runs, function(z) z$predictions)
)

rownames(predictions) <- NULL

predictions <- predictions[
  order(predictions$ngram, predictions$min_df),
]

if (nrow(predictions) != 15L * 2045L) {
  stop("Unexpected number of prediction rows")
}

if (anyNA(predictions$correct)) {
  stop("Missing correctness indicators")
}

for (i in seq_len(nrow(results))) {
  
  r <- results[i, ]
  
  p <- predictions[
    predictions$ngram == r$ngram &
      predictions$min_df == r$min_df,
  ]
  
  if (nrow(p) != r$test_chunks) {
    stop("Prediction count mismatch")
  }
  
  if (sum(p$correct) != r$correct) {
    stop("Correct prediction count mismatch")
  }
  
  if (any(
    p$correct !=
    (p$actual_author == p$predicted_author)
  )) {
    stop("Inconsistent prediction correctness")
  }
}

write.csv(
  predictions,
  file.path(RESULTS_DIR, "all_predictions.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# 4. Combine hyperparameter tuning results
# ------------------------------------------------------------

tuning <- do.call(
  rbind,
  lapply(runs, `[[`, "tuning")
)

rownames(tuning) <- NULL

tuning <- tuning[
  order(tuning$ngram, tuning$min_df, tuning$C),
]

rownames(tuning) <- NULL

tuning <- tuning[
  order(tuning$ngram, tuning$min_df, tuning$C),
]

if (nrow(tuning) != 60L) {
  stop("Expected 60 tuning records (15 conditions x 4 C)")
}

write.csv(
  tuning,
  file.path(RESULTS_DIR, "all_tuning.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# 5. Identify best-performing conditions
# ------------------------------------------------------------

best_accuracy <- results[
  which.max(results$test_accuracy),
]

best_f1 <- results[
  which.max(results$macro_f1),
]

cat("\nBEST TEST ACCURACY\n")
print(best_accuracy, row.names = FALSE)

cat("\nBEST MACRO-F1\n")
print(best_f1, row.names = FALSE)

# Best condition within each n-gram length

best_by_ngram <- do.call(
  rbind,
  lapply(split(results, results$ngram), function(z) {
    z[which.max(z$test_accuracy), ]
  })
)

rownames(best_by_ngram) <- NULL

write.csv(
  best_by_ngram,
  file.path(RESULTS_DIR, "best_by_ngram.csv"),
  row.names = FALSE
)

cat("\nBEST CONDITION PER N-GRAM LENGTH\n")
print(best_by_ngram, row.names = FALSE)

# ------------------------------------------------------------
# 6. Compare CV and test accuracy
# ------------------------------------------------------------

results$cv_test_difference <- (
  results$test_accuracy - results$cv_accuracy
)

write.csv(
  results,
  file.path(RESULTS_DIR, "all_results_with_cv_difference.csv"),
  row.names = FALSE
)

cat("\nCV VS TEST ACCURACY\n")

print(
  results[
    ,
    c(
      "ngram",
      "min_df",
      "cv_accuracy",
      "test_accuracy",
      "cv_test_difference"
    )
  ],
  row.names = FALSE
)

# ------------------------------------------------------------
# 7. Plot test accuracy by n-gram length
# ------------------------------------------------------------

plot_accuracy <- function() {
  
  df_values <- c(3L, 5L, 10L)
  
  plot(
    NA,
    xlim = c(1, 5),
    ylim = range(results$test_accuracy, 0),
    xlab = "XPOS n-gram length",
    ylab = "Test accuracy",
    main = "Authorship attribution accuracy",
    xaxt = "n"
  )
  
  axis(1, at = 1:5)
  
  line_types <- c(1, 2, 3)
  point_types <- c(16, 17, 15)
  
  for (i in seq_along(df_values)) {
    
    d <- results[
      results$min_df == df_values[i],
    ]
    
    d <- d[order(d$ngram), ]
    
    lines(
      d$ngram,
      d$test_accuracy,
      type = "b",
      lty = line_types[i],
      pch = point_types[i],
      lwd = 2
    )
  }
  
  legend(
    "topleft",
    legend = paste("Min. DF", df_values),
    lty = line_types,
    pch = point_types,
    lwd = 2,
    bty = "n"
  )
}

png(
  file.path(RESULTS_DIR, "test_accuracy.png"),
  width = 1200,
  height = 800,
  res = 150
)

plot_accuracy()
dev.off()

pdf(
  file.path(RESULTS_DIR, "test_accuracy.pdf"),
  width = 8,
  height = 5.5
)

plot_accuracy()
dev.off()

# ------------------------------------------------------------
# 8. Plot macro-F1 by n-gram length
# ------------------------------------------------------------

plot_f1 <- function() {
  
  df_values <- c(3L, 5L, 10L)
  
  plot(
    NA,
    xlim = c(1, 5),
    ylim = range(results$macro_f1, 0),
    xlab = "XPOS n-gram length",
    ylab = "Macro-F1",
    main = "Authorship attribution macro-F1",
    xaxt = "n"
  )
  
  axis(1, at = 1:5)
  
  line_types <- c(1, 2, 3)
  point_types <- c(16, 17, 15)
  
  for (i in seq_along(df_values)) {
    
    d <- results[
      results$min_df == df_values[i],
    ]
    
    d <- d[order(d$ngram), ]
    
    lines(
      d$ngram,
      d$macro_f1,
      type = "b",
      lty = line_types[i],
      pch = point_types[i],
      lwd = 2
    )
  }
  
  legend(
    "topleft",
    legend = paste("Min. DF", df_values),
    lty = line_types,
    pch = point_types,
    lwd = 2,
    bty = "n"
  )
}

png(
  file.path(RESULTS_DIR, "macro_f1.png"),
  width = 1200,
  height = 800,
  res = 150
)

plot_f1()
dev.off()

pdf(
  file.path(RESULTS_DIR, "macro_f1.pdf"),
  width = 8,
  height = 5.5
)

plot_f1()
dev.off()

# ------------------------------------------------------------
# 9. Save combined R object
# ------------------------------------------------------------

evaluation <- list(
  results = results,
  predictions = predictions,
  tuning = tuning,
  best_accuracy = best_accuracy,
  best_f1 = best_f1,
  best_by_ngram = best_by_ngram,
  cv_fold_info = lapply(runs, function(z) z$cv_fold_info)
)

saveRDS(
  evaluation,
  file.path(RESULTS_DIR, "evaluation.rds")
)

cat("\nEvaluation completed successfully.\n")
cat("Output directory:", RESULTS_DIR, "\n")

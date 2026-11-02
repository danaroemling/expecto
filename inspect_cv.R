
#!/usr/bin/env Rscript

BASE <- "PATH/Misc/FF_Corpus"

OUT <- file.path(
  BASE,
  "HP_Liebesgeschichte_1000word_chunks_FINAL",
  "POS_tagged",
  "experiment_xpos"
)

x <- readRDS(file.path(OUT, "results_n3.rds"))

cat("\n===== SAVED OBJECT STRUCTURE =====\n")
print(names(x))

cat("\n===== CV FOLD INFORMATION =====\n")
print(x$cv_fold_info)

cat("\n===== SINGLE-STORY AUTHORS =====\n")
print(x$cv_single_story_authors)

cat("\n===== TUNING DATA STRUCTURE =====\n")
str(x$tuning)

cat("\n===== TUNING RESULTS =====\n")
print(x$tuning)

cat("\n===== CLASSIFICATION RESULTS =====\n")
print(x$results)

cat("\n===== PREDICTION DATA STRUCTURE =====\n")
str(x$predictions)

cat("\n===== PREDICTION EXAMPLE =====\n")
print(utils::head(x$predictions, 5))

cat("\n===== FEATURE MATRIX METADATA =====\n")

feature_file <- file.path(
  OUT,
  "features_n3_df3plus.rds"
)

if (file.exists(feature_file)) {
  
  f <- readRDS(feature_file)
  
  cat("\nMetadata columns:\n")
  print(names(f$metadata))
  
  cat("\nTraining and test counts:\n")
  print(table(f$metadata$set, useNA = "ifany"))
  
  cat("\nTraining stories per author:\n")
  
  m <- f$metadata[f$metadata$set == "train", ]
  
  if (all(c("author_id", "text_id") %in% names(m))) {
    
    stories <- unique(m[c("author_id", "text_id")])
    
    counts <- as.data.frame(
      table(stories$author_id),
      stringsAsFactors = FALSE
    )
    
    names(counts) <- c("author_id", "n_stories")
    
    counts$n_stories <- as.integer(counts$n_stories)
    
    print(table(counts$n_stories))
    
    cat(
      "\nAuthors with one training story:",
      sum(counts$n_stories == 1L),
      "\n"
    )
    
  } else {
    cat("Required metadata columns not found.\n")
  }
  
} else {
  cat("Feature file not found.\n")
}

cat("\nDiagnostic completed.\n")

# ============================================================
# tag_final_corpus.R
#
# POS TAGGING OF FINAL FANFICTION CORPUS
#
# Input:
#   HP_Liebesgeschichte_1000word_chunks_FINAL/final_chunks.rds
#
# Model:
#   german-hdt-ud-2.5-191206.udpipe
#
# Output:
#   HP_Liebesgeschichte_1000word_chunks_FINAL/POS_tagged/
#
# DESIGN:
#   - 409 authors
#   - 8,180 chunks
#   - 9,584,239 tokens including punctuation
#   - existing tokenisation is PRESERVED
#   - UDPipe does NOT re-tokenise
#   - XPOS is retained for later n-grams
#   - UPOS and FEATS are also retained
#
# MODES:
#   MODE=worker
#       Run as SLURM array 1-50
#
#   MODE=assemble
#       Run once after all 50 workers finish
#
# Can be run from a fresh R session.
# ============================================================


# ============================================================
# 0. LIBRARIES
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(udpipe)
})

options(scipen = 999)


# ============================================================
# 1. SETTINGS
# ============================================================

BASE <- "PATH/Misc/FF_Corpus"

FINAL_DIR <- file.path(
  BASE,
  "HP_Liebesgeschichte_1000word_chunks_FINAL"
)

FINAL_RDS <- file.path(
  FINAL_DIR,
  "final_chunks.rds"
)

MODEL_FILE <- file.path(
  BASE,
  "german-hdt-ud-2.5-191206.udpipe"
)

TAG_DIR <- file.path(
  FINAL_DIR,
  "POS_tagged"
)

PARTS_DIR <- file.path(
  TAG_DIR,
  "parts"
)

TAGGED_RDS <- file.path(
  TAG_DIR,
  "tagged_tokens.rds"
)

TAGGED_CHUNKS_RDS <- file.path(
  TAG_DIR,
  "tagged_chunks.rds"
)

VALIDATION_FILE <- file.path(
  TAG_DIR,
  "tagging_validation.csv"
)


N_TASKS <- 50L

EXPECTED_AUTHORS <- 409L

EXPECTED_CHUNKS <- 8180L

EXPECTED_TRAIN <- 6135L

EXPECTED_TEST <- 2045L

EXPECTED_WORDS <- 8180000L

EXPECTED_PUNCT <- 1404239L

EXPECTED_TOKENS <- 9584239L


MODE <- Sys.getenv(
  "MODE",
  unset = ""
)


# ============================================================
# 2. BASIC INPUT CHECKS
# ============================================================

if (!file.exists(FINAL_RDS)) {
  
  stop(
    paste(
      "Cannot find final corpus:",
      FINAL_RDS
    )
  )
}


if (!file.exists(MODEL_FILE)) {
  
  stop(
    paste(
      "Cannot find UDPipe model:",
      MODEL_FILE
    )
  )
}


if (
  !MODE %in%
  c(
    "worker",
    "assemble"
  )
) {
  
  stop(
    paste0(
      "MODE must be either 'worker' or 'assemble'.\n",
      "Current MODE: ",
      MODE
    )
  )
}


# ============================================================
# 3. LOAD FINAL CORPUS
# ============================================================

cat("\n========================================\n")
cat("FINAL CORPUS POS TAGGING\n")
cat("========================================\n")


cat(
  "Mode:",
  MODE,
  "\n"
)


cat(
  "\nLoading final corpus...\n"
)


chunks <- readRDS(
  FINAL_RDS
)


required_columns <- c(
  "author_id",
  "author_username",
  "set",
  "final_filename",
  "text_id",
  "story_chunk",
  "split_chunk",
  "words",
  "punctuation_tokens",
  "total_tokens",
  "text"
)


missing_columns <- setdiff(
  required_columns,
  names(chunks)
)


if (
  length(
    missing_columns
  ) > 0L
) {
  
  stop(
    paste(
      "Final corpus is missing columns:",
      paste(
        missing_columns,
        collapse = ", "
      )
    )
  )
}


# ============================================================
# 4. VALIDATE INPUT CORPUS
# ============================================================

if (
  nrow(chunks) !=
  EXPECTED_CHUNKS
) {
  
  stop(
    paste(
      "Expected",
      EXPECTED_CHUNKS,
      "chunks but found",
      nrow(chunks)
    )
  )
}


if (
  n_distinct(
    chunks$author_id
  ) != EXPECTED_AUTHORS
) {
  
  stop(
    paste(
      "Expected",
      EXPECTED_AUTHORS,
      "authors but found",
      n_distinct(
        chunks$author_id
      )
    )
  )
}


if (
  sum(
    chunks$set == "train"
  ) != EXPECTED_TRAIN
) {
  
  stop(
    "Training chunk count is wrong."
  )
}


if (
  sum(
    chunks$set == "test"
  ) != EXPECTED_TEST
) {
  
  stop(
    "Test chunk count is wrong."
  )
}


if (
  sum(
    chunks$words
  ) != EXPECTED_WORDS
) {
  
  stop(
    "Word count is wrong."
  )
}


if (
  sum(
    chunks$punctuation_tokens
  ) != EXPECTED_PUNCT
) {
  
  stop(
    "Punctuation-token count is wrong."
  )
}


if (
  sum(
    chunks$total_tokens
  ) != EXPECTED_TOKENS
) {
  
  stop(
    "Total token count is wrong."
  )
}


cat(
  "Authors:",
  n_distinct(
    chunks$author_id
  ),
  "\n"
)

cat(
  "Chunks:",
  nrow(chunks),
  "\n"
)

cat(
  "Word tokens:",
  sum(chunks$words),
  "\n"
)

cat(
  "Punctuation tokens:",
  sum(chunks$punctuation_tokens),
  "\n"
)

cat(
  "Total tokens:",
  sum(chunks$total_tokens),
  "\n"
)


# ============================================================
# 5. CREATE OUTPUT DIRECTORIES
# ============================================================

dir.create(
  TAG_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)


dir.create(
  PARTS_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)


# ============================================================
# 6. WORKER MODE
# ============================================================

if (
  MODE == "worker"
) {
  
  
  # ----------------------------------------------------------
  # 6.1 SLURM task ID
  # ----------------------------------------------------------
  
  TASK_ID <- as.integer(
    Sys.getenv(
      "SLURM_ARRAY_TASK_ID"
    )
  )
  
  
  if (
    is.na(TASK_ID) ||
    TASK_ID < 1L ||
    TASK_ID > N_TASKS
  ) {
    
    stop(
      paste(
        "SLURM_ARRAY_TASK_ID must be between 1 and",
        N_TASKS
      )
    )
  }
  
  
  cat("\n========================================\n")
  cat("TAGGING WORKER\n")
  cat("========================================\n")
  
  cat(
    "Task:",
    TASK_ID,
    "of",
    N_TASKS,
    "\n"
  )
  
  
  # ----------------------------------------------------------
  # 6.2 Deterministically assign chunks to workers
  #
  # Every chunk belongs permanently to one of the 50 tasks.
  # ----------------------------------------------------------
  
  chunks <- chunks %>%
    
    arrange(
      author_id,
      set,
      split_chunk,
      final_filename
    ) %>%
    
    mutate(
      task =
        ((row_number() - 1L) %% N_TASKS) + 1L
    )
  
  
  my_chunks <- chunks %>%
    
    filter(
      task == TASK_ID
    )
  
  
  cat(
    "Chunks assigned:",
    nrow(my_chunks),
    "\n"
  )
  
  
  cat(
    "Expected input tokens:",
    sum(
      my_chunks$total_tokens
    ),
    "\n"
  )
  
  
  if (
    nrow(my_chunks) == 0L
  ) {
    
    stop(
      "This worker has no chunks."
    )
  }
  
  
  # ----------------------------------------------------------
  # 6.3 Load UDPipe model
  # ----------------------------------------------------------
  
  cat(
    "\nLoading UDPipe model...\n"
  )
  
  
  model <- udpipe_load_model(
    MODEL_FILE
  )
  
  
  cat(
    "Model loaded.\n"
  )
  
  
  # ----------------------------------------------------------
  # 6.4 Tag one already-tokenised chunk
  #
  # IMPORTANT:
  #
  # The corpus text is already stored as:
  #
  #   token token , token . token ...
  #
  # We split on whitespace to recover EXACTLY those tokens.
  #
  # We then give UDPipe one token per line and use:
  #
  #   tokenizer = "vertical"
  #
  # Therefore UDPipe performs POS tagging but DOES NOT
  # perform a new linguistic tokenisation of the text.
  #
  # Punctuation remains present as tokens.
  # ----------------------------------------------------------
  
  tag_chunk <- function(
    chunk_text,
    expected_tokens,
    filename) {
    
    
    input_tokens <- strsplit(
      chunk_text,
      "[[:space:]]+"
    )[[1]]
    
    
    input_tokens <- input_tokens[
      nzchar(
        input_tokens
      )
    ]
    
    
    if (
      length(input_tokens) !=
      expected_tokens
    ) {
      
      stop(
        paste0(
          "\nINPUT TOKEN COUNT ERROR\n",
          "Chunk: ",
          filename,
          "\n",
          "Expected: ",
          expected_tokens,
          "\n",
          "Recovered: ",
          length(input_tokens),
          "\n"
        )
      )
    }
    
    
    # Vertical input:
    # one pre-existing token per line.
    
    vertical_text <- paste(
      input_tokens,
      collapse = "\n"
    )
    
    
    ann <- udpipe_annotate(
      
      model,
      
      x =
        vertical_text,
      
      tokenizer =
        "vertical",
      
      tagger =
        "default",
      
      parser =
        "none"
    )
    
    
    ann <- as.data.frame(
      ann,
      stringsAsFactors = FALSE
    )
    
    
    # Defensive removal of any multiword-token range rows.
    
    if (
      "token_id" %in%
      names(ann)
    ) {
      
      ann <- ann[
        
        !grepl(
          "-",
          as.character(
            ann$token_id
          )
        ),
        
        ,
        
        drop = FALSE
      ]
    }
    
    
    ann$token <- as.character(
      ann$token
    )
    
    
    # --------------------------------------------------------
    # Absolutely crucial validation:
    #
    # same number of tokens
    # AND
    # same token sequence
    # --------------------------------------------------------
    
    if (
      nrow(ann) !=
      length(input_tokens)
    ) {
      
      stop(
        paste0(
          "\nUDPIPE CHANGED TOKEN COUNT\n",
          "Chunk: ",
          filename,
          "\n",
          "Input tokens: ",
          length(input_tokens),
          "\n",
          "Tagged tokens: ",
          nrow(ann),
          "\n"
        )
      )
    }
    
    
    if (
      !identical(
        as.character(
          ann$token
        ),
        as.character(
          input_tokens
        )
      )
    ) {
      
      bad <- which(
        as.character(
          ann$token
        ) !=
          as.character(
            input_tokens
          )
      )
      
      
      first_bad <- if (
        length(bad) > 0L
      ) {
        bad[1]
      } else {
        NA_integer_
      }
      
      
      stop(
        paste0(
          "\nUDPIPE CHANGED TOKEN SEQUENCE\n",
          "Chunk: ",
          filename,
          "\n",
          "First mismatch: ",
          first_bad,
          "\n"
        )
      )
    }
    
    
    # --------------------------------------------------------
    # Keep only fields useful for later analysis.
    #
    # XPOS is the primary feature representation.
    #
    # UPOS and FEATS are retained so that we do not need
    # to tag the corpus again if we later want them.
    # --------------------------------------------------------
    
    tibble(
      
      token_position =
        seq_len(
          nrow(ann)
        ),
      
      token =
        as.character(
          ann$token
        ),
      
      lemma =
        as.character(
          ann$lemma
        ),
      
      upos =
        as.character(
          ann$upos
        ),
      
      xpos =
        as.character(
          ann$xpos
        ),
      
      feats =
        as.character(
          ann$feats
        )
    )
  }
  
  
  # ----------------------------------------------------------
  # 6.5 Tag assigned chunks
  # ----------------------------------------------------------
  
  results <- vector(
    "list",
    nrow(my_chunks)
  )
  
  
  for (
    i in seq_len(
      nrow(my_chunks)
    )
  ) {
    
    
    row <- my_chunks[
      i,
    ]
    
    
    cat(
      "[",
      i,
      "/",
      nrow(my_chunks),
      "]",
      row$final_filename,
      "\n"
    )
    
    
    tagged <- tag_chunk(
      
      chunk_text =
        row$text,
      
      expected_tokens =
        as.integer(
          row$total_tokens
        ),
      
      filename =
        row$final_filename
    )
    
    
    tagged <- tagged %>%
      
      mutate(
        
        author_id =
          row$author_id,
        
        author_username =
          row$author_username,
        
        set =
          row$set,
        
        final_filename =
          row$final_filename,
        
        text_id =
          as.character(
            row$text_id
          ),
        
        story_chunk =
          as.integer(
            row$story_chunk
          ),
        
        split_chunk =
          as.integer(
            row$split_chunk
          ),
        
        .before =
          token_position
      )
    
    
    results[[i]] <- tagged
    
    
    if (
      i %% 25L == 0L ||
      i == nrow(my_chunks)
    ) {
      
      gc()
    }
  }
  
  
  # ----------------------------------------------------------
  # 6.6 Combine worker output
  # ----------------------------------------------------------
  
  tagged_part <- bind_rows(
    results
  )
  
  
  rm(results)
  
  gc()
  
  
  expected_part_tokens <- sum(
    my_chunks$total_tokens
  )
  
  
  observed_part_tokens <- nrow(
    tagged_part
  )
  
  
  if (
    observed_part_tokens !=
    expected_part_tokens
  ) {
    
    stop(
      paste(
        "Worker token count failed.",
        "Expected:",
        expected_part_tokens,
        "Observed:",
        observed_part_tokens
      )
    )
  }
  
  
  # Every assigned chunk must be present exactly once.
  
  chunk_check <- tagged_part %>%
    
    distinct(
      final_filename
    )
  
  
  if (
    nrow(chunk_check) !=
    nrow(my_chunks)
  ) {
    
    stop(
      "Worker chunk count failed."
    )
  }
  
  
  # XPOS should not be entirely absent.
  
  if (
    all(
      is.na(
        tagged_part$xpos
      ) |
      tagged_part$xpos == ""
    )
  ) {
    
    stop(
      "No XPOS tags were produced."
    )
  }
  
  
  # ----------------------------------------------------------
  # 6.7 Atomic save
  # ----------------------------------------------------------
  
  output_file <- file.path(
    
    PARTS_DIR,
    
    sprintf(
      "tagged_part_%03d.rds",
      TASK_ID
    )
  )
  
  
  temp_file <- paste0(
    output_file,
    ".tmp"
  )
  
  
  saveRDS(
    tagged_part,
    temp_file
  )
  
  
  # Read temporary file back before accepting it.
  
  check_part <- readRDS(
    temp_file
  )
  
  
  if (
    nrow(check_part) !=
    expected_part_tokens
  ) {
    
    stop(
      "Temporary RDS validation failed."
    )
  }
  
  
  if (
    file.exists(
      output_file
    )
  ) {
    
    unlink(
      output_file
    )
  }
  
  
  renamed <- file.rename(
    temp_file,
    output_file
  )
  
  
  if (!renamed) {
    
    stop(
      "Could not rename temporary RDS to final worker RDS."
    )
  }
  
  
  cat("\n========================================\n")
  cat("WORKER COMPLETE\n")
  cat("========================================\n")
  
  
  cat(
    "Task:",
    TASK_ID,
    "\n"
  )
  
  
  cat(
    "Chunks:",
    nrow(my_chunks),
    "\n"
  )
  
  
  cat(
    "Tagged tokens:",
    nrow(tagged_part),
    "\n"
  )
  
  
  cat(
    "Output:",
    output_file,
    "\n"
  )
  
  
  quit(
    save = "no",
    status = 0
  )
}


# ============================================================
# 7. ASSEMBLE MODE
# ============================================================

if (
  MODE == "assemble"
) {
  
  
  cat("\n========================================\n")
  cat("ASSEMBLING TAGGED CORPUS\n")
  cat("========================================\n")
  
  
  # ----------------------------------------------------------
  # 7.1 Require all 50 worker files
  # ----------------------------------------------------------
  
  expected_files <- file.path(
    
    PARTS_DIR,
    
    sprintf(
      "tagged_part_%03d.rds",
      seq_len(
        N_TASKS
      )
    )
  )
  
  
  missing_files <- expected_files[
    !file.exists(
      expected_files
    )
  ]
  
  
  if (
    length(
      missing_files
    ) > 0L
  ) {
    
    stop(
      paste0(
        "Assembly aborted. Missing worker files:\n",
        paste(
          basename(
            missing_files
          ),
          collapse = "\n"
        )
      )
    )
  }
  
  
  cat(
    "All",
    N_TASKS,
    "worker files found.\n"
  )
  
  
  # ----------------------------------------------------------
  # 7.2 Load parts
  # ----------------------------------------------------------
  
  cat(
    "\nLoading worker files...\n"
  )
  
  
  tagged <- bind_rows(
    lapply(
      expected_files,
      readRDS
    )
  )
  
  
  cat(
    "Tagged rows loaded:",
    nrow(tagged),
    "\n"
  )
  
  
  # ----------------------------------------------------------
  # 7.3 Sort corpus into stable order
  # ----------------------------------------------------------
  
  tagged <- tagged %>%
    
    arrange(
      author_id,
      set,
      split_chunk,
      final_filename,
      token_position
    )
  
  
  # ----------------------------------------------------------
  # 7.4 Validate total token count
  # ----------------------------------------------------------
  
  if (
    nrow(tagged) !=
    EXPECTED_TOKENS
  ) {
    
    stop(
      paste(
        "Expected",
        EXPECTED_TOKENS,
        "tagged tokens but found",
        nrow(tagged)
      )
    )
  }
  
  
  # ----------------------------------------------------------
  # 7.5 Validate chunks
  # ----------------------------------------------------------
  
  tagged_chunk_counts <- tagged %>%
    
    count(
      final_filename,
      name = "tagged_tokens"
    )
  
  
  expected_chunk_counts <- chunks %>%
    
    select(
      final_filename,
      expected_tokens =
        total_tokens
    )
  
  
  chunk_validation <- expected_chunk_counts %>%
    
    left_join(
      tagged_chunk_counts,
      by = "final_filename"
    )
  
  
  if (
    any(
      is.na(
        chunk_validation$tagged_tokens
      )
    )
  ) {
    
    stop(
      "At least one final chunk is missing from tagged output."
    )
  }
  
  
  if (
    any(
      chunk_validation$expected_tokens !=
      chunk_validation$tagged_tokens
    )
  ) {
    
    stop(
      "At least one chunk changed token count during tagging."
    )
  }
  
  
  if (
    nrow(
      chunk_validation
    ) != EXPECTED_CHUNKS
  ) {
    
    stop(
      "Tagged chunk count is wrong."
    )
  }
  
  
  # ----------------------------------------------------------
  # 7.6 Check duplicate token positions
  # ----------------------------------------------------------
  
  duplicate_positions <- tagged %>%
    
    count(
      final_filename,
      token_position,
      name = "n"
    ) %>%
    
    filter(
      n > 1L
    )
  
  
  if (
    nrow(
      duplicate_positions
    ) > 0L
  ) {
    
    stop(
      "Duplicate token positions found."
    )
  }
  
  
  # ----------------------------------------------------------
  # 7.7 Check XPOS
  # ----------------------------------------------------------
  
  missing_xpos <- sum(
    is.na(
      tagged$xpos
    ) |
      tagged$xpos == ""
  )
  
  
  xpos_types <- n_distinct(
    tagged$xpos[
      !is.na(tagged$xpos) &
        tagged$xpos != ""
    ]
  )
  
  
  cat(
    "Missing/empty XPOS tags:",
    missing_xpos,
    "\n"
  )
  
  
  cat(
    "Distinct XPOS tags:",
    xpos_types,
    "\n"
  )
  
  
  if (
    missing_xpos ==
    nrow(tagged)
  ) {
    
    stop(
      "XPOS tagging failed completely."
    )
  }
  
  
  # ----------------------------------------------------------
  # 7.8 Explicit punctuation check
  #
  # We do NOT infer punctuation from the POS tag here.
  # Instead, we check that the total input token stream was
  # preserved exactly.
  #
  # Since the original corpus contains 1,404,239 punctuation
  # tokens and total token count is unchanged, punctuation
  # cannot have disappeared.
  # ----------------------------------------------------------
  
  token_count_preserved <-
    nrow(tagged) ==
    sum(
      chunks$total_tokens
    )
  
  
  if (
    !token_count_preserved
  ) {
    
    stop(
      "Original token stream was not preserved."
    )
  }
  
  
  # ----------------------------------------------------------
  # 7.9 Create chunk-level tagged representation
  #
  # This is particularly convenient for later POS n-gram
  # extraction.
  #
  # xpos_sequence contains every XPOS tag in token order,
  # INCLUDING the tags assigned to punctuation.
  # ----------------------------------------------------------
  
  cat(
    "\nCreating chunk-level XPOS sequences...\n"
  )
  
  
  tagged_chunks <- tagged %>%
    
    group_by(
      author_id,
      author_username,
      set,
      final_filename,
      text_id,
      story_chunk,
      split_chunk
    ) %>%
    
    summarise(
      
      tagged_tokens =
        n(),
      
      xpos_sequence =
        paste(
          xpos,
          collapse = " "
        ),
      
      upos_sequence =
        paste(
          upos,
          collapse = " "
        ),
      
      .groups =
        "drop"
    ) %>%
    
    arrange(
      author_id,
      set,
      split_chunk
    )
  
  
  if (
    nrow(tagged_chunks) !=
    EXPECTED_CHUNKS
  ) {
    
    stop(
      "Chunk-level tagged corpus has wrong number of chunks."
    )
  }
  
  
  # ----------------------------------------------------------
  # 7.10 Validation table
  # ----------------------------------------------------------
  
  validation <- tibble(
    
    check = c(
      "authors",
      "chunks",
      "train_chunks",
      "test_chunks",
      "input_words",
      "input_punctuation_tokens",
      "input_total_tokens",
      "tagged_total_tokens",
      "token_count_preserved",
      "all_chunks_present",
      "duplicate_token_positions",
      "xpos_tags_present"
    ),
    
    expected = c(
      "409",
      "8180",
      "6135",
      "2045",
      "8180000",
      "1404239",
      "9584239",
      "9584239",
      "TRUE",
      "TRUE",
      "0",
      "TRUE"
    ),
    
    observed = c(
      
      as.character(
        n_distinct(
          tagged$author_id
        )
      ),
      
      as.character(
        n_distinct(
          tagged$final_filename
        )
      ),
      
      as.character(
        n_distinct(
          tagged$final_filename[
            tagged$set == "train"
          ]
        )
      ),
      
      as.character(
        n_distinct(
          tagged$final_filename[
            tagged$set == "test"
          ]
        )
      ),
      
      as.character(
        sum(
          chunks$words
        )
      ),
      
      as.character(
        sum(
          chunks$punctuation_tokens
        )
      ),
      
      as.character(
        sum(
          chunks$total_tokens
        )
      ),
      
      as.character(
        nrow(tagged)
      ),
      
      as.character(
        token_count_preserved
      ),
      
      as.character(
        nrow(
          chunk_validation
        ) ==
          EXPECTED_CHUNKS &&
          all(
            chunk_validation$expected_tokens ==
              chunk_validation$tagged_tokens
          )
      ),
      
      as.character(
        nrow(
          duplicate_positions
        )
      ),
      
      as.character(
        missing_xpos <
          nrow(tagged)
      )
    ),
    
    passed = c(
      
      n_distinct(
        tagged$author_id
      ) == EXPECTED_AUTHORS,
      
      n_distinct(
        tagged$final_filename
      ) == EXPECTED_CHUNKS,
      
      n_distinct(
        tagged$final_filename[
          tagged$set == "train"
        ]
      ) == EXPECTED_TRAIN,
      
      n_distinct(
        tagged$final_filename[
          tagged$set == "test"
        ]
      ) == EXPECTED_TEST,
      
      sum(
        chunks$words
      ) == EXPECTED_WORDS,
      
      sum(
        chunks$punctuation_tokens
      ) == EXPECTED_PUNCT,
      
      sum(
        chunks$total_tokens
      ) == EXPECTED_TOKENS,
      
      nrow(tagged) ==
        EXPECTED_TOKENS,
      
      token_count_preserved,
      
      nrow(
        chunk_validation
      ) ==
        EXPECTED_CHUNKS &&
        all(
          chunk_validation$expected_tokens ==
            chunk_validation$tagged_tokens
        ),
      
      nrow(
        duplicate_positions
      ) == 0L,
      
      missing_xpos <
        nrow(tagged)
    )
  )
  
  
  cat("\n========================================\n")
  cat("TAGGING VALIDATION\n")
  cat("========================================\n")
  
  
  print(
    validation,
    n = Inf
  )
  
  
  if (
    !all(
      validation$passed
    )
  ) {
    
    stop(
      "TAGGING VALIDATION FAILED."
    )
  }
  
  
  # ----------------------------------------------------------
  # 7.11 Save complete tagged corpus
  # ----------------------------------------------------------
  
  cat(
    "\nSaving complete token-level tagged corpus...\n"
  )
  
  
  saveRDS(
    tagged,
    TAGGED_RDS,
    compress = FALSE
  )
  
  
  cat(
    "Saving chunk-level tagged corpus...\n"
  )
  
  
  saveRDS(
    tagged_chunks,
    TAGGED_CHUNKS_RDS,
    compress = FALSE
  )
  
  
  write.csv(
    validation,
    VALIDATION_FILE,
    row.names = FALSE
  )
  
  
  # ----------------------------------------------------------
  # 7.12 Read back
  # ----------------------------------------------------------
  
  cat(
    "\nReading saved files back...\n"
  )
  
  
  check_tagged <- readRDS(
    TAGGED_RDS
  )
  
  
  check_chunks <- readRDS(
    TAGGED_CHUNKS_RDS
  )
  
  
  if (
    nrow(check_tagged) !=
    EXPECTED_TOKENS
  ) {
    
    stop(
      "Read-back validation failed for tagged_tokens.rds."
    )
  }
  
  
  if (
    nrow(check_chunks) !=
    EXPECTED_CHUNKS
  ) {
    
    stop(
      "Read-back validation failed for tagged_chunks.rds."
    )
  }
  
  
  # ----------------------------------------------------------
  # 7.13 Final report
  # ----------------------------------------------------------
  
  cat("\n========================================\n")
  cat("POS TAGGING SUCCESSFULLY COMPLETED\n")
  cat("========================================\n")
  
  
  cat(
    "Authors:",
    n_distinct(
      tagged$author_id
    ),
    "\n"
  )
  
  
  cat(
    "Chunks:",
    n_distinct(
      tagged$final_filename
    ),
    "\n"
  )
  
  
  cat(
    "Tagged tokens:",
    nrow(tagged),
    "\n"
  )
  
  
  cat(
    "Distinct XPOS tags:",
    xpos_types,
    "\n"
  )
  
  
  cat(
    "Missing/empty XPOS:",
    missing_xpos,
    "\n"
  )
  
  
  cat(
    "\nToken-level output:\n",
    TAGGED_RDS,
    "\n"
  )
  
  
  cat(
    "\nChunk-level output:\n",
    TAGGED_CHUNKS_RDS,
    "\n"
  )
  
  
  cat("\n========================================\n")
  cat("DONE\n")
  cat("========================================\n")
}
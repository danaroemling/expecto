# ============================================================
# create_final_corpus_with_punctuation.R
#
# Darmstadt Fanfiction Corpus
# Harry Potter × Liebesgeschichte
#
# COMPLETE FRESH CORPUS CREATION
#
# INPUTS:
#   metadata_merged.csv
#   counts_parts/
#   Texte_txt_all/
#   german-hdt UDPipe model
#
# OUTPUT:
#   HP_Liebesgeschichte_1000word_chunks_FINAL/
#
# DESIGN:
#   409 authors
#   20 chunks per author
#   15 training chunks
#   5 test chunks
#
# CHUNK DEFINITION:
#   exactly 1,000 WORD tokens
#   PLUS all punctuation tokens occurring between the
#   first and 1,000th word-token boundaries.
#
# Punctuation DOES NOT count towards the 1,000 words.
#
# No existing manifests, chunk_parts or reconstruction
# files are used.
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

CORPUS_DIR <- file.path(
  BASE,
  "Texte_txt_all"
)

COUNTS_DIR <- file.path(
  BASE,
  "counts_parts"
)

META_FILE <- file.path(
  BASE,
  "metadata_merged.csv"
)

MODEL_FILE <- file.path(
  BASE,
  "german-hdt-ud-2.5-191206.udpipe"
)

FINAL_DIR <- file.path(
  BASE,
  "HP_Liebesgeschichte_1000word_chunks_FINAL"
)

TRAIN_DIR <- file.path(
  FINAL_DIR,
  "train"
)

TEST_DIR <- file.path(
  FINAL_DIR,
  "test"
)


TARGET_FANDOM <- "Harry Potter"

TARGET_GENRE <- "Liebesgeschichte"


CHUNK_SIZE <- 1000L

N_TRAIN <- 15L

N_TEST <- 5L

N_TOTAL <- N_TRAIN + N_TEST


EXPECTED_AUTHORS <- 409L

EXPECTED_TRAIN_CHUNKS <- 6135L

EXPECTED_TEST_CHUNKS <- 2045L

EXPECTED_TOTAL_CHUNKS <- 8180L

EXPECTED_WORDS <- 8180000L


# Same seeds as original final assembly.

TRAIN_SEED <- 20261027

TEST_SEED <- 20261028


cat("\n========================================\n")
cat("FRESH FINAL CORPUS CREATION\n")
cat("WITH PUNCTUATION\n")
cat("========================================\n")


# ============================================================
# 2. CHECK REQUIRED INPUTS
# ============================================================

if (!dir.exists(CORPUS_DIR)) {
  stop(
    paste(
      "Corpus directory not found:",
      CORPUS_DIR
    )
  )
}


if (!dir.exists(COUNTS_DIR)) {
  stop(
    paste(
      "Count directory not found:",
      COUNTS_DIR
    )
  )
}


if (!file.exists(META_FILE)) {
  stop(
    paste(
      "Metadata file not found:",
      META_FILE
    )
  )
}


if (!file.exists(MODEL_FILE)) {
  stop(
    paste(
      "UDPipe model not found:",
      MODEL_FILE
    )
  )
}


# ============================================================
# 3. LOAD METADATA
# ============================================================

cat("\nLoading metadata...\n")


metadata <- read.csv(
  META_FILE,
  header = TRUE,
  fileEncoding = "latin1",
  stringsAsFactors = FALSE
) %>%
  mutate(
    text_id = as.character(text_id)
  )


cat(
  "Metadata rows:",
  nrow(metadata),
  "\n"
)


# ============================================================
# 4. RECREATE ORIGINAL SUBCORPUS
#
# Original definition:
#
# fandom contains:
#   Harry Potter
#
# genre includes:
#   Liebesgeschichte
#   Romanze
#   Romance
#
# Romanze / Romance are harmonised to Liebesgeschichte.
# ============================================================

cat("\nRecreating Harry Potter × Liebesgeschichte subcorpus...\n")


subcorpus_meta <- metadata %>%
  
  separate_rows(
    genre,
    sep = ",\\s*"
  ) %>%
  
  mutate(
    
    genre = trimws(
      genre
    ),
    
    genre = case_when(
      
      genre %in% c(
        "Romanze",
        "Romance"
      ) ~ "Liebesgeschichte",
      
      TRUE ~ genre
    )
  ) %>%
  
  filter(
    
    str_detect(
      fandom_categories,
      fixed(
        TARGET_FANDOM
      )
    ),
    
    genre ==
      TARGET_GENRE
  ) %>%
  
  distinct(
    text_id,
    .keep_all = TRUE
  )


cat("\n========================================\n")
cat("SUBCORPUS\n")
cat("========================================\n")

cat(
  "Texts:",
  nrow(subcorpus_meta),
  "\n"
)

cat(
  "Authors:",
  n_distinct(
    subcorpus_meta$author_username
  ),
  "\n"
)


# These were established in the previous workflow.

if (nrow(subcorpus_meta) != 5153L) {
  
  stop(
    paste(
      "Expected 5,153 subcorpus texts but found",
      nrow(subcorpus_meta)
    )
  )
}


if (
  n_distinct(
    subcorpus_meta$author_username
  ) != 2701L
) {
  
  stop(
    paste(
      "Expected 2,701 subcorpus authors but found",
      n_distinct(
        subcorpus_meta$author_username
      )
    )
  )
}


# ============================================================
# 5. LOAD EXISTING UDPipe WORD COUNTS
#
# These are used ONLY to determine:
#
#   - number of complete 1,000-word chunks per story
#   - author eligibility
#   - story-level train/test allocation
#
# Final chunk text is recreated below from the original files.
# ============================================================

cat("\nLoading UDPipe count files...\n")


count_files <- list.files(
  COUNTS_DIR,
  pattern = "^part_.*\\.rds$",
  full.names = TRUE
)


if (length(count_files) == 0L) {
  
  stop(
    "No UDPipe count files found."
  )
}


cat(
  "Count files:",
  length(count_files),
  "\n"
)


counts <- bind_rows(
  lapply(
    count_files,
    readRDS
  )
) %>%
  
  mutate(
    
    text_id = str_extract(
      filename,
      "^[^_\\s]+"
    ),
    
    text_id = as.character(
      text_id
    )
  )


required_count_columns <- c(
  "filename",
  "words",
  "text_id"
)


missing_count_columns <- setdiff(
  required_count_columns,
  names(counts)
)


if (length(missing_count_columns) > 0L) {
  
  stop(
    paste(
      "Count files are missing columns:",
      paste(
        missing_count_columns,
        collapse = ", "
      )
    )
  )
}


cat(
  "Count rows:",
  nrow(counts),
  "\n"
)


# ============================================================
# 6. JOIN COUNTS TO SUBCORPUS
# ============================================================

subcorpus <- subcorpus_meta %>%
  
  left_join(
    
    counts %>%
      select(
        text_id,
        filename,
        words
      ),
    
    by = "text_id"
  )


cat("\n========================================\n")
cat("COUNT JOIN\n")
cat("========================================\n")


cat(
  "Subcorpus texts:",
  nrow(subcorpus),
  "\n"
)


cat(
  "Texts with UDPipe counts:",
  sum(
    !is.na(
      subcorpus$words
    )
  ),
  "\n"
)


cat(
  "Texts without UDPipe counts:",
  sum(
    is.na(
      subcorpus$words
    )
  ),
  "\n"
)


# Expected from previous validated workflow.

if (
  sum(
    !is.na(
      subcorpus$words
    )
  ) != 5031L
) {
  
  stop(
    paste(
      "Expected 5,031 texts with UDPipe counts but found",
      sum(
        !is.na(
          subcorpus$words
        )
      )
    )
  )
}


# ============================================================
# 7. CALCULATE COMPLETE 1,000-WORD CHUNKS PER STORY
# ============================================================

story_chunks <- subcorpus %>%
  
  filter(
    !is.na(words),
    words > 0
  ) %>%
  
  mutate(
    
    available_chunks = floor(
      words /
        CHUNK_SIZE
    )
  )


cat("\n========================================\n")
cat("CHUNK AVAILABILITY\n")
cat("========================================\n")


cat(
  "Authors with usable counts:",
  n_distinct(
    story_chunks$author_username
  ),
  "\n"
)


cat(
  "Stories with >=1 complete chunk:",
  sum(
    story_chunks$available_chunks >= 1L
  ),
  "\n"
)


if (
  n_distinct(
    story_chunks$author_username
  ) != 2630L
) {
  
  stop(
    paste(
      "Expected 2,630 authors with usable counts but found",
      n_distinct(
        story_chunks$author_username
      )
    )
  )
}


if (
  sum(
    story_chunks$available_chunks >= 1L
  ) != 4364L
) {
  
  stop(
    paste(
      "Expected 4,364 stories with >=1 complete chunk but found",
      sum(
        story_chunks$available_chunks >= 1L
      )
    )
  )
}


# ============================================================
# 8. FIND STORY-LEVEL TRAIN/TEST SPLIT
#
# Requirement:
#
# TEST stories:
#   >= 5 complete chunks
#
# TRAIN stories:
#   >= 15 complete chunks
#
# A story may NEVER occur in both.
#
# Preference:
#
# 1. smallest possible number of test stories
# 2. among those, test capacity closest to exactly 5 chunks
#
# Deterministic.
# ============================================================

find_story_split <- function(
    text_ids,
    chunk_counts,
    n_train = 15L,
    n_test = 5L) {
  
  
  keep <- chunk_counts > 0L
  
  
  text_ids <- text_ids[
    keep
  ]
  
  
  chunk_counts <- chunk_counts[
    keep
  ]
  
  
  if (length(text_ids) < 2L) {
    return(NULL)
  }
  
  
  if (
    sum(chunk_counts) <
    n_train + n_test
  ) {
    return(NULL)
  }
  
  
  n_stories <- length(
    text_ids
  )
  
  
  for (
    k in seq_len(
      n_stories - 1L
    )
  ) {
    
    
    combinations <- combn(
      seq_len(
        n_stories
      ),
      k,
      simplify = FALSE
    )
    
    
    valid <- lapply(
      
      combinations,
      
      function(idx) {
        
        
        test_available <- sum(
          chunk_counts[
            idx
          ]
        )
        
        
        train_available <- sum(
          chunk_counts[
            -idx
          ]
        )
        
        
        if (
          test_available >= n_test &&
          train_available >= n_train
        ) {
          
          return(
            list(
              idx = idx,
              test_available = test_available,
              train_available = train_available
            )
          )
        }
        
        
        NULL
      }
    )
    
    
    valid <- valid[
      !vapply(
        valid,
        is.null,
        logical(1)
      )
    ]
    
    
    if (length(valid) > 0L) {
      
      
      test_sizes <- vapply(
        valid,
        function(x) {
          x$test_available
        },
        numeric(1)
      )
      
      
      best <- valid[[
        which.min(
          test_sizes
        )
      ]]
      
      
      test_idx <- best$idx
      
      
      return(
        tibble(
          
          text_id =
            text_ids,
          
          set =
            if_else(
              seq_along(
                text_ids
              ) %in% test_idx,
              "test",
              "train"
            ),
          
          available_chunks =
            chunk_counts
        )
      )
    }
  }
  
  
  NULL
}


# ============================================================
# 9. DETERMINE STORY ALLOCATION FOR EVERY AUTHOR
# ============================================================

cat("\n========================================\n")
cat("DETERMINING STORY ALLOCATION\n")
cat("========================================\n")


authors <- sort(
  unique(
    story_chunks$author_username
  )
)


allocation_list <- vector(
  "list",
  length(
    authors
  )
)


names(
  allocation_list
) <- authors


for (
  i in seq_along(
    authors
  )
) {
  
  
  author <- authors[
    i
  ]
  
  
  dat <- story_chunks %>%
    
    filter(
      author_username ==
        author
    ) %>%
    
    arrange(
      text_id
    )
  
  
  result <- find_story_split(
    
    text_ids =
      dat$text_id,
    
    chunk_counts =
      dat$available_chunks,
    
    n_train =
      N_TRAIN,
    
    n_test =
      N_TEST
  )
  
  
  # Preserve NULL entries.
  
  allocation_list[i] <- list(
    result
  )
  
  
  if (
    i %% 250L == 0L ||
    i == length(authors)
  ) {
    
    cat(
      "Processed",
      i,
      "of",
      length(authors),
      "authors\n"
    )
  }
}


# ============================================================
# 10. IDENTIFY ELIGIBLE AUTHORS
# ============================================================

is_eligible <- !vapply(
  allocation_list,
  is.null,
  logical(1)
)


eligible_names <- names(
  allocation_list
)[
  is_eligible
]


cat("\n========================================\n")
cat("ELIGIBILITY\n")
cat("========================================\n")


cat(
  "Eligible authors:",
  length(
    eligible_names
  ),
  "\n"
)


if (
  length(
    eligible_names
  ) != EXPECTED_AUTHORS
) {
  
  stop(
    paste(
      "Expected 409 eligible authors but found",
      length(
        eligible_names
      )
    )
  )
}


# ============================================================
# 11. BUILD STORY ALLOCATION MANIFEST
# ============================================================

allocation_manifest <- bind_rows(
  
  lapply(
    
    eligible_names,
    
    function(author) {
      
      
      allocation_list[[
        author
      ]] %>%
        
        mutate(
          author_username =
            author
        )
    }
  )
) %>%
  
  select(
    author_username,
    text_id,
    set,
    available_chunks
  ) %>%
  
  left_join(
    
    story_chunks %>%
      
      select(
        text_id,
        filename,
        words
      ),
    
    by = "text_id"
  ) %>%
  
  arrange(
    author_username,
    set,
    text_id
  )


# ============================================================
# 12. VALIDATE STORY ALLOCATION
# ============================================================

allocation_check <- allocation_manifest %>%
  
  group_by(
    author_username,
    set
  ) %>%
  
  summarise(
    
    available_chunks =
      sum(
        available_chunks
      ),
    
    .groups =
      "drop"
  ) %>%
  
  pivot_wider(
    names_from =
      set,
    values_from =
      available_chunks,
    values_fill =
      0
  )


cat("\n========================================\n")
cat("STORY ALLOCATION CHECK\n")
cat("========================================\n")


cat(
  "Minimum train chunks:",
  min(
    allocation_check$train
  ),
  "\n"
)


cat(
  "Minimum test chunks:",
  min(
    allocation_check$test
  ),
  "\n"
)


if (
  any(
    allocation_check$train <
    N_TRAIN
  )
) {
  
  stop(
    "At least one eligible author has fewer than 15 train chunks."
  )
}


if (
  any(
    allocation_check$test <
    N_TEST
  )
) {
  
  stop(
    "At least one eligible author has fewer than 5 test chunks."
  )
}


# ============================================================
# 13. VERIFY STORY-LEVEL SEPARATION
# ============================================================

story_overlap <- allocation_manifest %>%
  
  distinct(
    author_username,
    text_id,
    set
  ) %>%
  
  count(
    author_username,
    text_id,
    name = "n_sets"
  ) %>%
  
  filter(
    n_sets > 1L
  )


if (nrow(story_overlap) > 0L) {
  
  stop(
    "Story allocation contains train/test overlap."
  )
}


cat(
  "Train/test story overlap:",
  nrow(story_overlap),
  "\n"
)


# ============================================================
# 14. INDEX ORIGINAL TEXT FILES
# ============================================================

cat("\nIndexing original text files...\n")


original_files <- list.files(
  CORPUS_DIR,
  recursive = TRUE,
  full.names = TRUE
)


if (length(original_files) == 0L) {
  
  stop(
    "No original text files found."
  )
}


file_lookup <- tibble(
  
  original_filename =
    basename(
      original_files
    ),
  
  full_path =
    original_files
)


# Check filenames needed by this experiment.

needed_filenames <- unique(
  basename(
    allocation_manifest$filename
  )
)


missing_originals <- setdiff(
  needed_filenames,
  file_lookup$original_filename
)


if (length(missing_originals) > 0L) {
  
  stop(
    paste0(
      "Original text files missing:\n",
      paste(
        missing_originals,
        collapse = "\n"
      )
    )
  )
}


duplicate_originals <- file_lookup %>%
  
  filter(
    original_filename %in%
      needed_filenames
  ) %>%
  
  count(
    original_filename,
    name = "n"
  ) %>%
  
  filter(
    n > 1L
  )


if (nrow(duplicate_originals) > 0L) {
  
  stop(
    paste0(
      "Duplicate original filenames found:\n",
      paste(
        duplicate_originals$original_filename,
        collapse = "\n"
      )
    )
  )
}


# ============================================================
# 15. LOAD UDPipe MODEL
# ============================================================

cat("\nLoading UDPipe model...\n")


ud_model <- udpipe_load_model(
  MODEL_FILE
)


cat(
  "UDPipe model loaded.\n"
)


# ============================================================
# 16. TOKENISE STORY
#
# IMPORTANT:
#
# We KEEP the complete UDPipe token stream.
#
# is_word is TRUE when the token contains at least one
# alphanumeric character.
#
# This is the same word definition used for the existing
# counts.
#
# Punctuation remains in the annotation.
# ============================================================

tokenise_story <- function(
    file) {
  
  
  txt <- paste(
    readLines(
      file,
      warn = FALSE,
      encoding = "UTF-8"
    ),
    collapse = "\n"
  )
  
  
  ann <- udpipe_annotate(
    
    ud_model,
    
    x =
      txt,
    
    tagger =
      "none",
    
    parser =
      "none"
  )
  
  
  ann <- as.data.frame(
    ann,
    stringsAsFactors = FALSE
  )
  
  
  # Remove UD multiword-token range rows exactly as in the
  # previous counting workflow.
  
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
  
  
  ann$is_word <- grepl(
    "[[:alnum:]]",
    ann$token
  )
  
  
  ann
}


# ============================================================
# 17. CREATE COMPLETE 1,000-WORD CHUNKS WITH PUNCTUATION
#
# Example:
#
# word 1 -------------------------- word 1000
#   |                                  |
#   v                                  v
#
# Das war , ehrlich gesagt , ... Ende .
#
# All punctuation between the first and 1,000th word is kept.
#
# Each resulting chunk therefore has:
#
#   words = 1,000
#
# but:
#
#   total_tokens > 1,000
#
# whenever punctuation occurs.
# ============================================================

make_story_chunks <- function(
    ann,
    chunk_size = 1000L) {
  
  
  word_rows <- which(
    ann$is_word
  )
  
  
  n_words <- length(
    word_rows
  )
  
  
  n_complete <- floor(
    n_words /
      chunk_size
  )
  
  
  if (n_complete == 0L) {
    
    return(
      tibble(
        
        story_chunk =
          integer(),
        
        words =
          integer(),
        
        punctuation_tokens =
          integer(),
        
        total_tokens =
          integer(),
        
        text =
          character()
      )
    )
  }
  
  
  result <- vector(
    "list",
    n_complete
  )
  
  
  for (
    i in seq_len(
      n_complete
    )
  ) {
    
    
    first_word <-
      ((i - 1L) *
         chunk_size) + 1L
    
    
    last_word <-
      i *
      chunk_size
    
    
    first_row <- word_rows[
      first_word
    ]
    
    
    last_row <- word_rows[
      last_word
    ]
    
    
    # KEEP EVERYTHING between the two word boundaries.
    
    chunk_ann <- ann[
      first_row:last_row,
      ,
      drop = FALSE
    ]
    
    
    words_in_chunk <- sum(
      chunk_ann$is_word
    )
    
    
    punctuation_in_chunk <- sum(
      !chunk_ann$is_word
    )
    
    
    total_tokens_in_chunk <- nrow(
      chunk_ann
    )
    
    
    if (
      words_in_chunk !=
      chunk_size
    ) {
      
      stop(
        paste(
          "Internal chunking error:",
          words_in_chunk,
          "words instead of",
          chunk_size
        )
      )
    }
    
    
    if (
      total_tokens_in_chunk !=
      words_in_chunk +
      punctuation_in_chunk
    ) {
      
      stop(
        "Internal token accounting error."
      )
    }
    
    
    result[[i]] <- tibble(
      
      story_chunk =
        i,
      
      words =
        words_in_chunk,
      
      punctuation_tokens =
        punctuation_in_chunk,
      
      total_tokens =
        total_tokens_in_chunk,
      
      text =
        paste(
          chunk_ann$token,
          collapse = " "
        )
    )
  }
  
  
  bind_rows(
    result
  )
}


# ============================================================
# 18. TOKENISE ALL ALLOCATED STORIES AND CREATE CANDIDATES
# ============================================================

cat("\n========================================\n")
cat("TOKENISING AND CHUNKING ORIGINAL STORIES\n")
cat("========================================\n")


candidate_results <- vector(
  "list",
  nrow(
    allocation_manifest
  )
)


for (
  i in seq_len(
    nrow(
      allocation_manifest
    )
  )
) {
  
  
  row <- allocation_manifest[
    i,
  ]
  
  
  original_filename <- basename(
    row$filename
  )
  
  
  file_match <- file_lookup %>%
    
    filter(
      original_filename ==
        !!original_filename
    )
  
  
  if (nrow(file_match) != 1L) {
    
    stop(
      paste(
        "Expected exactly one original file for",
        original_filename,
        "but found",
        nrow(file_match)
      )
    )
  }
  
  
  file <- file_match$full_path[1]
  
  
  cat(
    "[",
    i,
    "/",
    nrow(allocation_manifest),
    "]",
    original_filename,
    "\n"
  )
  
  
  # ----------------------------------------------------------
  # Fresh tokenisation of original text
  # ----------------------------------------------------------
  
  ann <- tokenise_story(
    file
  )
  
  
  fresh_word_count <- sum(
    ann$is_word
  )
  
  
  # This should reproduce the existing UDPipe count.
  
  if (
    fresh_word_count !=
    as.integer(
      row$words
    )
  ) {
    
    stop(
      paste0(
        "\nWORD COUNT MISMATCH\n",
        "text_id: ",
        row$text_id,
        "\n",
        "file: ",
        original_filename,
        "\n",
        "existing count: ",
        row$words,
        "\n",
        "fresh count: ",
        fresh_word_count,
        "\n"
      )
    )
  }
  
  
  # ----------------------------------------------------------
  # Create ALL complete candidate chunks
  # ----------------------------------------------------------
  
  chunks <- make_story_chunks(
    
    ann =
      ann,
    
    chunk_size =
      CHUNK_SIZE
  )
  
  
  expected_chunks <- as.integer(
    row$available_chunks
  )
  
  
  actual_chunks <- nrow(
    chunks
  )
  
  
  if (
    actual_chunks !=
    expected_chunks
  ) {
    
    stop(
      paste0(
        "\nCHUNK COUNT MISMATCH\n",
        "text_id: ",
        row$text_id,
        "\n",
        "file: ",
        original_filename,
        "\n",
        "expected chunks: ",
        expected_chunks,
        "\n",
        "actual chunks: ",
        actual_chunks,
        "\n"
      )
    )
  }
  
  
  chunks <- chunks %>%
    
    mutate(
      
      author_username =
        row$author_username,
      
      text_id =
        as.character(
          row$text_id
        ),
      
      set =
        row$set,
      
      original_filename =
        original_filename
    ) %>%
    
    select(
      author_username,
      text_id,
      set,
      original_filename,
      story_chunk,
      words,
      punctuation_tokens,
      total_tokens,
      text
    )
  
  
  candidate_results[[i]] <- chunks
  
  
  # Free annotation before next story.
  
  rm(
    ann,
    chunks
  )
  
  
  if (
    i %% 50L == 0L ||
    i == nrow(allocation_manifest)
  ) {
    
    gc()
    
    cat(
      "Completed",
      i,
      "of",
      nrow(allocation_manifest),
      "stories\n"
    )
  }
}


# ============================================================
# 19. COMBINE CANDIDATE CHUNKS
# ============================================================

cat("\nCombining candidate chunks...\n")


candidate_chunks <- bind_rows(
  candidate_results
)


rm(
  candidate_results
)

gc()


cat("\n========================================\n")
cat("CANDIDATE CORPUS\n")
cat("========================================\n")


cat(
  "Candidate chunks:",
  nrow(candidate_chunks),
  "\n"
)


cat(
  "Authors:",
  n_distinct(
    candidate_chunks$author_username
  ),
  "\n"
)


cat(
  "Stories:",
  n_distinct(
    candidate_chunks$text_id
  ),
  "\n"
)


cat(
  "Punctuation tokens:",
  sum(
    candidate_chunks$punctuation_tokens
  ),
  "\n"
)


# ============================================================
# 20. VALIDATE CANDIDATE CORPUS
# ============================================================

if (
  n_distinct(
    candidate_chunks$author_username
  ) != EXPECTED_AUTHORS
) {
  
  stop(
    "Candidate corpus does not contain 409 authors."
  )
}


if (
  any(
    candidate_chunks$words !=
    CHUNK_SIZE
  )
) {
  
  stop(
    "At least one candidate chunk does not contain exactly 1,000 words."
  )
}


if (
  sum(
    candidate_chunks$punctuation_tokens
  ) <= 0L
) {
  
  stop(
    "No punctuation was retained. STOPPING."
  )
}


if (
  any(
    candidate_chunks$total_tokens !=
    candidate_chunks$words +
    candidate_chunks$punctuation_tokens
  )
) {
  
  stop(
    "Candidate token accounting failed."
  )
}


duplicate_candidates <- candidate_chunks %>%
  
  count(
    author_username,
    text_id,
    story_chunk,
    name = "n"
  ) %>%
  
  filter(
    n > 1L
  )


if (nrow(duplicate_candidates) > 0L) {
  
  stop(
    "Duplicate candidate chunks detected."
  )
}


candidate_story_overlap <- candidate_chunks %>%
  
  distinct(
    author_username,
    text_id,
    set
  ) %>%
  
  count(
    author_username,
    text_id,
    name = "n"
  ) %>%
  
  filter(
    n > 1L
  )


if (
  nrow(
    candidate_story_overlap
  ) > 0L
) {
  
  stop(
    "Candidate corpus violates story-level train/test separation."
  )
}


# ============================================================
# 21. CHECK AVAILABLE CHUNKS PER AUTHOR
# ============================================================

availability <- candidate_chunks %>%
  
  count(
    author_username,
    set,
    name = "available_chunks"
  ) %>%
  
  pivot_wider(
    names_from =
      set,
    values_from =
      available_chunks,
    values_fill =
      0
  )


cat(
  "Minimum available train chunks:",
  min(
    availability$train
  ),
  "\n"
)


cat(
  "Minimum available test chunks:",
  min(
    availability$test
  ),
  "\n"
)


if (
  any(
    availability$train <
    N_TRAIN
  )
) {
  
  stop(
    "At least one author has fewer than 15 candidate train chunks."
  )
}


if (
  any(
    availability$test <
    N_TEST
  )
) {
  
  stop(
    "At least one author has fewer than 5 candidate test chunks."
  )
}


# ============================================================
# 22. ASSIGN STABLE ANONYMISED AUTHOR IDs
# ============================================================

author_key <- tibble(
  
  author_username =
    sort(
      unique(
        candidate_chunks$author_username
      )
    )
  
) %>%
  
  mutate(
    
    author_id =
      sprintf(
        "A%03d",
        row_number()
      )
  ) %>%
  
  select(
    author_id,
    author_username
  )


if (
  nrow(author_key) !=
  EXPECTED_AUTHORS
) {
  
  stop(
    "Author ID creation failed."
  )
}


# ============================================================
# 23. REPRODUCIBLY SELECT 15 TRAIN CHUNKS
#
# SAME SEED AS ORIGINAL WORKFLOW.
# ============================================================

cat("\nSelecting 15 training chunks per author...\n")


set.seed(
  TRAIN_SEED
)


selected_train <- candidate_chunks %>%
  
  filter(
    set == "train"
  ) %>%
  
  group_by(
    author_username
  ) %>%
  
  slice_sample(
    n = N_TRAIN,
    replace = FALSE
  ) %>%
  
  ungroup()


# ============================================================
# 24. REPRODUCIBLY SELECT 5 TEST CHUNKS
#
# SAME SEED AS ORIGINAL WORKFLOW.
# ============================================================

cat(
  "Selecting 5 test chunks per author...\n"
)


set.seed(
  TEST_SEED
)


selected_test <- candidate_chunks %>%
  
  filter(
    set == "test"
  ) %>%
  
  group_by(
    author_username
  ) %>%
  
  slice_sample(
    n = N_TEST,
    replace = FALSE
  ) %>%
  
  ungroup()


# ============================================================
# 25. CREATE FINAL SELECTION
# ============================================================

final_chunks <- bind_rows(
  selected_train,
  selected_test
) %>%
  
  left_join(
    author_key,
    by = "author_username"
  ) %>%
  
  arrange(
    author_id,
    set,
    text_id,
    story_chunk
  ) %>%
  
  group_by(
    author_id,
    set
  ) %>%
  
  mutate(
    split_chunk =
      row_number()
  ) %>%
  
  ungroup() %>%
  
  mutate(
    
    final_filename =
      case_when(
        
        set == "train" ~
          sprintf(
            "%s_train_%02d.txt",
            author_id,
            split_chunk
          ),
        
        set == "test" ~
          sprintf(
            "%s_test_%02d.txt",
            author_id,
            split_chunk
          )
      )
  )


# ============================================================
# 26. FINAL VALIDATION
# ============================================================

cat("\n========================================\n")
cat("FINAL VALIDATION\n")
cat("========================================\n")


per_author <- final_chunks %>%
  
  count(
    author_id,
    set,
    name = "chunks"
  ) %>%
  
  pivot_wider(
    names_from =
      set,
    values_from =
      chunks,
    values_fill =
      0
  ) %>%
  
  mutate(
    total =
      train + test
  )


final_overlap <- final_chunks %>%
  
  distinct(
    author_id,
    text_id,
    set
  ) %>%
  
  count(
    author_id,
    text_id,
    name = "n"
  ) %>%
  
  filter(
    n > 1L
  )


duplicate_final <- final_chunks %>%
  
  count(
    author_id,
    text_id,
    story_chunk,
    name = "n"
  ) %>%
  
  filter(
    n > 1L
  )


validation <- tibble(
  
  check = c(
    "authors",
    "train_chunks",
    "test_chunks",
    "total_chunks",
    "words_per_chunk",
    "total_words",
    "chunks_per_author",
    "train_chunks_per_author",
    "test_chunks_per_author",
    "train_test_story_overlap",
    "duplicate_chunks",
    "punctuation_retained",
    "token_accounting"
  ),
  
  expected = c(
    "409",
    "6135",
    "2045",
    "8180",
    "1000",
    "8180000",
    "20",
    "15",
    "5",
    "0",
    "0",
    ">0",
    "TRUE"
  ),
  
  observed = c(
    
    as.character(
      n_distinct(
        final_chunks$author_id
      )
    ),
    
    as.character(
      sum(
        final_chunks$set ==
          "train"
      )
    ),
    
    as.character(
      sum(
        final_chunks$set ==
          "test"
      )
    ),
    
    as.character(
      nrow(
        final_chunks
      )
    ),
    
    ifelse(
      all(
        final_chunks$words ==
          CHUNK_SIZE
      ),
      "1000",
      "FAILED"
    ),
    
    as.character(
      sum(
        final_chunks$words
      )
    ),
    
    ifelse(
      all(
        per_author$total ==
          N_TOTAL
      ),
      "20",
      "FAILED"
    ),
    
    ifelse(
      all(
        per_author$train ==
          N_TRAIN
      ),
      "15",
      "FAILED"
    ),
    
    ifelse(
      all(
        per_author$test ==
          N_TEST
      ),
      "5",
      "FAILED"
    ),
    
    as.character(
      nrow(
        final_overlap
      )
    ),
    
    as.character(
      nrow(
        duplicate_final
      )
    ),
    
    as.character(
      sum(
        final_chunks$punctuation_tokens
      )
    ),
    
    as.character(
      all(
        final_chunks$total_tokens ==
          final_chunks$words +
          final_chunks$punctuation_tokens
      )
    )
  ),
  
  passed = c(
    
    n_distinct(
      final_chunks$author_id
    ) == EXPECTED_AUTHORS,
    
    sum(
      final_chunks$set ==
        "train"
    ) == EXPECTED_TRAIN_CHUNKS,
    
    sum(
      final_chunks$set ==
        "test"
    ) == EXPECTED_TEST_CHUNKS,
    
    nrow(
      final_chunks
    ) == EXPECTED_TOTAL_CHUNKS,
    
    all(
      final_chunks$words ==
        CHUNK_SIZE
    ),
    
    sum(
      final_chunks$words
    ) == EXPECTED_WORDS,
    
    all(
      per_author$total ==
        N_TOTAL
    ),
    
    all(
      per_author$train ==
        N_TRAIN
    ),
    
    all(
      per_author$test ==
        N_TEST
    ),
    
    nrow(
      final_overlap
    ) == 0L,
    
    nrow(
      duplicate_final
    ) == 0L,
    
    sum(
      final_chunks$punctuation_tokens
    ) > 0L,
    
    all(
      final_chunks$total_tokens ==
        final_chunks$words +
        final_chunks$punctuation_tokens
    )
  )
)


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
    "FINAL VALIDATION FAILED. Nothing has been written."
  )
}


# ============================================================
# 27. PREPARE CLEAN FINAL OUTPUT DIRECTORY
#
# We only touch FINAL_DIR now that the complete in-memory
# corpus has passed validation.
# ============================================================

cat("\nValidation passed. Creating final output...\n")


if (
  dir.exists(
    FINAL_DIR
  )
) {
  
  unlink(
    FINAL_DIR,
    recursive = TRUE,
    force = TRUE
  )
}


dir.create(
  TRAIN_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)


dir.create(
  TEST_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)


# ============================================================
# 28. WRITE FINAL TEXT FILES
# ============================================================

cat("\nWriting final corpus files...\n")


for (
  i in seq_len(
    nrow(
      final_chunks
    )
  )
) {
  
  
  destination <- if (
    final_chunks$set[i] ==
    "train"
  ) {
    
    TRAIN_DIR
    
  } else {
    
    TEST_DIR
  }
  
  
  writeLines(
    
    final_chunks$text[i],
    
    file.path(
      destination,
      final_chunks$final_filename[i]
    ),
    
    useBytes = TRUE
  )
  
  
  if (
    i %% 500L == 0L ||
    i == nrow(final_chunks)
  ) {
    
    cat(
      "Written",
      i,
      "of",
      nrow(final_chunks),
      "files\n"
    )
  }
}


# ============================================================
# 29. CHECK FILE COUNTS ON DISK
# ============================================================

train_files <- list.files(
  TRAIN_DIR,
  pattern = "\\.txt$",
  full.names = TRUE
)


test_files <- list.files(
  TEST_DIR,
  pattern = "\\.txt$",
  full.names = TRUE
)


if (
  length(
    train_files
  ) != EXPECTED_TRAIN_CHUNKS
) {
  
  stop(
    paste(
      "Expected 6,135 train files but found",
      length(
        train_files
      )
    )
  )
}


if (
  length(
    test_files
  ) != EXPECTED_TEST_CHUNKS
) {
  
  stop(
    paste(
      "Expected 2,045 test files but found",
      length(
        test_files
      )
    )
  )
}


# ============================================================
# 30. SAVE FINAL DATA OBJECTS
# ============================================================

saveRDS(
  
  final_chunks,
  
  file.path(
    FINAL_DIR,
    "final_chunks.rds"
  )
)


write.csv(
  
  final_chunks %>%
    
    select(
      author_id,
      author_username,
      set,
      final_filename,
      text_id,
      original_filename,
      story_chunk,
      split_chunk,
      words,
      punctuation_tokens,
      total_tokens
    ),
  
  file.path(
    FINAL_DIR,
    "chunk_manifest.csv"
  ),
  
  row.names = FALSE,
  
  fileEncoding = "UTF-8"
)


write.csv(
  
  author_key,
  
  file.path(
    FINAL_DIR,
    "author_key.csv"
  ),
  
  row.names = FALSE,
  
  fileEncoding = "UTF-8"
)


write.csv(
  
  allocation_manifest,
  
  file.path(
    FINAL_DIR,
    "story_allocation_manifest.csv"
  ),
  
  row.names = FALSE,
  
  fileEncoding = "UTF-8"
)


write.csv(
  
  validation,
  
  file.path(
    FINAL_DIR,
    "validation.csv"
  ),
  
  row.names = FALSE
)


experiment_summary <- tibble(
  
  authors =
    EXPECTED_AUTHORS,
  
  chunks_per_author =
    N_TOTAL,
  
  train_chunks_per_author =
    N_TRAIN,
  
  test_chunks_per_author =
    N_TEST,
  
  train_chunks =
    EXPECTED_TRAIN_CHUNKS,
  
  test_chunks =
    EXPECTED_TEST_CHUNKS,
  
  total_chunks =
    EXPECTED_TOTAL_CHUNKS,
  
  words_per_chunk =
    CHUNK_SIZE,
  
  total_words =
    sum(
      final_chunks$words
    ),
  
  punctuation_tokens =
    sum(
      final_chunks$punctuation_tokens
    ),
  
  total_tokens_including_punctuation =
    sum(
      final_chunks$total_tokens
    ),
  
  train_test_story_overlap =
    nrow(
      final_overlap
    )
)


write.csv(
  
  experiment_summary,
  
  file.path(
    FINAL_DIR,
    "experiment_summary.csv"
  ),
  
  row.names = FALSE
)


# ============================================================
# 31. READ FINAL RDS BACK AND VALIDATE AGAIN
# ============================================================

cat("\nReading final corpus back from disk...\n")


check_final <- readRDS(
  
  file.path(
    FINAL_DIR,
    "final_chunks.rds"
  )
)


if (
  nrow(
    check_final
  ) != EXPECTED_TOTAL_CHUNKS
) {
  
  stop(
    "Final RDS read-back failed: wrong number of chunks."
  )
}


if (
  n_distinct(
    check_final$author_id
  ) != EXPECTED_AUTHORS
) {
  
  stop(
    "Final RDS read-back failed: wrong number of authors."
  )
}


if (
  !all(
    check_final$words ==
    CHUNK_SIZE
  )
) {
  
  stop(
    "Final RDS read-back failed: chunk word count."
  )
}


if (
  sum(
    check_final$words
  ) != EXPECTED_WORDS
) {
  
  stop(
    "Final RDS read-back failed: total word count."
  )
}


if (
  sum(
    check_final$punctuation_tokens
  ) <= 0L
) {
  
  stop(
    "Final RDS read-back failed: punctuation missing."
  )
}


if (
  !all(
    check_final$total_tokens ==
    check_final$words +
    check_final$punctuation_tokens
  )
) {
  
  stop(
    "Final RDS read-back failed: token accounting."
  )
}


# ============================================================
# 32. FINAL REPORT
# ============================================================

cat("\n========================================\n")
cat("FINAL CORPUS SUCCESSFULLY CREATED\n")
cat("========================================\n")


cat(
  "Authors:",
  n_distinct(
    check_final$author_id
  ),
  "\n"
)


cat(
  "Training chunks:",
  sum(
    check_final$set ==
      "train"
  ),
  "\n"
)


cat(
  "Test chunks:",
  sum(
    check_final$set ==
      "test"
  ),
  "\n"
)


cat(
  "Total chunks:",
  nrow(
    check_final
  ),
  "\n"
)


cat(
  "Words per chunk:",
  CHUNK_SIZE,
  "\n"
)


cat(
  "Total words:",
  sum(
    check_final$words
  ),
  "\n"
)


cat(
  "Punctuation tokens retained:",
  sum(
    check_final$punctuation_tokens
  ),
  "\n"
)


cat(
  "Total tokens including punctuation:",
  sum(
    check_final$total_tokens
  ),
  "\n"
)


cat(
  "Train/test story overlap:",
  nrow(
    final_overlap
  ),
  "\n"
)


cat(
  "\nFinal corpus:\n",
  FINAL_DIR,
  "\n"
)


cat("\n========================================\n")
cat("DONE\n")
cat("========================================\n")
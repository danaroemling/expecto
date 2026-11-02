# ============================================================
# Darmstadt Fanfiction Corpus
# Harry Potter × Liebesgeschichte:
# subcorpus diagnostics for authorship-attribution sampling
#
# Original subcorpus definition:
#   - fandom label contains "Harry Potter"
#   - genre includes Liebesgeschichte, Romanze or Romance
#
# Proposed experimental design:
#   - >= 20,000 usable words per author
#   - 20 x 1,000-word chunks
#   - 15 training chunks (15,000 words)
#   - 5 test chunks (5,000 words)
#   - no story may occur in both training and test
# ============================================================


# ------------------------------------------------------------
# 0. SETUP
# ------------------------------------------------------------

library(tidyverse)

options(scipen = 999)

BASE <- "PATH/Misc/FF_Corpus"

PARTS_DIR <- file.path(BASE, "counts_parts")
META_FILE <- file.path(BASE, "metadata_merged.csv")
OUT_DIR   <- file.path(BASE, "subcorpus_diagnostics")

dir.create(
  OUT_DIR,
  showWarnings = FALSE,
  recursive = TRUE
)

stopifnot(
  dir.exists(PARTS_DIR),
  file.exists(META_FILE)
)


# ------------------------------------------------------------
# 1. LOAD UDPipe WORD COUNTS
# ------------------------------------------------------------

parts <- list.files(
  PARTS_DIR,
  pattern = "^part_.*\\.rds$",
  full.names = TRUE
)

cat(length(parts), "part files found\n")

counts <- bind_rows(
  lapply(parts, readRDS)
) %>%
  mutate(
    text_id = str_extract(
      filename,
      "^[^_\\s]+"
    )
  )

cat(
  nrow(counts),
  "texts counted;",
  n_distinct(counts$filename),
  "distinct filenames\n"
)


# ------------------------------------------------------------
# 2. LOAD METADATA
# ------------------------------------------------------------

metadata <- read.csv(
  META_FILE,
  header = TRUE,
  fileEncoding = "latin1"
) %>%
  mutate(
    text_id = as.character(text_id)
  )

cat(
  nrow(metadata),
  "metadata rows;",
  n_distinct(metadata$text_id),
  "distinct text IDs\n"
)


# ------------------------------------------------------------
# 3. RECONSTRUCT ORIGINAL SUBCORPUS DEFINITION
# ------------------------------------------------------------

# This follows the original subcorpus_creation.R:
#
# - split multi-value genre field
# - harmonise Romanze and Romance to Liebesgeschichte
# - fandom contains "Harry Potter"
# - genre == Liebesgeschichte
#
# distinct(text_id) prevents duplication where necessary.

subcorpus_meta <- metadata %>%
  separate_rows(
    genre,
    sep = ",\\s*"
  ) %>%
  mutate(
    genre = trimws(genre),
    
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
      fixed("Harry Potter")
    ),
    genre == "Liebesgeschichte"
  ) %>%
  distinct(
    text_id,
    .keep_all = TRUE
  )


cat("\n========================================\n")
cat("ORIGINAL SUBCORPUS DEFINITION\n")
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


# ------------------------------------------------------------
# 4. ATTACH UDPipe WORD COUNTS
# ------------------------------------------------------------

subcorpus <- subcorpus_meta %>%
  left_join(
    counts %>%
      select(
        text_id,
        filename,
        tokens,
        words
      ),
    by = "text_id"
  )


cat(
  "Texts without UDPipe count:",
  sum(is.na(subcorpus$words)),
  "\n"
)


# ------------------------------------------------------------
# 5. COMPARE METADATA AND UDPipe WORD COUNTS
# ------------------------------------------------------------

# Important diagnostic:
# The old subcorpus script used metadata wordcount.
# The newer corpus-counting workflow gives us a consistent
# UDPipe-derived word count.
#
# We compare both before choosing the final eligibility measure.

author_comparison <- subcorpus %>%
  group_by(author_username) %>%
  summarise(
    metadata_words =
      sum(wordcount, na.rm = TRUE),
    
    udpipe_words =
      sum(words, na.rm = TRUE),
    
    n_texts = n(),
    
    .groups = "drop"
  )


summary_comparison <- author_comparison %>%
  summarise(
    authors = n(),
    
    authors_20k_metadata =
      sum(metadata_words >= 20000),
    
    authors_20k_udpipe =
      sum(udpipe_words >= 20000),
    
    authors_20k_metadata_2plus =
      sum(
        metadata_words >= 20000 &
          n_texts >= 2
      ),
    
    authors_20k_udpipe_2plus =
      sum(
        udpipe_words >= 20000 &
          n_texts >= 2
      )
  )


cat("\n========================================\n")
cat("20K THRESHOLD COMPARISON\n")
cat("========================================\n")

print(summary_comparison)


# ------------------------------------------------------------
# 6. AUTHOR-LEVEL DIAGNOSTICS
# ------------------------------------------------------------

# For the experimental sampling we use UDPipe-derived words,
# because these correspond to the tokenisation that will also
# be used to construct the 1,000-word chunks.

author_diagnostics <- subcorpus %>%
  filter(
    !is.na(words),
    words > 0
  ) %>%
  group_by(author_username) %>%
  summarise(
    total_words = sum(words),
    
    n_texts = n(),
    
    largest_text_words =
      max(words),
    
    smallest_text_words =
      min(words),
    
    words_outside_largest =
      total_words - largest_text_words,
    
    .groups = "drop"
  ) %>%
  mutate(
    eligible_20k =
      total_words >= 20000,
    
    eligible_20k_2texts =
      total_words >= 20000 &
      n_texts >= 2
  ) %>%
  arrange(
    desc(total_words)
  )


# ------------------------------------------------------------
# 7. FUNCTION: CAN AUTHOR SUPPORT CLEAN 15K / 5K SPLIT?
# ------------------------------------------------------------

# Requirement:
#
# Training:
#   >= 15,000 words
#
# Test:
#   >= 5,000 words
#
# Critically:
#   training and test must consist of different stories.
#
# We therefore ask whether some subset of an author's stories
# can provide >= 5,000 test words while leaving >= 15,000
# words in the remaining stories.


can_split_15k_5k <- function(
    word_counts,
    train_min = 15000,
    test_min = 5000) {
  
  word_counts <- as.integer(
    word_counts
  )
  
  total <- sum(word_counts)
  
  # Need at least two separate stories
  if (length(word_counts) < 2) {
    return(FALSE)
  }
  
  # Need enough total material
  if (
    total <
    train_min + test_min
  ) {
    return(FALSE)
  }
  
  # Largest permissible amount allocated
  # to the test side while retaining
  # 15k for training
  max_test <-
    total - train_min
  
  if (max_test < test_min) {
    return(FALSE)
  }
  
  
  # Start with empty subset
  possible <- 0L
  
  
  # Generate possible subset sums
  for (w in word_counts) {
    
    new_sums <-
      possible + w
    
    # We don't care about allocations
    # too large to leave 15k for training
    new_sums <-
      new_sums[
        new_sums <= max_test
      ]
    
    possible <-
      unique(
        c(
          possible,
          new_sums
        )
      )
    
    
    # Stop immediately once a valid
    # test allocation exists
    if (
      any(
        possible >= test_min &
        possible <= max_test
      )
    ) {
      return(TRUE)
    }
  }
  
  
  return(FALSE)
}


# ------------------------------------------------------------
# 8. TEST SPLIT FEASIBILITY FOR EVERY AUTHOR
# ------------------------------------------------------------

split_feasibility <- subcorpus %>%
  filter(
    !is.na(words),
    words > 0
  ) %>%
  group_by(author_username) %>%
  summarise(
    split_15k_5k_possible =
      can_split_15k_5k(words),
    
    .groups = "drop"
  )


author_diagnostics <-
  author_diagnostics %>%
  left_join(
    split_feasibility,
    by = "author_username"
  )


# ------------------------------------------------------------
# 9. ELIGIBILITY SUMMARY
# ------------------------------------------------------------

eligibility_summary <-
  author_diagnostics %>%
  summarise(
    all_authors = n(),
    
    authors_20k =
      sum(eligible_20k),
    
    authors_20k_2plus_texts =
      sum(eligible_20k_2texts),
    
    authors_clean_15k_5k_split =
      sum(split_15k_5k_possible)
  )


cat("\n========================================\n")
cat("ELIGIBILITY FOR PROPOSED DESIGN\n")
cat("========================================\n")

print(eligibility_summary)


# ------------------------------------------------------------
# 10. NUMBER OF STORIES AMONG >=20K AUTHORS
# ------------------------------------------------------------

text_count_distribution <-
  author_diagnostics %>%
  filter(
    eligible_20k
  ) %>%
  count(
    n_texts,
    name = "authors"
  ) %>%
  arrange(
    n_texts
  )


cat("\n========================================\n")
cat("STORIES PER AUTHOR (>=20K)\n")
cat("========================================\n")

print(
  as.data.frame(
    text_count_distribution
  ),
  row.names = FALSE
)


# ------------------------------------------------------------
# 11. AUTHORS FAILING CROSS-STORY REQUIREMENT
# ------------------------------------------------------------

failed_split <-
  author_diagnostics %>%
  filter(
    eligible_20k,
    !split_15k_5k_possible
  ) %>%
  arrange(
    total_words
  )


cat("\n========================================\n")
cat("FAILED CROSS-STORY SPLIT\n")
cat("========================================\n")

cat(
  "Authors with >=20k words but no clean 15k/5k split:",
  nrow(failed_split),
  "\n"
)


# ------------------------------------------------------------
# 12. FINAL ELIGIBLE AUTHORS
# ------------------------------------------------------------

eligible_authors <-
  author_diagnostics %>%
  filter(
    split_15k_5k_possible
  ) %>%
  arrange(
    desc(total_words)
  )


cat("\n========================================\n")
cat("FINAL CANDIDATE SET\n")
cat("========================================\n")

cat(
  "Authors:",
  nrow(eligible_authors),
  "\n"
)


# ------------------------------------------------------------
# 13. TEXT-LEVEL DATA FOR ELIGIBLE AUTHORS
# ------------------------------------------------------------

eligible_texts <-
  subcorpus %>%
  semi_join(
    eligible_authors,
    by = "author_username"
  ) %>%
  arrange(
    author_username,
    desc(words)
  )


# ------------------------------------------------------------
# 14. SAVE OUTPUTS
# ------------------------------------------------------------

write.csv(
  subcorpus,
  file.path(
    OUT_DIR,
    "HP_Liebesgeschichte_all_texts.csv"
  ),
  row.names = FALSE
)


write.csv(
  author_comparison,
  file.path(
    OUT_DIR,
    "HP_Liebesgeschichte_wordcount_comparison.csv"
  ),
  row.names = FALSE
)


write.csv(
  author_diagnostics,
  file.path(
    OUT_DIR,
    "HP_Liebesgeschichte_author_diagnostics.csv"
  ),
  row.names = FALSE
)


write.csv(
  eligibility_summary,
  file.path(
    OUT_DIR,
    "HP_Liebesgeschichte_eligibility_summary.csv"
  ),
  row.names = FALSE
)


write.csv(
  failed_split,
  file.path(
    OUT_DIR,
    "HP_Liebesgeschichte_failed_15k_5k_split.csv"
  ),
  row.names = FALSE
)


write.csv(
  eligible_authors,
  file.path(
    OUT_DIR,
    "HP_Liebesgeschichte_eligible_authors.csv"
  ),
  row.names = FALSE
)


write.csv(
  eligible_texts,
  file.path(
    OUT_DIR,
    "HP_Liebesgeschichte_eligible_texts.csv"
  ),
  row.names = FALSE
)


cat("\n========================================\n")
cat("DONE\n")
cat("========================================\n")

cat(
  "Outputs saved to:\n",
  OUT_DIR,
  "\n"
)
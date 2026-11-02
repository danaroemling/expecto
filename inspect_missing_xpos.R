library(dplyr)

BASE <- "PATH/Misc/FF_Corpus"

FILE <- file.path(
  BASE,
  "HP_Liebesgeschichte_1000word_chunks_FINAL",
  "POS_tagged",
  "tagged_tokens.rds"
)

tagged <- readRDS(FILE)

missing <- tagged %>%
  filter(is.na(xpos) | xpos == "")

cat("\n=== MISSING XPOS TAGS ===\n")
cat("Total:", nrow(missing), "\n")
cat("Affected chunks:", n_distinct(missing$final_filename), "\n")
cat("Affected authors:", n_distinct(missing$author_id), "\n")

cat("\n=== MOST COMMON UNTAGGED TOKENS ===\n")

missing %>%
  count(token, sort = TRUE) %>%
  head(30) %>%
  print(n = 30)

cat("\n=== UPOS TAGS OF UNTAGGED TOKENS ===\n")

missing %>%
  count(upos, sort = TRUE) %>%
  print(n = Inf)

cat("\n=== EXAMPLES WITH CONTEXT ===\n")

examples <- missing %>%
  select(
    author_id,
    final_filename,
    token_position,
    token,
    lemma,
    upos,
    xpos,
    feats
  ) %>%
  head(30)

print(examples, n = 30, width = Inf)
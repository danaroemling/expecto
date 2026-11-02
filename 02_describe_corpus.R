# ============================================================
# Darmstadt Fanfiction Corpus: descriptive statistics (full corpus)
# Counts come from the raw .txt files only (UDPipe tokenisation, no tagging).
# Metadata is used only to attach author and fandom to each text.
# ============================================================

library(tidyverse)
library(fs)
options(scipen = 999)

# ---- 0. CONFIG ----
PARTS_DIR  <- "PATH/Misc/FF_Corpus/counts_parts"   # the folder downloaded from the HPC; adjust
META_FILE  <- "PATH/Misc/FF_Corpus/metadata_merged.csv"
OUT_DIR    <- "PATH/Misc/FF_Corpus/corpus_stats"
FONT       <- "Optima"
dir.create(OUT_DIR, showWarnings = FALSE)
purple <- "#6A3D9A"; sage <- "#8FAE8B"
stopifnot(dir.exists(PARTS_DIR), file.exists(META_FILE))

# ---- 1. LOAD COUNTS ----
parts <- list.files(PARTS_DIR, pattern = "^part_.*\\.rds$", full.names = TRUE)
cat(length(parts), "part files found\n")
counts <- bind_rows(lapply(parts, readRDS)) %>%
  mutate(text_id = str_extract(filename, "^[^_\\s]+"))
cat(nrow(counts), "texts counted;", n_distinct(counts$filename), "distinct file names\n")

# ---- 2. ATTACH AUTHOR AND FANDOM FROM THE METADATA ----
df <- read.csv(META_FILE, header = TRUE, fileEncoding = "latin1") %>%
  mutate(
    text_id = as.character(text_id),
    fandom = case_when(
      fandom_categories == "Haikyuu!! / FF" ~ "Haikyuu!!",
      fandom_categories == "My Hero Academia / FF" ~ "My Hero Academia",
      fandom_categories == "J.R.R. Tolkien / Mittelerde / Der Hobbit" ~ "J.R.R. Tolkien / Mittelerde / Der Herr der Ringe",
      TRUE ~ fandom_categories
    )
  ) %>%
  select(text_id, author_username, fandom) %>%
  left_join(counts %>% select(text_id, tokens, words), by = "text_id")

cat(nrow(df), "metadata rows;", n_distinct(df$text_id), "distinct text_ids\n")
cat(sum(is.na(df$tokens)), "metadata rows without a count (should be near 0)\n")
cat(sum(!counts$text_id %in% df$text_id), "counted files without a metadata row (should be near 0)\n")

# ---- 3. CORPUS-LEVEL STATISTICS ----
overall <- df %>%
  filter(!is.na(tokens)) %>%
  summarise(
    texts                 = n(),
    authors               = n_distinct(author_username),
    fandoms               = n_distinct(fandom[!is.na(fandom) & fandom != ""]),
    tokens_total          = sum(tokens),
    words_total           = sum(words),
    words_per_text_mean   = round(mean(words), 1),
    words_per_text_median = median(words),
    words_per_text_min    = min(words),
    words_per_text_max    = max(words)
  )
print(overall)
write.csv(overall, file.path(OUT_DIR, "overall.csv"), row.names = FALSE)

# ---- 4. PER-FANDOM STATISTICS ----
per_fandom <- df %>%
  filter(!is.na(tokens), !is.na(fandom), fandom != "") %>%
  group_by(fandom) %>%
  summarise(
    texts                 = n(),
    authors               = n_distinct(author_username),
    tokens                = sum(tokens),
    words                 = sum(words),
    words_per_text_mean   = round(mean(words), 1),
    words_per_text_median = median(words),
    .groups = "drop"
  ) %>%
  arrange(desc(words)) %>%
  mutate(pct_of_words = round(100 * words / sum(words), 2))
print(per_fandom, n = 30)
write.csv(per_fandom, file.path(OUT_DIR, "per_fandom.csv"), row.names = FALSE)

# ---- 5. PER-AUTHOR STATISTICS ----
per_author <- df %>%
  filter(!is.na(tokens)) %>%
  group_by(author_username) %>%
  summarise(
    texts   = n(),
    tokens  = sum(tokens),
    words   = sum(words),
    fandoms = n_distinct(fandom[!is.na(fandom) & fandom != ""]),
    .groups = "drop"
  )

author_summary <- per_author %>%
  summarise(
    texts_per_author_mean       = round(mean(texts), 2),
    texts_per_author_median     = median(texts),
    texts_per_author_max        = max(texts),
    authors_with_2plus_texts    = sum(texts >= 2),
    authors_with_5plus_texts    = sum(texts >= 5),
    authors_with_10plus_texts   = sum(texts >= 10),
    words_per_author_median     = median(words),
    authors_with_10k_plus_words = sum(words >= 10000),
    authors_in_multiple_fandoms = sum(fandoms > 1),
    pct_words_from_top_10pct_authors = round(100 * sum(head(sort(words, decreasing = TRUE), ceiling(0.1 * n()))) / sum(words), 1)
  )
print(author_summary)
write.csv(per_author, file.path(OUT_DIR, "per_author.csv"), row.names = FALSE)
write.csv(author_summary, file.path(OUT_DIR, "author_summary.csv"), row.names = FALSE)

# ---- 6. LENGTH BANDS (in words) ----
bands <- df %>%
  filter(!is.na(words)) %>%
  mutate(length_band = cut(words, c(-Inf, 500, 1000, 2500, 5000, 10000, Inf),
                           labels = c("<=500", "501-1,000", "1,001-2,500", "2,501-5,000", "5,001-10,000", ">10,000"))) %>%
  group_by(length_band) %>%
  summarise(texts = n(), words = sum(words), .groups = "drop")
print(bands)
write.csv(bands, file.path(OUT_DIR, "length_bands.csv"), row.names = FALSE)

# ---- 7. PLOTS ----
th <- theme_minimal(base_family = FONT, base_size = 12) +
  theme(panel.grid.minor = element_blank())

p1 <- per_fandom %>%
  slice_max(words, n = 15) %>%
  ggplot(aes(x = reorder(fandom, words), y = words)) +
  geom_col(fill = "#7671a8") + coord_flip() +
  scale_y_continuous(
    labels = scales::label_number(big.mark = "."),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(x = NULL, y = "Worte", title = "Worte pro Fandom (Top 15)") + th +
  theme(plot.margin = margin(t = 5, r = 25, b = 5, l = 5, unit = "pt"))
ggsave(file.path(OUT_DIR, "words_per_fandom.png"), p1, width = 10, height = 6, dpi = 300, bg = "white")

p2 <- df %>%
  filter(!is.na(words), words > 0) %>%
  ggplot(aes(x = words)) +
  geom_histogram(bins = 50, fill = purple, colour = "white") +
  scale_x_log10(labels = scales::comma) +
  labs(x = "Words per text (log scale)", y = "Texts", title = "Text length distribution") + th
ggsave(file.path(OUT_DIR, "text_length.png"), p2, width = 8, height = 5, dpi = 300, bg = "white")

p3 <- ggplot(per_author, aes(x = texts)) +
  geom_histogram(bins = 40, fill = sage, colour = "white") +
  scale_x_log10() +
  labs(x = "Texts per author (log scale)", y = "Authors", title = "Texts per author") + th
ggsave(file.path(OUT_DIR, "texts_per_author.png"), p3, width = 8, height = 5, dpi = 300, bg = "white")



p2_alt <- df %>%
  filter(!is.na(words), words > 0, words <= 30000) %>% # Drop extreme outliers for visualization
  ggplot(aes(x = words)) +
  geom_histogram(bins = 50, fill = "#7671a8", colour = "white") +
  scale_y_continuous(labels = scales::label_number(big.mark = "."))  +
  labs(x = "Textlänge", y = "Texte", 
       title = "Textlängenverteilung bis 30.000 Worte") + th

ggsave(file.path(OUT_DIR, "text_length_trimmed_3.png"), p2_alt, width = 8, height = 5, dpi = 300, bg = "white")


top_fandoms_list <- df %>%
  filter(!is.na(fandom), fandom != "") %>%
  group_by(fandom) %>%
  summarise(total_words = sum(words, na.rm = TRUE)) %>%
  slice_max(total_words, n = 10) %>%
  pull(fandom)

p2_box <- df %>%
  filter(fandom %in% top_fandoms_list, words > 0, words <= 20000) %>%
  ggplot(aes(x = reorder(fandom, words, FUN = median), y = words)) +
  geom_boxplot(fill = "#7671a8", alpha = 0.8, outlier.shape = NA) +
  coord_flip() +
  scale_y_continuous(labels = scales::label_number(big.mark = ".")) +
  labs(x = NULL, y = "Worte pro Text", title = "Textlänge pro Fandom") + th

ggsave(file.path(OUT_DIR, "text_length_by_fandom_2.png"), p2_box, width = 10, height = 6, dpi = 300, bg = "white")



author_tiers <- per_author %>%
  mutate(tier = case_when(
    texts == 1 ~ "1 Text",
    texts >= 2 & texts <= 4 ~ "2–4 Texte",
    texts >= 5 & texts <= 9 ~ "5–9 Texte",
    texts >= 10 ~ "10+ Texte"
  )) %>%
  mutate(tier = factor(tier, levels = c("1 Text", "2–4 Texte", "5–9 Texte", "10+ Texte"))) %>%
  count(tier)

p3_tiers <- ggplot(author_tiers, aes(x = tier, y = n)) +
  geom_col(fill = "#7671a8") +
  scale_y_continuous(labels = scales::label_number(big.mark = ".")) +
  labs(x = "Texte pro Autor*in", y = "Anzahl Autor*innen", 
       title = "Produktivitätsverteilung") + th

ggsave(file.path(OUT_DIR, "author_tiers_2.png"), p3_tiers, width = 8, height = 5, dpi = 300, bg = "white")



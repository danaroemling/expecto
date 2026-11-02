# Overview

This repository has all code that was used in the analysis of the "Expecto Auctorem" paper.

The data for this work is the Darmstadt Fanfiction Corpus: Glawion, Anastasia, und Thomas Weitin. „Darmstadt Fanfiction Corpus 1.0 (Fanfiktion.de, 2020–2023)". TU Darmstadt, 2024. [https://doi.org/10.48328/TUDATALIB-1452](https://doi.org/10.48328/TUDATALIB-1452)

The analysis focuses on individual linguistic preferences in recurring grammatical patterns in German-language Harry Potter fanfiction. The scripts cover corpus preparation, authorship analysis using part-of-speech n-grams, and subsequent investigations of author-specific grammatical patterns, their lexical realisations and their recurrence across stories.

## Scripts

### 01_count_corpus_tokens.R

Tokenises the original corpus using UDPipe and calculates token and word counts for each text. Processing is distributed across 50 SLURM array tasks.

### 02_describe_corpus.R

Generates descriptive statistics and visualisations of the original corpus, including text lengths, author productivity, fandom distributions and word counts.

### 03_create_experimental_corpus.R

Selects the Harry Potter romance subcorpus, identifies eligible authors and constructs the experimental dataset. Creates 15 training and 5 test chunks per author, each containing exactly 1,000 words plus punctuation. Training and test chunks originate from different stories.

### 04_tag_corpus_pos.R

Annotates the experimental corpus using the German HDT UDPipe model while preserving the existing tokenisation. Extracts XPOS, UPOS, lemmas and morphological features. POS tagging is distributed across 50 SLURM array tasks, followed by assembly and validation.

### 05_extract_xpos_ngrams.R

Extracts XPOS n-grams of lengths 1–5, including punctuation, and constructs sparse document-feature matrices containing raw occurrence counts. Missing XPOS tags are replaced with `XY` during processing. Features are retained if they occur in at least three training chunks.

### 06_classify_authorship.R

Trains and evaluates linear SVM classifiers across 15 experimental conditions, combining five n-gram lengths with minimum document-frequency thresholds of 3, 5 and 10. Hyperparameters are selected using story-based cross-validation on the training data, with final evaluation on held-out test chunks.

### 07_evaluate_results.R

Combines classification results across experimental conditions and summarises authorship analysis performance to assess the effects of grammatical sequence length and feature-selection thresholds.

### 08_analyse_xpos_preferences.R

Investigates individual preferences for recurring grammatical patterns using XPOS bigrams and trigrams. Compares pattern frequencies across authors, identifies overrepresented patterns and examines their distribution across stories.

### 09_analyse_xpos_examples.R

Examines the lexical realisations of selected grammatical patterns. Extracts the words and contexts associated with XPOS sequences and summarises their occurrence across chunks and stories.

### 10_lexical_author_specificity.R

Investigates the author specificity of lexical sequences associated with grammatical patterns. Compares lexical realisations across authors and examines their recurrence across stories to assess the relationship between grammatical preferences and repeated lexical sequences.

## Additional scripts

### inspect_missing_xpos.R

Identifies tokens without XPOS annotations and summarises their distribution across tokens, chunks and authors.

### inspect_author_eligibility.R

Investigates author eligibility and the feasibility of separating training and test material at the story level.

### inspect_cv.R

Inspects cross-validation results and compares them with test performance.

## Requirements

R packages:

- tidyverse
- udpipe
- quanteda
- Matrix
- LiblineaR
- data.table
- fs

The scripts use the German HDT UDPipe model for linguistic annotation. Computationally intensive processing was conducted using SLURM array jobs.

File paths in the scripts use placeholders and must be adjusted to the local environment. The original corpus and derived datasets are not included in this repository. Please contact the corpus convenors to request access to the data (see intro).

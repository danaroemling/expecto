#!/usr/bin/env Rscript
# Run as array job NGRAM_N=1,...,5. Does not modify original corpus.
suppressPackageStartupMessages({
  library(quanteda)
  library(Matrix)
})
BASE <- 'PATH/Misc/FF_Corpus'
DIR <- file.path(BASE, 'HP_Liebesgeschichte_1000word_chunks_FINAL', 'POS_tagged')
OUT <- file.path(DIR, 'experiment_xpos')
dir.create(OUT, recursive=TRUE, showWarnings=FALSE)
n <- as.integer(Sys.getenv('NGRAM_N', unset=Sys.getenv('SLURM_ARRAY_TASK_ID', unset='')))
if (is.na(n) || !(n %in% 1:5)) stop('Set NGRAM_N or SLURM_ARRAY_TASK_ID to 1,...,5')
cat('Extracting XPOS', n, '-grams\n')
d <- readRDS(file.path(DIR, 'tagged_chunks.rds'))
required <- c('author_id','set','final_filename','text_id','xpos_sequence')
if (!all(required %in% names(d))) stop('Missing required columns: ',paste(setdiff(required,names(d)),collapse=', '))
if (nrow(d)!=8180L || sum(d$set=='train')!=6135L || sum(d$set=='test')!=2045L || anyDuplicated(d$final_filename)) stop('Unexpected chunk manifest')
# Replace complete NA tags only, not substrings of valid XPOS labels.
seqs <- strsplit(trimws(as.character(d$xpos_sequence)), '[[:space:]]+')
na_before <- sum(vapply(seqs, function(z) sum(z=='NA'), integer(1)))
if (na_before != 950L) stop('Expected 950 literal NA tags; found ',na_before,'; inspect before proceeding')
seqs <- lapply(seqs, function(z) {z[z=='NA'] <- 'XY'; z})
if (any(vapply(seqs, function(z) any(is.na(z)|z==''|z=='NA'), logical(1)))) stop('Invalid POS tags remain')
if (sum(lengths(seqs))!=9584239L) stop('POS token count mismatch')
cat('Replaced',na_before,'NA tags with XY in memory\n')
# Construct each order once. quanteda retains punctuation tags as regular tokens.
# Passing a list of token vectors to as.tokens() avoids text re-tokenisation.
toks <- quanteda::as.tokens(seqs)
rm(seqs); gc()
ng <- quanteda::tokens_ngrams(toks, n=n, concatenator='__')
rm(toks); gc()
xdfm <- quanteda::dfm(ng, tolower=FALSE)
rm(ng); gc()
if (quanteda::ndoc(xdfm)!=8180L) stop('Document count changed')
# Document frequency is calculated EXCLUSIVELY on training chunks.
train <- which(d$set=='train')
df_train <- quanteda::docfreq(xdfm[train, ])
keep <- which(df_train>=3L)
cat('Features with training DF>=3:',length(keep),'\n')
if (!length(keep)) stop('No features retained')
xdfm <- xdfm[, keep]
feature_names <- quanteda::featnames(xdfm)
feature_df <- as.integer(df_train[keep])
rm(df_train); gc()
x <- methods::as(xdfm, 'dgCMatrix')
rm(xdfm); gc()
if (nrow(x)!=8180L || ncol(x)!=length(feature_names)) stop('Matrix dimensions invalid')
# The same rows and column vocabulary will be used for all three thresholds.
metadata <- d[, intersect(c('author_id','author_username','set','final_filename','text_id','story_chunk','split_chunk'),names(d)),drop=FALSE]
file <- file.path(OUT,sprintf('features_n%d_df3plus.rds',n))
tmp <- paste0(file,'.tmp')
saveRDS(list(x=x, metadata=metadata, feature_names=feature_names, feature_df=feature_df,
             ngram=n, min_df_extracted=3L, na_replaced=na_before), tmp, compress=FALSE)
if (!file.rename(tmp,file)) stop('Failed to publish output file')
cat('Saved:',file,'\nRows:',nrow(x),' Features:',ncol(x),' Nonzeros:',length(x@x),'\n')

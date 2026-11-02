# ============================================================
# Token counts per text, one SLURM array task per slice of the corpus
# Tokens = UDPipe tokeniser output (no tagging, no parsing)
# Words  = tokens containing at least one letter or digit
# ============================================================

BASE <- "PATH/Misc/FF_Corpus"

lib_path <- file.path(BASE, "r_libs")
.libPaths(c(lib_path, .libPaths()))
library(udpipe)

# ---- 0. CONFIG (nothing to edit) ----
CORPUS_DIR <- file.path(BASE, "Texte_txt_all")
PARTS_DIR  <- file.path(BASE, "counts_parts")
N_TASKS    <- 50      # matches --array=1-50 in the job script
dir.create(PARTS_DIR, showWarnings = FALSE)

models <- list.files(BASE, pattern = "^german-hdt-ud-2\\.5-191206\\.udpipe$",
                     recursive = TRUE, full.names = TRUE)
if (length(models) == 0) stop("Model german-hdt-ud-2.5-191206.udpipe not found under ", BASE)
MODEL_FILE <- models[1]
cat("Model:", MODEL_FILE, "\n")
stopifnot(dir.exists(CORPUS_DIR))

# ---- 1. WORK OUT THIS TASK'S SLICE ----
task_id <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
if (is.na(task_id)) task_id <- 1
all_files <- sort(list.files(CORPUS_DIR, pattern = "\\.txt$"))
slice <- split(all_files, cut(seq_along(all_files), N_TASKS, labels = FALSE))[[task_id]]
cat(sprintf("[Task %d] %d of %d texts\n", task_id, length(slice), length(all_files)))

# ---- 2. COUNT ----
# If a task is killed, submit it again: texts already counted are skipped.
model <- udpipe_load_model(MODEL_FILE)

count_file <- function(fn) {
  x <- paste(readLines(file.path(CORPUS_DIR, fn), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  x <- iconv(x, "UTF-8", "UTF-8", sub = "")
  a <- as.data.frame(udpipe_annotate(model, x = x, tagger = "none", parser = "none"))
  a <- a[!grepl("-", as.character(a$token_id)), ]   # drops multiword-token range entries if the model has them
  data.frame(filename = fn, tokens = nrow(a), words = sum(grepl("[[:alnum:]]", a$token)),
             stringsAsFactors = FALSE)
}

part_file <- file.path(PARTS_DIR, sprintf("part_%03d.rds", task_id))
done <- if (file.exists(part_file)) readRDS(part_file) else
  data.frame(filename = character(), tokens = integer(), words = integer(), stringsAsFactors = FALSE)
todo <- setdiff(slice, done$filename)
cat(sprintf("[Task %d] %d already counted, %d to do\n", task_id, nrow(done), length(todo)))

buffer <- list()
for (i in seq_along(todo)) {
  buffer[[length(buffer) + 1]] <- tryCatch(count_file(todo[i]), error = function(e) {
    cat(sprintf("ERROR in %s: %s\n", todo[i], e$message))
    NULL
  })
  if (i %% 100 == 0 || i == length(todo)) {
    done <- rbind(done, do.call(rbind, buffer))
    buffer <- list()
    saveRDS(done, part_file)
    cat(sprintf("[Task %d] %d of %d done\n", task_id, i, length(todo)))
  }
}
cat(sprintf("[Task %d] Finished\n", task_id))
#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readxl)
})

dir.create("results", showWarnings = FALSE, recursive = TRUE)
file_arg <- commandArgs(trailingOnly = FALSE)
file_arg <- file_arg[grepl("^--file=", file_arg)]
script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[1]) else "r_script.R"
script_name <- sub("\\.R$", "", basename(script_path))
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
log_file <- file.path("results", paste0(script_name, "_", timestamp, ".log"))
log_con <- file(log_file, open = "wt")
sink(log_con, split = TRUE)
sink(log_con, type = "message")
on.exit({
  sink(type = "message")
  sink()
  close(log_con)
}, add = TRUE)
cat("Saving output to:", log_file, "\n")

input_file <- "merge_data.xlsx"
if (length(commandArgs(trailingOnly = TRUE)) >= 1) {
  input_file <- commandArgs(trailingOnly = TRUE)[1]
}

normalize_name <- function(x) {
  x <- tolower(x)
  x <- gsub("[\r\n\t]+", " ", x)
  x <- gsub("[[:space:]]+", " ", x)
  trimws(x)
}

get_col <- function(df, patterns, label) {
  nm <- names(df)
  norm <- normalize_name(nm)
  idx <- rep(FALSE, length(nm))
  for (p in patterns) {
    idx <- idx | grepl(p, norm, perl = TRUE)
  }
  matches <- which(idx)
  if (length(matches) == 0) {
    stop(sprintf("Could not find required column for '%s'.", label))
  }
  if (length(matches) > 1) {
    stop(sprintf("Multiple columns matched '%s': %s", label, paste(nm[matches], collapse = " | ")))
  }
  message(sprintf("Matched '%s' -> '%s'", label, nm[matches]))
  df[[matches]]
}

get_col_info <- function(df, patterns, label) {
  nm <- names(df)
  norm <- normalize_name(nm)
  idx <- rep(FALSE, length(nm))
  for (p in patterns) {
    idx <- idx | grepl(p, norm, perl = TRUE)
  }
  matches <- which(idx)
  if (length(matches) == 0) {
    stop(sprintf("Could not find required column for '%s'.", label))
  }
  if (length(matches) > 1) {
    stop(sprintf("Multiple columns matched '%s': %s", label, paste(nm[matches], collapse = " | ")))
  }
  list(values = df[[matches]], matched_col = nm[matches])
}

raw_df <- read_excel(input_file)
selected_specs <- list(
  list(alias = "lucid", patterns = c("were you at any point lucid")),
  list(alias = "arousalN", patterns = c("^arousaln$")),
  list(alias = "totalCues", patterns = c("^totalcues$")),
  list(alias = "cue_notice", patterns = c("did you notice any cues while you were dreaming")),
  list(alias = "highestVol", patterns = c("^highestvol$")),
  list(alias = "wakeThresh", patterns = c("^wakethresh$")),
  list(alias = "time_asleep", patterns = c("^time asleep$")),
  list(alias = "sleep_quality", patterns = c("^sleep quality$", "how would you rate your sleep quality")),
  list(alias = "wake_up_duration", patterns = c("how long did you wake up", "if you woke up one or more times.*how long")),
  list(alias = "pid", patterns = c("^pid$"))
)

selected_values <- list()
selected_missing_table <- data.frame(
  alias = character(0),
  matched_column = character(0),
  missing_n = integer(0),
  missing_pct = numeric(0)
)

for (spec in selected_specs) {
  info <- get_col_info(raw_df, spec$patterns, spec$alias)
  selected_values[[spec$alias]] <- info$values
  missing_n <- sum(is.na(info$values))
  selected_missing_table <- rbind(
    selected_missing_table,
    data.frame(
      alias = spec$alias,
      matched_column = info$matched_col,
      missing_n = as.integer(missing_n),
      missing_pct = round((missing_n / nrow(raw_df)) * 100, 2)
    )
  )
}

# Replace raw column names by matched aliases when available.
display_name_map <- setNames(selected_missing_table$alias, selected_missing_table$matched_column)
raw_col_names <- names(raw_df)
display_col_names <- ifelse(
  raw_col_names %in% names(display_name_map),
  unname(display_name_map[raw_col_names]),
  raw_col_names
)

diag_df <- data.frame(
  lucid = selected_values[["lucid"]],
  arousalN = selected_values[["arousalN"]],
  totalCues = selected_values[["totalCues"]],
  cue_notice = selected_values[["cue_notice"]],
  highestVol = selected_values[["highestVol"]],
  wakeThresh = selected_values[["wakeThresh"]],
  sleep_quality = selected_values[["sleep_quality"]],
  wake_up_duration = selected_values[["wake_up_duration"]],
  pid = selected_values[["pid"]],
  time_asleep = selected_values[["time_asleep"]],
  stringsAsFactors = FALSE
)

# Convert numeric-like columns
diag_df$lucid <- suppressWarnings(as.numeric(diag_df$lucid))
diag_df$arousalN <- suppressWarnings(as.numeric(diag_df$arousalN))
diag_df$totalCues <- suppressWarnings(as.numeric(diag_df$totalCues))
diag_df$highestVol <- suppressWarnings(as.numeric(diag_df$highestVol))
diag_df$wakeThresh <- suppressWarnings(as.numeric(diag_df$wakeThresh))
diag_df$time_asleep <- suppressWarnings(as.numeric(diag_df$time_asleep))

# Try to map cue_notice to numeric when possible.
cue_num <- suppressWarnings(as.numeric(diag_df$cue_notice))
if (all(is.na(cue_num))) {
  cue_txt <- normalize_name(as.character(diag_df$cue_notice))
  cue_num <- ifelse(
    grepl("^1$|^yes$|true|noticed", cue_txt),
    1,
    ifelse(grepl("^0$|^no$|false|did not|none", cue_txt), 0, NA_real_)
  )
}
diag_df$cue_notice <- cue_num

na_counts <- colSums(is.na(raw_df))
names(na_counts) <- display_col_names
zero_totalcues <- sum(diag_df$totalCues == 0, na.rm = TRUE)

safe_arousal_rate <- ifelse(diag_df$totalCues == 0, NA_real_, diag_df$arousalN / diag_df$totalCues)
arousal_rate_na <- sum(is.na(safe_arousal_rate))
arousal_rate_nan <- sum(is.nan(safe_arousal_rate))

n_total <- nrow(diag_df)
n_complete_core_cols <- sum(
  complete.cases(diag_df[, c("cue_notice", "wakeThresh", "highestVol", "sleep_quality", "wake_up_duration", "pid", "totalCues")])
)
n_valid_arousal_rate <- sum(!is.na(safe_arousal_rate))
n_complete_requested_model <- sum(
  complete.cases(diag_df[, c("cue_notice", "wakeThresh", "highestVol", "sleep_quality", "wake_up_duration", "pid", "totalCues")]) &
    !is.na(safe_arousal_rate)
)

cat("\n===== Missing Values by Column =====\n")
print(na_counts)

cat("\n===== Zero totalCues Check =====\n")
cat("Rows with totalCues == 0:", zero_totalcues, "\n")

cat("\n===== Arousal Rate NA/NaN Check =====\n")
cat("Rows where safe arousal_rate is NA:", arousal_rate_na, "\n")
cat("Rows where safe arousal_rate is NaN:", arousal_rate_nan, "\n")

cat("\n===== Data Loss Summary =====\n")
cat("Total rows in input:", n_total, "\n")
cat(
  "Rows complete for [cue_notice, wakeThresh, highestVol, sleep_quality, wake_up_duration, pid, totalCues]:",
  n_complete_core_cols,
  "\n"
)
cat("Rows with valid arousal_rate (arousalN/totalCues, NA when totalCues == 0):", n_valid_arousal_rate, "\n")
cat(
  "Rows complete for requested model terms [arousal_rate, cue_notice, wakeThresh, highestVol, sleep_quality, wake_up_duration, (1|pid), totalCues]:",
  n_complete_requested_model,
  "\n"
)
cat(
  "Rows dropped due to missingness in requested non-arousal columns:",
  n_total - n_complete_core_cols,
  "\n"
)
cat(
  "Rows dropped due to invalid arousal_rate (missing arousalN/totalCues or totalCues == 0):",
  n_total - n_valid_arousal_rate,
  "\n"
)
cat(
  "Final rows dropped before modeling (all requested terms):",
  n_total - n_complete_requested_model,
  "\n"
)


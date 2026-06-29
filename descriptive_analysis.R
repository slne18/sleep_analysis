#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readxl)
})

file_arg <- commandArgs(trailingOnly = FALSE)
file_arg <- file_arg[grepl("^--file=", file_arg)]
script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[1]) else "r_script.R"
script_name <- sub("\\.R$", "", basename(script_path))
script_dir <- normalizePath(dirname(script_path), winslash = "/", mustWork = FALSE)
results_dir <- file.path(script_dir, "results")
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

log_file <- file.path(results_dir, paste0(script_name, "_", timestamp, ".log"))
log_con <- file(log_file, open = "wt")
sink(log_con, split = TRUE)
sink(log_con, type = "message")
on.exit({
  sink(type = "message")
  sink()
  close(log_con)
}, add = TRUE)
cat("Saving output to:", log_file, "\n")

input_candidates <- c(
  "merge_data_cleaned.xlsx",
  "data/merge_data_cleaned.xlsx",
  file.path(script_dir, "merge_data_cleaned.xlsx"),
  file.path(script_dir, "data/merge_data_cleaned.xlsx")
)
existing_candidates <- input_candidates[file.exists(input_candidates)]
if (length(existing_candidates) == 0) {
  stop("Could not find merge_data_cleaned.xlsx. Tried: ", paste(input_candidates, collapse = ", "))
}
input_file <- existing_candidates[1]
cat("Using input file:", input_file, "\n")

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

get_col_optional <- function(df, patterns, label) {
  nm <- names(df)
  norm <- normalize_name(nm)
  idx <- rep(FALSE, length(nm))
  for (p in patterns) {
    idx <- idx | grepl(p, norm, perl = TRUE)
  }
  matches <- which(idx)
  if (length(matches) == 0) {
    message(sprintf("Optional column '%s' not found. Filling with NA.", label))
    return(rep(NA_real_, nrow(df)))
  }
  if (length(matches) > 1) {
    stop(sprintf("Multiple columns matched optional '%s': %s", label, paste(nm[matches], collapse = " | ")))
  }
  message(sprintf("Matched optional '%s' -> '%s'", label, nm[matches]))
  suppressWarnings(as.numeric(df[[matches]]))
}

to_sleep_quality_num <- function(x) {
  txt <- normalize_name(as.character(x))
  out <- suppressWarnings(as.numeric(txt))
  needs_map <- is.na(out) & !is.na(txt) & nzchar(txt)
  if (any(needs_map)) {
    mapped <- rep(NA_real_, length(txt))
    mapped[grepl("^very poor$|^very bad$", txt)] <- 1
    mapped[grepl("^poor$|^bad$", txt)] <- 2
    mapped[grepl("^fair$|^ok$|^okay$|^average$|^neutral$", txt)] <- 3
    mapped[grepl("^good$", txt)] <- 4
    mapped[grepl("^very good$|^excellent$", txt)] <- 5
    out[needs_map] <- mapped[needs_map]
  }
  out
}

to_wake_duration_min <- function(x) {
  txt <- normalize_name(as.character(x))
  numeric_direct <- suppressWarnings(as.numeric(txt))
  ifelse_numeric <- !is.na(numeric_direct) & nzchar(txt)
  out <- rep(NA_real_, length(txt))
  out[ifelse_numeric] <- numeric_direct[ifelse_numeric]
  out[grepl("^0\\s*-\\s*15", txt)] <- 7.5
  out[grepl("^15\\s*-\\s*30", txt)] <- 22.5
  out[grepl("^30\\s*-\\s*45", txt)] <- 37.5
  out[grepl("^45\\s*-\\s*60", txt)] <- 52.5
  out
}

raw_df <- read_excel(input_file)

# Same robust matching style as GLMM scripts, with easy names.
df <- data.frame(
  lucid = get_col(raw_df, c("^lucid$", "were you at any point lucid"), "lucid outcome"),
  cue_notice = get_col(raw_df, c("^cue_notice$", "did you notice any cues"), "cue notice"),
  pid = get_col(raw_df, c("^pid$"), "pid"),
  wakeThresh = get_col(raw_df, c("^wakethresh$"), "wakeThresh"),
  highestVol = get_col(raw_df, c("^highestvol$"), "highestVol"),
  arousalN = get_col(raw_df, c("^arousaln$"), "arousalN"),
  totalCues = get_col(raw_df, c("^totalcues$"), "totalCues"),
  had_arousal = get_col(raw_df, c("^had_arousal$"), "had_arousal"),
  cued_night = get_col(raw_df, c("^cued_night$"), "cued_night"),
  wake_up_duration_raw = get_col(
    raw_df,
    c("^wake_up_duration$", "how long did you wake up", "if you woke up one or more times.*how long"),
    "wake up duration"
  ),
  sleep_quality = get_col(raw_df, c("^sleep_quality$", "^sleep quality$", "how would you rate your sleep quality"), "sleep quality"),
  lucid_dreams_freq_pw = if ("lucid_dreams_freq_pw" %in% names(raw_df)) {
    suppressWarnings(as.numeric(raw_df$lucid_dreams_freq_pw))
  } else {
    suppressWarnings(as.numeric(get_col(
      raw_df,
      c("^lucid_dreams_past_week$", "how many lucid dreams.*past week"),
      "lucid dreams past week"
    ))) / 7
  },
  lucid_attempts_freq_pw = if ("lucid_attempts_freq_pw" %in% names(raw_df)) {
    suppressWarnings(as.numeric(raw_df$lucid_attempts_freq_pw))
  } else {
    suppressWarnings(as.numeric(get_col(
      raw_df,
      c("^lucid_attempts_past_week$", "lucid dreaming attempts in past week"),
      "lucid attempts past week"
    ))) / 7
  },
  age = get_col(raw_df, c("^age$", "^what is your age\\??"), "age"),
  time_asleep = get_col(raw_df, c("^time_asleep$", "^time asleep$"), "time asleep"),
  overall_motion = get_col(raw_df, c("^overall_motion$"), "overall_motion"),
  stimulation_motion = get_col(raw_df, c("^stimulation_motion$"), "stimulation_motion"),
  fitbit_overall_motion = get_col_optional(raw_df, c("^fitbit_overall_motion$", "^fitbit_wrist_overall_motion$"), "fitbit_overall_motion"),
  fitbit_stim_motion = get_col_optional(raw_df, c("^fitbit_stim_motion$", "^fitbit_wrist_stim_motion$"), "fitbit_stim_motion"),
  cue_delta_var = get_col_optional(raw_df, c("^cue_delta_var$"), "cue_delta_var"),
  high_freq = get_col_optional(raw_df, c("^high_freq$"), "high_freq"),
  low_freq = get_col_optional(raw_df, c("^low_freq$"), "low_freq"),
  gender = get_col(raw_df, c("^gender$", "^what is your gender\\??"), "gender"),
  stringsAsFactors = FALSE
)

numeric_cols <- c(
  "wakeThresh", "highestVol", "arousalN", "totalCues", "had_arousal", "cued_night",
  "lucid_dreams_freq_pw", "lucid_attempts_freq_pw", "age", "time_asleep",
  "overall_motion", "stimulation_motion", "fitbit_overall_motion", "fitbit_stim_motion",
  "cue_delta_var", "high_freq", "low_freq"
)
for (col in numeric_cols) {
  df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
}

df$sleep_quality_num <- to_sleep_quality_num(df$sleep_quality)
df$wake_up_duration_min <- to_wake_duration_min(df$wake_up_duration_raw)
df$arousal_rate <- ifelse(!is.na(df$totalCues) & df$totalCues > 0, df$arousalN / df$totalCues, NA_real_)

# Analyze numeric columns only, excluding pid/lucid/cue_notice by request.
analysis_cols <- c(
  "wakeThresh", "highestVol", "arousalN", "totalCues", "arousal_rate",
  "sleep_quality_num", "wake_up_duration_min",
  "lucid_dreams_freq_pw", "lucid_attempts_freq_pw", "age", "time_asleep",
  "overall_motion", "stimulation_motion", "fitbit_overall_motion", "fitbit_stim_motion",
  "cue_delta_var", "high_freq", "low_freq"
)

analysis_df <- df[, analysis_cols, drop = FALSE]

# Keep rows needed for lucid-rate summaries.
model_df <- df[
  df$lucid %in% c(0, 1) &
    !is.na(df$had_arousal) &
    !is.na(df$cued_night),
  ,
  drop = FALSE
]
cued_df <- model_df[model_df$cued_night == 1, , drop = FALSE]

total_rows <- nrow(df)
rows_totalcues_nonzero <- sum(df$totalCues != 0, na.rm = TRUE)
rows_totalcues_non_missing <- sum(!is.na(df$totalCues))
rows_totalcues_nonzero_pct <- if (rows_totalcues_non_missing > 0) {
  100 * rows_totalcues_nonzero / rows_totalcues_non_missing
} else {
  NA_real_
}
rows_arousaln_positive <- sum(df$arousalN > 0, na.rm = TRUE)
rows_arousaln_non_missing <- sum(!is.na(df$arousalN))
rows_arousaln_positive_pct <- if (rows_arousaln_non_missing > 0) {
  100 * rows_arousaln_positive / rows_arousaln_non_missing
} else {
  NA_real_
}

pid_non_missing <- !is.na(df$pid) & nzchar(as.character(df$pid))
rows_with_pid <- sum(pid_non_missing)
participant_nights <- table(df$pid[pid_non_missing])
participant_count <- length(participant_nights)
avg_nights_per_participant <- if (participant_count > 0) {
  as.numeric(mean(participant_nights))
} else {
  NA_real_
}

summarise_one <- function(x, nm) {
  x <- x[is.finite(x)]
  if (length(x) == 0) {
    return(data.frame(
      variable = nm, n = 0, mean = NA_real_, sd = NA_real_, median = NA_real_,
      p25 = NA_real_, p75 = NA_real_, min = NA_real_, max = NA_real_, range = NA_real_
    ))
  }
  data.frame(
    variable = nm,
    n = length(x),
    mean = mean(x),
    sd = sd(x),
    median = median(x),
    p25 = unname(quantile(x, 0.25)),
    p75 = unname(quantile(x, 0.75)),
    min = min(x),
    max = max(x),
    range = max(x) - min(x)
  )
}

fmt_mean_sd <- function(x, digits = 1) {
  x <- x[is.finite(x)]
  if (length(x) == 0) {
    return(NA_character_)
  }
  sprintf(
    paste0("%.", digits, "f (%.", digits, "f)"),
    mean(x),
    sd(x)
  )
}

fmt_median_iqr <- function(x, digits = 1) {
  x <- x[is.finite(x)]
  if (length(x) == 0) {
    return(NA_character_)
  }
  q <- unname(quantile(x, c(0.25, 0.5, 0.75), na.rm = TRUE))
  sprintf(
    paste0("%.", digits, "f [%.", digits, "f, %.", digits, "f]"),
    q[2],
    q[1],
    q[3]
  )
}

fmt_n_pct <- function(n, denom, digits = 1) {
  if (denom <= 0) {
    return(sprintf("0 (0.%d%%)", digits))
  }
  sprintf(paste0("%d (%.", digits, "f%%)"), n, 100 * n / denom)
}

add_sample_row <- function(rows, characteristic, value, n = NA_integer_, statistic = "") {
  rbind(
    rows,
    data.frame(
      characteristic = characteristic,
      statistic = statistic,
      value = value,
      n = n,
      stringsAsFactors = FALSE
    )
  )
}

add_continuous_sample_rows <- function(rows, label, x, digits = 1) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  n <- length(x)
  rows <- add_sample_row(rows, label, fmt_mean_sd(x, digits = digits), n, "mean (SD)")
  add_sample_row(rows, label, fmt_median_iqr(x, digits = digits), n, "median [IQR]")
}

build_sample_characteristics_table <- function(df) {
  pid_ok <- !is.na(df$pid) & nzchar(as.character(df$pid))
  participant_df <- df[pid_ok, , drop = FALSE]
  participant_df <- participant_df[
    order(participant_df$pid),
    ,
    drop = FALSE
  ]
  participant_df <- participant_df[
    !duplicated(participant_df$pid),
    ,
    drop = FALSE
  ]

  nights_per_participant <- as.numeric(table(df$pid[pid_ok]))
  participant_n <- length(nights_per_participant)
  total_nights <- sum(nights_per_participant)

  gender_vals <- as.character(participant_df$gender)
  gender_vals[is.na(gender_vals) | !nzchar(gender_vals)] <- "Missing/Unknown"
  gender_tab <- sort(table(gender_vals), decreasing = TRUE)
  gender_denom <- sum(gender_tab)

  sample_rows <- data.frame(
    characteristic = character(),
    statistic = character(),
    value = character(),
    n = integer(),
    stringsAsFactors = FALSE
  )

  sample_rows <- add_sample_row(sample_rows, "Participants", as.integer(participant_n), participant_n, "n")
  sample_rows <- add_sample_row(sample_rows, "Nights recorded", as.integer(total_nights), total_nights, "n")
  sample_rows <- add_continuous_sample_rows(sample_rows, "Nights per participant", nights_per_participant)
  sample_rows <- add_continuous_sample_rows(sample_rows, "Age (years)", participant_df$age)
  for (level_name in names(gender_tab)) {
    sample_rows <- add_sample_row(
      sample_rows,
      "Sex",
      fmt_n_pct(as.integer(gender_tab[[level_name]]), gender_denom),
      gender_denom,
      level_name
    )
  }
  sample_rows <- add_continuous_sample_rows(
    sample_rows,
    "Lucid dreams per day (past week)",
    participant_df$lucid_dreams_freq_pw
  )
  sample_rows <- add_continuous_sample_rows(
    sample_rows,
    "Lucid dream attempts per day (past week)",
    participant_df$lucid_attempts_freq_pw
  )

  lucid_rate <- tapply(
    df$lucid[pid_ok & df$lucid %in% c(0, 1)],
    df$pid[pid_ok & df$lucid %in% c(0, 1)],
    mean
  )
  sample_rows <- add_continuous_sample_rows(
    sample_rows,
    "Lucid dream rate (proportion of recorded nights)",
    as.numeric(lucid_rate)
  )

  sample_rows
}

stats_list <- lapply(names(analysis_df), function(nm) summarise_one(analysis_df[[nm]], nm))
stats_df <- do.call(rbind, stats_list)
median_sd <- median(stats_df$sd[is.finite(stats_df$sd) & stats_df$sd > 0], na.rm = TRUE)
stats_df$sd_vs_median_sd <- if (is.finite(median_sd) && median_sd > 0) stats_df$sd / median_sd else NA_real_
stats_df$large_scale_flag <- ifelse(!is.na(stats_df$sd_vs_median_sd) & stats_df$sd_vs_median_sd >= 5, "YES", "")
stats_df <- stats_df[order(-stats_df$sd, stats_df$variable), , drop = FALSE]

sample_df <- build_sample_characteristics_table(df)
sample_file <- file.path(results_dir, paste0(script_name, "_sample_characteristics_", timestamp, ".csv"))
write.csv(sample_df, sample_file, row.names = FALSE)

cat("\n===== Sample Characteristics (participant-level) =====\n")
print(sample_df, row.names = FALSE)

stats_file <- file.path(results_dir, paste0(script_name, "_numeric_stats_", timestamp, ".csv"))
write.csv(stats_df, stats_file, row.names = FALSE)

cat("\n===== Numeric Descriptive Stats (requested variables only) =====\n")
print(stats_df)
cat(
  "\nRows with totalCues != 0:",
  rows_totalcues_nonzero, "/", total_rows,
  sprintf("(%.2f%% of rows with non-missing totalCues)", rows_totalcues_nonzero_pct),
  "\n"
)
cat(
  "Rows with arousalN > 0:",
  rows_arousaln_positive, "/", total_rows,
  sprintf("(%.2f%% of rows with non-missing arousalN)", rows_arousaln_positive_pct),
  "\n"
)
cat(
  "Average nights per participant:",
  sprintf("%.2f", avg_nights_per_participant),
  sprintf("(participants with non-missing pid: %d, rows with non-missing pid: %d, total rows: %d)", participant_count, rows_with_pid, total_rows),
  "\n"
)

# 1) Lucid rate on arousal vs non-arousal nights (all nights in model_df).
cat("\n===== Lucid Rate by Arousal (all model_df nights) =====\n")
if (nrow(model_df) == 0) {
  cat("No rows available after filtering lucid/had_arousal/cued_night.\n")
} else {
  lucid_by_arousal <- aggregate(
    lucid ~ had_arousal,
    data = model_df,
    FUN = mean
  )
  names(lucid_by_arousal)[names(lucid_by_arousal) == "lucid"] <- "lucid_rate"
  n_by_arousal <- as.data.frame(table(model_df$had_arousal), stringsAsFactors = FALSE)
  names(n_by_arousal) <- c("had_arousal", "n")
  n_by_arousal$had_arousal <- suppressWarnings(as.numeric(as.character(n_by_arousal$had_arousal)))
  lucid_by_arousal <- merge(n_by_arousal, lucid_by_arousal, by = "had_arousal", all.x = TRUE, sort = TRUE)
  print(lucid_by_arousal)
}

# 2) Lucid rate on cued nights with vs without arousal.
cat("\n===== Lucid Rate by Arousal (cued nights only) =====\n")
if (nrow(cued_df) == 0) {
  cat("No cued-night rows available.\n")
} else {
  lucid_cued_by_arousal <- aggregate(
    lucid ~ had_arousal,
    data = cued_df,
    FUN = mean
  )
  names(lucid_cued_by_arousal)[names(lucid_cued_by_arousal) == "lucid"] <- "lucid_rate"
  n_cued_by_arousal <- as.data.frame(table(cued_df$had_arousal), stringsAsFactors = FALSE)
  names(n_cued_by_arousal) <- c("had_arousal", "n")
  n_cued_by_arousal$had_arousal <- suppressWarnings(as.numeric(as.character(n_cued_by_arousal$had_arousal)))
  lucid_cued_by_arousal <- merge(
    n_cued_by_arousal,
    lucid_cued_by_arousal,
    by = "had_arousal",
    all.x = TRUE,
    sort = TRUE
  )
  print(lucid_cued_by_arousal)
}

# 3) Cross-tabulation of arousal and cued nights.
cat("\n===== Cross-tab: had_arousal x cued_night =====\n")
if (nrow(model_df) == 0) {
  cat("No rows available for cross-tab.\n")
} else {
  print(table(model_df$had_arousal, model_df$cued_night))
}

cat("\nSaved sample characteristics to:", sample_file, "\n")
cat("Saved numeric stats to:", stats_file, "\n")

#!/usr/bin/env Rscript

file_arg <- commandArgs(trailingOnly = FALSE)
file_arg <- file_arg[grepl("^--file=", file_arg)]
script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[1]) else "model4_overall_motion.R"
script_dir <- normalizePath(dirname(script_path), winslash = "/", mustWork = FALSE)
script_name <- sub("\\.R$", "", basename(script_path))
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

local_lib_candidates <- c(
  file.path(script_dir, "r_libs"),
  file.path(script_dir, "../r_libs"),
  "/Users/solenenoize/Desktop/sleep_analysis/r_libs"
)
existing_local_libs <- local_lib_candidates[dir.exists(local_lib_candidates)]
if (length(existing_local_libs) > 0) {
  .libPaths(c(existing_local_libs, .libPaths()))
}

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(glmmTMB)
  library(DHARMa)
})

# Toggle: TRUE = z-score selected predictors, FALSE = use raw scale.
USE_Z_SCORING <- TRUE

input_file <- "/Users/solenenoize/Desktop/sleep_analysis/data/merge_data_cleaned.xlsx"
if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file)
}

raw_df <- read_excel(input_file)

motion_file <- "/Users/solenenoize/Desktop/sleep_analysis/data/motion_summary.csv"
if (!file.exists(motion_file)) {
  stop("Motion summary file not found: ", motion_file)
}
motion_df <- read.csv(motion_file, stringsAsFactors = FALSE)
required_motion_cols <- c("pid", "night_number", "device_type")
if (!all(required_motion_cols %in% names(motion_df))) {
  stop("motion_summary.csv must contain columns: pid, night_number, device_type")
}
allowed_phone_only_keys <- motion_df %>%
  mutate(
    pid_char = trimws(as.character(pid)),
    night_number = suppressWarnings(as.integer(night_number)),
    device_type = trimws(as.character(device_type))
  ) %>%
  filter(!is.na(pid_char), !is.na(night_number), nzchar(pid_char)) %>%
  group_by(pid_char, night_number) %>%
  summarise(
    has_phone_only = any(device_type == "Phone-Only"),
    has_fitbit_phone = any(device_type == "Fitbit+Phone"),
    .groups = "drop"
  ) %>%
  filter(has_phone_only, !has_fitbit_phone) %>%
  select(pid_char, night_number)
phone_only_pid_count <- dplyr::n_distinct(allowed_phone_only_keys$pid_char)
cat("Phone-only pid/night keys kept:", nrow(allowed_phone_only_keys), "\n")
cat("Phone-only unique PID count:", phone_only_pid_count, "\n")

pick_col <- function(df, candidates, label) {
  for (nm in candidates) {
    if (nm %in% names(df)) {
      return(df[[nm]])
    }
  }
  stop("Missing required column for ", label, ". Tried: ", paste(candidates, collapse = ", "))
}

df <- data.frame(
  pid_char = trimws(as.character(pick_col(raw_df, c("pid"), "pid"))),
  overall_motion = suppressWarnings(as.numeric(pick_col(raw_df, c("overall_motion"), "overall_motion"))),
  lucid = suppressWarnings(as.numeric(pick_col(raw_df, c("lucid"), "lucid"))),
  cued_night = suppressWarnings(as.numeric(pick_col(raw_df, c("cued_night"), "cued_night"))),
  lucid_attempts_past_week = suppressWarnings(as.numeric(pick_col(raw_df, c("lucid_attempts_past_week"), "lucid_attempts_past_week"))),
  lucid_dreams_past_week = suppressWarnings(as.numeric(pick_col(raw_df, c("lucid_dreams_past_week"), "lucid_dreams_past_week"))),
  age = suppressWarnings(as.numeric(pick_col(raw_df, c("age"), "age"))),
  wake_up_duration_min = suppressWarnings(as.numeric(pick_col(raw_df, c("wake_up_duration_min", "wake_up_duration_num", "wake_up_duration"), "wake_up_duration_min"))),
  sleep_quality_num = suppressWarnings(as.numeric(pick_col(raw_df, c("sleep_quality_num", "sleep_quality"), "sleep_quality_num"))),
  wakeThresh = suppressWarnings(as.numeric(pick_col(raw_df, c("wakeThresh"), "wakeThresh"))),
  pid = as.factor(pick_col(raw_df, c("pid"), "pid")),
  stringsAsFactors = FALSE
)

model_df <- df %>%
  mutate(
    inferred_night_number = ave(
      seq_along(pid_char),
      pid_char,
      FUN = function(x) seq_along(x) - 1L
    )
  ) %>%
  semi_join(
    allowed_phone_only_keys,
    by = c("pid_char", "inferred_night_number" = "night_number")
  ) %>%
  filter(complete.cases(
    overall_motion, lucid, cued_night, lucid_attempts_past_week,
    lucid_dreams_past_week, age, wake_up_duration_min, sleep_quality_num,
    wakeThresh, pid
  )) %>%
  mutate(
    lucid = as.numeric(lucid),
    cued_night = as.numeric(cued_night),
    log_overall_motion = log(overall_motion + 1)
  )

scale_cols <- c(
  "lucid_attempts_past_week",
  "lucid_dreams_past_week",
  "age",
  "wake_up_duration_min",
  "sleep_quality_num",
  "wakeThresh"
)

if (USE_Z_SCORING) {
  model_df <- model_df %>%
    mutate(
      across(
        all_of(scale_cols),
        ~ as.numeric(scale(.x)),
        .names = "{.col}_z"
      )
    )
  predictor_suffix <- "_z"
  cat("Predictor scaling mode: z-scored\n")
} else {
  for (col in scale_cols) model_df[[paste0(col, "_raw")]] <- model_df[[col]]
  predictor_suffix <- "_raw"
  cat("Predictor scaling mode: raw (no z-scoring)\n")
}

cat("Rows used in model4:", nrow(model_df), "\n")

continuous_predictors <- paste0(scale_cols, predictor_suffix)
formula_str <- paste0(
  "log_overall_motion ~ lucid + cued_night + ",
  paste(continuous_predictors, collapse = " + "),
  " + (1 | pid)"
)
cat("Model formula:", formula_str, "\n")

motion_model <- glmmTMB(
  as.formula(formula_str),
  data = model_df,
  family = gaussian()
)

cat("\n===== Model 4 Summary (overall motion outcome) =====\n")
print(summary(motion_model))

cat("\n===== Fixed Effects =====\n")
print(fixef(motion_model)$cond)

cat("\n===== DHARMa Diagnostics =====\n")
plot_file <- file.path(results_dir, paste0(script_name, "_Rplots_", timestamp, ".pdf"))
dh_ok <- tryCatch({
  simulationOutput <- simulateResiduals(motion_model)
  pdf(plot_file)
  plot(simulationOutput)
  dev.off()
  cat("Saved DHARMa plot to:", plot_file, "\n")
  TRUE
}, error = function(e) {
  if (dev.cur() != 1) dev.off()
  cat("DHARMa diagnostics skipped due to error:", conditionMessage(e), "\n")
  FALSE
})
if (!dh_ok) {
  cat("Model summary above is still valid; diagnostics can be rerun after simplifying rank-deficient predictors.\n")
}

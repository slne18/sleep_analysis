#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readxl)
})

file_arg <- commandArgs(trailingOnly = FALSE)
file_arg <- file_arg[grepl("^--file=", file_arg)]
script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[1]) else "logistic_overall_motion.R"
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
  "../data/merge_data_cleaned.xlsx",
  "data/merge_data_cleaned.xlsx",
  "merge_data_cleaned.xlsx",
  file.path(script_dir, "../data/merge_data_cleaned.xlsx"),
  file.path(script_dir, "data/merge_data_cleaned.xlsx")
)
existing_candidates <- input_candidates[file.exists(input_candidates)]
if (length(existing_candidates) == 0) {
  stop("Could not find input file. Tried: ", paste(input_candidates, collapse = ", "))
}
input_file <- existing_candidates[1]
cat("Using input file:", input_file, "\n")

raw_df <- read_excel(input_file)

motion_candidates <- c(
  "../data/motion_summary.csv",
  "data/motion_summary.csv",
  "motion_summary.csv",
  file.path(script_dir, "../data/motion_summary.csv"),
  file.path(script_dir, "data/motion_summary.csv")
)
motion_existing <- motion_candidates[file.exists(motion_candidates)]
if (length(motion_existing) == 0) {
  stop("Could not find motion_summary.csv. Tried: ", paste(motion_candidates, collapse = ", "))
}
motion_file <- motion_existing[1]
cat("Using motion file:", motion_file, "\n")
motion_df <- read.csv(motion_file, stringsAsFactors = FALSE)
if (!all(c("pid", "device_type") %in% names(motion_df))) {
  stop("motion_summary.csv must contain columns: pid, device_type")
}

motion_df$pid <- trimws(as.character(motion_df$pid))
motion_df$device_type <- trimws(as.character(motion_df$device_type))
fitbit_pids <- unique(motion_df$pid[motion_df$device_type == "Fitbit+Phone"])
phone_only_pids <- setdiff(unique(motion_df$pid[motion_df$device_type == "Phone-Only"]), fitbit_pids)
cat("Phone-only participant count:", length(phone_only_pids), "\n")

pick_col <- function(df, candidates, label) {
  for (nm in candidates) {
    if (nm %in% names(df)) {
      return(df[[nm]])
    }
  }
  stop("Missing required column for ", label, ". Tried: ", paste(candidates, collapse = ", "))
}

df <- data.frame(
  lucid = suppressWarnings(as.numeric(pick_col(raw_df, c("lucid"), "lucid"))),
  overall_motion = suppressWarnings(as.numeric(pick_col(raw_df, c("overall_motion"), "overall_motion"))),
  pid = trimws(as.character(pick_col(raw_df, c("pid"), "pid"))),
  stringsAsFactors = FALSE
)

model_df <- df[
  complete.cases(df) &
    df$lucid %in% c(0, 1) &
    df$pid %in% phone_only_pids,
  ,
  drop = FALSE
]

if (nrow(model_df) == 0) {
  stop("No complete rows available for logistic regression.")
}

fit <- glm(lucid ~ log(overall_motion + 1), data = model_df, family = binomial(link = "logit"))

cat("\n===== Logistic Regression Summary =====\n")
print(summary(fit))

cat("\n===== Odds Ratios (95% CI) =====\n")
beta <- coef(fit)
ci <- suppressMessages(confint.default(fit))
or_table <- data.frame(
  term = names(beta),
  odds_ratio = exp(beta),
  ci_lower_95 = exp(ci[, 1]),
  ci_upper_95 = exp(ci[, 2]),
  row.names = NULL
)
print(or_table)

cat("\nN used:", nrow(model_df), "\n")

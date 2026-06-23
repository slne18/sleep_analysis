#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readxl)
})

file_arg <- commandArgs(trailingOnly = FALSE)
file_arg <- file_arg[grepl("^--file=", file_arg)]
script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[1]) else "logistic_fitbit_overall_motion.R"
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
  fitbit_overall_motion = suppressWarnings(as.numeric(pick_col(raw_df, c("fitbit_overall_motion"), "fitbit_overall_motion"))),
  stringsAsFactors = FALSE
)

model_df <- df[complete.cases(df) & df$lucid %in% c(0, 1), , drop = FALSE]
if (nrow(model_df) == 0) {
  stop("No complete rows available for logistic regression.")
}

fit <- glm(lucid ~ fitbit_overall_motion, data = model_df, family = binomial(link = "logit"))

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

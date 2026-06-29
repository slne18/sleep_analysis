#!/usr/bin/env Rscript

script_dir <- local({
  file_arg <- commandArgs(trailingOnly = FALSE)
  file_arg <- file_arg[grepl("^--file=", file_arg)]
  script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[1]) else "model1.R"
  normalizePath(dirname(script_path), winslash = "/", mustWork = FALSE)
})
source(file.path(script_dir, "..", "glmm_common.R"))

ctx <- init_glmm_script("model1.R")
input_file <- resolve_input_file(ctx$script_dir)
log_cat("Using input file:", input_file, "\n")

df <- read_cleaned_data(input_file)
df <- prepare_lucid_outcome(df)

continuous_cols <- c(
  "age",
  "sleep_quality",
  "lucid_dreams_freq_pw",
  "lucid_attempts_freq_pw",
  "time_asleep",
  "wake_up_duration"
)

model_df <- add_transformed_predictors(df, continuous_cols)
model_df$gender <- factor(model_df$gender)
if (nlevels(model_df$gender) > 1) {
  model_df$gender <- relevel(model_df$gender, ref = names(sort(table(model_df$gender), decreasing = TRUE))[1])
}

predictor_cols <- c(log_predictor_suffix(continuous_cols), "gender")
model_df <- model_df[complete.cases(model_df[, c("lucid", "pid", predictor_cols)]), , drop = FALSE]

log_cat("Rows before complete-case filtering:", nrow(df), "\n")
print_predictor_preprocessing(continuous_cols)

formula <- as.formula(
  paste(
    "lucid ~",
    paste(c(log_predictor_suffix(continuous_cols), "gender"), collapse = " + "),
    "+ (1 | pid)"
  )
)

run_lucid_glmm(
  formula = formula,
  data = model_df,
  model_label = "model1_participant_factors",
  script_name = ctx$script_name,
  results_dir = ctx$results_dir,
  timestamp = ctx$timestamp,
  summary_title = "Model 1 Summary (glmmTMB binomial)"
)

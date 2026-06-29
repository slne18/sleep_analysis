#!/usr/bin/env Rscript

script_dir <- local({
  file_arg <- commandArgs(trailingOnly = FALSE)
  file_arg <- file_arg[grepl("^--file=", file_arg)]
  script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[1]) else "model2_interaction.R"
  normalizePath(dirname(script_path), winslash = "/", mustWork = FALSE)
})
source(file.path(script_dir, "..", "glmm_common.R"))

ctx <- init_glmm_script("model2_interaction.R")
input_file <- resolve_input_file(ctx$script_dir)
log_cat("Using input file:", input_file, "\n")

df <- read_cleaned_data(input_file)
df <- prepare_lucid_outcome(df)

continuous_cols <- c("wakeThresh", "highestVol")
model_df <- add_transformed_predictors(df, continuous_cols)

all_predictor_cols <- c(
  log_predictor_suffix(continuous_cols),
  "cue_notice",
  CUEING_TERM,
  "had_arousal"
)
model_df <- model_df[complete.cases(model_df[, c("lucid", "pid", all_predictor_cols)]), , drop = FALSE]

model_df$cue_notice <- factorize_predictor(model_df$cue_notice, "cue_notice")
model_df$had_arousal <- factorize_predictor(model_df$had_arousal, "arousal")
model_df <- prepare_cueing_factor(model_df)

factor_cols <- c("cue_notice", "had_arousal")
factor_check <- drop_single_level_factors(model_df, factor_cols)
if (length(factor_check$dropped) > 0) {
  log_cat(
    "Dropped categorical predictors with only one level after filtering:",
    paste(factor_check$dropped, collapse = ", "),
    "\n"
  )
}

cueing_check <- drop_single_level_factors(model_df, CUEING_TERM)
if (length(cueing_check$dropped) > 0) {
  stop(CUEING_TERM, " must have two levels for interaction models.")
}

predictor_terms <- c(log_predictor_suffix(continuous_cols), factor_check$kept)

log_cat("Rows before complete-case filtering:", nrow(df), "\n")
log_cat("Rows used in model2 interaction:", nrow(model_df), "\n")
print_predictor_preprocessing(continuous_cols)
log_cat("Categorical predictors interacting with cueing:", paste(factor_check$kept, collapse = ", "), "\n")
log_cueing_interaction_note(predictor_terms)

formula <- build_cueing_interaction_formula(predictor_terms)

run_lucid_glmm(
  formula = formula,
  data = model_df,
  model_label = "model2_device_stimulation_cueing_interaction",
  script_name = ctx$script_name,
  results_dir = ctx$results_dir,
  timestamp = ctx$timestamp,
  summary_title = "Model 2 Cueing Interaction Summary (glmmTMB binomial)"
)

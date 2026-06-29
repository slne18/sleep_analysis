#!/usr/bin/env Rscript

script_dir <- local({
  file_arg <- commandArgs(trailingOnly = FALSE)
  file_arg <- file_arg[grepl("^--file=", file_arg)]
  script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[1]) else "phone_overall_interaction.R"
  normalizePath(dirname(script_path), winslash = "/", mustWork = FALSE)
})
source(file.path(script_dir, "..", "glmm_common.R"))

ctx <- init_glmm_script("phone_overall_interaction.R")
input_file <- resolve_input_file(ctx$script_dir)
log_cat("Using input file:", input_file, "\n")

df <- read_cleaned_data(input_file)
df <- prepare_lucid_outcome(df)
df <- df[!is.na(df$overall_motion), , drop = FALSE]

continuous_cols <- c("overall_motion", "high_freq", "low_freq")
model_df <- add_transformed_predictors(df, continuous_cols)
model_df <- prepare_cueing_factor(model_df)

predictor_terms <- log_predictor_suffix(continuous_cols)
predictor_cols <- c(predictor_terms, CUEING_TERM)
model_df <- model_df[complete.cases(model_df[, c("lucid", "pid", predictor_cols)]), , drop = FALSE]

log_cat("Subgroup: phone-measured nights (non-missing overall_motion)\n")
log_cat("Rows before complete-case filtering:", nrow(df), "\n")
log_cat("Rows used in phone overall interaction:", nrow(model_df), "\n")
print_predictor_preprocessing(continuous_cols)
log_cueing_interaction_note(predictor_terms)

formula <- build_cueing_interaction_formula(predictor_terms)

run_lucid_glmm(
  formula = formula,
  data = model_df,
  model_label = "model3_phone_overall_motion_cueing_interaction",
  script_name = ctx$script_name,
  results_dir = ctx$results_dir,
  timestamp = ctx$timestamp,
  summary_title = "Model 3 Phone Overall Cueing Interaction Summary (glmmTMB binomial)"
)

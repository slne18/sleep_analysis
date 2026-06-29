#!/usr/bin/env Rscript

script_dir <- local({
  file_arg <- commandArgs(trailingOnly = FALSE)
  file_arg <- file_arg[grepl("^--file=", file_arg)]
  script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[1]) else "model1_interaction.R"
  normalizePath(dirname(script_path), winslash = "/", mustWork = FALSE)
})
source(file.path(script_dir, "..", "glmm_common.R"))

ctx <- init_glmm_script("model1_interaction.R")
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
model_df <- prepare_cueing_factor(model_df)

predictor_terms <- c(log_predictor_suffix(continuous_cols), "gender")
predictor_cols <- c(predictor_terms, CUEING_TERM)
model_df <- model_df[complete.cases(model_df[, c("lucid", "pid", predictor_cols)]), , drop = FALSE]

log_cat("Rows before complete-case filtering:", nrow(df), "\n")
log_cat("Rows used in model1 interaction:", nrow(model_df), "\n")
print_predictor_preprocessing(continuous_cols)
log_cueing_interaction_note(predictor_terms)

formula <- build_cueing_interaction_formula(predictor_terms)

run_lucid_glmm(
  formula = formula,
  data = model_df,
  model_label = "model1_participant_factors_cueing_interaction",
  script_name = ctx$script_name,
  results_dir = ctx$results_dir,
  timestamp = ctx$timestamp,
  summary_title = "Model 1 Cueing Interaction Summary (glmmTMB binomial)"
)

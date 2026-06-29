#!/usr/bin/env Rscript

script_dir <- local({
  file_arg <- commandArgs(trailingOnly = FALSE)
  file_arg <- file_arg[grepl("^--file=", file_arg)]
  script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[1]) else "phone_stim_cued.R"
  normalizePath(dirname(script_path), winslash = "/", mustWork = FALSE)
})
source(file.path(script_dir, "..", "glmm_common.R"))

ctx <- init_glmm_script("phone_stim_cued.R")
input_file <- resolve_input_file(ctx$script_dir)
log_cat("Using input file:", input_file, "\n")

df <- read_cleaned_data(input_file)
df <- prepare_lucid_outcome(df)
df <- df[!is.na(df$overall_motion), , drop = FALSE]
df <- df[df$cued_night == 1, , drop = FALSE]

continuous_cols <- c("stimulation_motion", "cue_delta_var")
model_df <- add_transformed_predictors(df, continuous_cols)
predictor_cols <- log_predictor_suffix(continuous_cols)
model_df <- model_df[complete.cases(model_df[, c("lucid", "pid", predictor_cols)]), , drop = FALSE]

log_cat("Subgroup: phone-measured cued nights\n")
log_cat("Rows before complete-case filtering:", nrow(df), "\n")
print_predictor_preprocessing(continuous_cols)

formula <- as.formula(
  paste(
    "lucid ~",
    paste(predictor_cols, collapse = " + "),
    "+ (1 | pid)"
  )
)

run_lucid_glmm(
  formula = formula,
  data = model_df,
  model_label = "model3_phone_stimulation_cued",
  script_name = ctx$script_name,
  results_dir = ctx$results_dir,
  timestamp = ctx$timestamp,
  summary_title = "Model 3 Phone Stim Cued Summary (glmmTMB binomial)"
)

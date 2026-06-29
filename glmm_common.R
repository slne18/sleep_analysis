.GLMM_LOG <- new.env(parent = emptyenv())

log_cat <- function(...) {
  text <- paste0(..., collapse = "")
  if (!grepl("\n$", text)) {
    text <- paste0(text, "\n")
  }
  cat(text)
  if (exists("log_file", envir = .GLMM_LOG, inherits = FALSE)) {
    cat(text, file = get("log_file", envir = .GLMM_LOG), append = TRUE)
  }
}

log_print <- function(x) {
  log_cat(paste(capture.output(print(x)), collapse = "\n"))
}

init_glmm_script <- function(default_script_name = "glmm_model.R") {
  file_arg <- commandArgs(trailingOnly = FALSE)
  file_arg <- file_arg[grepl("^--file=", file_arg)]
  script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[1]) else default_script_name
  script_dir <- normalizePath(dirname(script_path), winslash = "/", mustWork = FALSE)
  script_name <- sub("\\.R$", "", basename(script_path))
  results_dir <- file.path(script_dir, "results")
  dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  log_file <- file.path(results_dir, paste0(script_name, "_", timestamp, ".log"))
  assign("log_file", log_file, envir = .GLMM_LOG)

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
    library(glmmTMB)
    library(DHARMa)
  })

  log_cat("Saving output to:", log_file, "\n")
  list(
    script_dir = script_dir,
    script_name = script_name,
    results_dir = results_dir,
    timestamp = timestamp,
    log_file = log_file
  )
}

resolve_input_file <- function(script_dir) {
  input_candidates <- c(
    file.path(script_dir, "../data/merge_data_cleaned.xlsx"),
    file.path(script_dir, "data/merge_data_cleaned.xlsx"),
    "data/merge_data_cleaned.xlsx",
    "merge_data_cleaned.xlsx"
  )
  existing_candidates <- input_candidates[file.exists(input_candidates)]
  if (length(existing_candidates) == 0) {
    stop("Could not find merge_data_cleaned.xlsx. Tried: ", paste(input_candidates, collapse = ", "))
  }
  existing_candidates[[1]]
}

pick_col <- function(df, candidates, label) {
  for (nm in candidates) {
    if (nm %in% names(df)) {
      return(df[[nm]])
    }
  }
  stop("Missing required column for ", label, ". Tried: ", paste(candidates, collapse = ", "))
}

read_weekly_lucid_freq <- function(df, freq_col, week_col, label) {
  if (freq_col %in% names(df)) {
    return(suppressWarnings(as.numeric(df[[freq_col]])))
  }
  if (week_col %in% names(df)) {
    return(suppressWarnings(as.numeric(df[[week_col]])) / 7)
  }
  stop("Missing required column for ", label, ". Tried: ", freq_col, ", ", week_col)
}

read_cleaned_data <- function(input_file) {
  raw_df <- read_excel(input_file)
  data.frame(
    lucid = suppressWarnings(as.numeric(pick_col(raw_df, c("lucid"), "lucid"))),
    pid = pick_col(raw_df, c("pid"), "pid"),
    age = suppressWarnings(as.numeric(pick_col(raw_df, c("age"), "age"))),
    gender = pick_col(raw_df, c("gender"), "gender"),
    sleep_quality = suppressWarnings(as.numeric(pick_col(raw_df, c("sleep_quality", "sleep_quality_num"), "sleep_quality"))),
    lucid_dreams_freq_pw = read_weekly_lucid_freq(
      raw_df, "lucid_dreams_freq_pw", "lucid_dreams_past_week", "lucid_dreams_freq_pw"
    ),
    lucid_attempts_freq_pw = read_weekly_lucid_freq(
      raw_df, "lucid_attempts_freq_pw", "lucid_attempts_past_week", "lucid_attempts_freq_pw"
    ),
    time_asleep = suppressWarnings(as.numeric(pick_col(raw_df, c("time_asleep"), "time_asleep"))),
    wake_up_duration = suppressWarnings(as.numeric(pick_col(raw_df, c("wake_up_duration", "wake_up_duration_min"), "wake_up_duration"))),
    cue_notice = suppressWarnings(as.numeric(pick_col(raw_df, c("cue_notice"), "cue_notice"))),
    wakeThresh = suppressWarnings(as.numeric(pick_col(raw_df, c("wakeThresh"), "wakeThresh"))),
    highestVol = suppressWarnings(as.numeric(pick_col(raw_df, c("highestVol"), "highestVol"))),
    cued_night = suppressWarnings(as.numeric(pick_col(raw_df, c("cued_night"), "cued_night"))),
    had_arousal = suppressWarnings(as.numeric(pick_col(raw_df, c("had_arousal"), "had_arousal"))),
    arousal_rate = suppressWarnings(as.numeric(pick_col(raw_df, c("arousal_rate"), "arousal_rate"))),
    overall_motion = suppressWarnings(as.numeric(pick_col(raw_df, c("overall_motion"), "overall_motion"))),
    stimulation_motion = suppressWarnings(as.numeric(pick_col(raw_df, c("stimulation_motion"), "stimulation_motion"))),
    cue_delta_var = suppressWarnings(as.numeric(pick_col(raw_df, c("cue_delta_var"), "cue_delta_var"))),
    high_freq = suppressWarnings(as.numeric(pick_col(raw_df, c("high_freq"), "high_freq"))),
    low_freq = suppressWarnings(as.numeric(pick_col(raw_df, c("low_freq"), "low_freq"))),
    fitbit_overall_motion = suppressWarnings(as.numeric(pick_col(raw_df, c("fitbit_overall_motion"), "fitbit_overall_motion"))),
    fitbit_stim_motion = suppressWarnings(as.numeric(pick_col(raw_df, c("fitbit_stim_motion"), "fitbit_stim_motion"))),
    stringsAsFactors = FALSE
  )
}

transform_log_z <- function(x) {
  x_num <- suppressWarnings(as.numeric(x))
  if (all(is.na(x_num))) {
    return(rep(NA_real_, length(x_num)))
  }
  logged <- log(x_num + 1)
  as.numeric(scale(logged))
}

# Consistent rule for all continuous predictors across model1/2/3:
# 1) log(x + 1) to handle zeros and skew
# 2) z-score the logged values (mean 0, SD 1)
PREDICTOR_TRANSFORM_SUFFIX <- "_logz"

log_predictor_suffix <- function(col_names) {
  paste0(col_names, PREDICTOR_TRANSFORM_SUFFIX)
}

print_predictor_preprocessing <- function(continuous_cols) {
  log_cat("Predictor preprocessing (all continuous predictors):\n")
  log_cat("  1) log(x + 1)\n")
  log_cat("  2) z-score\n")
  log_cat("  Transformed columns:", paste(log_predictor_suffix(continuous_cols), collapse = ", "), "\n")
}

add_transformed_predictors <- function(df, continuous_cols) {
  out <- df
  for (col in continuous_cols) {
    if (!col %in% names(out)) {
      stop("Missing continuous predictor column: ", col)
    }
    out[[paste0(col, PREDICTOR_TRANSFORM_SUFFIX)]] <- transform_log_z(out[[col]])
  }
  out
}

prepare_lucid_outcome <- function(df) {
  out <- df
  out$lucid <- suppressWarnings(as.numeric(out$lucid))
  out <- out[out$lucid %in% c(0, 1), , drop = FALSE]
  out$pid <- factor(out$pid)
  out
}

factorize_predictor <- function(x, label) {
  vals <- suppressWarnings(as.numeric(x))
  factor(vals, levels = c(0, 1), labels = c("0", "1"))
}

drop_single_level_factors <- function(data, factor_cols) {
  kept <- character()
  dropped <- character()
  for (col in factor_cols) {
    if (!col %in% names(data)) {
      next
    }
    n_levels <- nlevels(droplevels(data[[col]]))
    if (n_levels < 2) {
      dropped <- c(dropped, col)
    } else {
      kept <- c(kept, col)
    }
  }
  list(kept = kept, dropped = dropped)
}

CUEING_TERM <- "cued_night"

prepare_cueing_factor <- function(df) {
  out <- df
  out[[CUEING_TERM]] <- factorize_predictor(out[[CUEING_TERM]], CUEING_TERM)
  out
}

build_cueing_interaction_formula <- function(predictor_terms, random_effect = "(1 | pid)") {
  if (length(predictor_terms) == 0) {
    stop("No predictor terms supplied for cueing interaction formula.")
  }
  term_pairs <- paste0(predictor_terms, " + ", predictor_terms, ":", CUEING_TERM)
  rhs <- paste(term_pairs, collapse = " + ")
  as.formula(paste("lucid ~", rhs, "+", random_effect))
}

log_cueing_interaction_note <- function(predictor_terms) {
  log_cat("Cueing interaction model: predictor main effects + predictor:cued_night interactions\n")
  log_cat(
    "Terms per predictor: predictor + predictor:cued_night\n",
    "Predictors:",
    paste(predictor_terms, collapse = ", "),
    "\n"
  )
}

extract_glmm_results <- function(model, model_label) {
  coef_tbl <- summary(model)$coefficients$cond
  if (is.null(coef_tbl) || nrow(coef_tbl) == 0) {
    stop("No fixed-effect coefficients found for model: ", model_label)
  }

  p_col <- "Pr(>|z|)"
  if (!(p_col %in% colnames(coef_tbl))) {
    stop("Expected p-value column '", p_col, "' not found for model: ", model_label)
  }

  terms <- rownames(coef_tbl)
  p_raw <- coef_tbl[, p_col]
  p_fdr <- p.adjust(p_raw, method = "fdr")

  out <- data.frame(
    model = model_label,
    term = terms,
    estimate = coef_tbl[, "Estimate"],
    std_error = coef_tbl[, "Std. Error"],
    z_value = coef_tbl[, "z value"],
    odds_ratio = exp(coef_tbl[, "Estimate"]),
    p_raw = p_raw,
    p_fdr = p_fdr,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  out$significant_raw_0.05 <- ifelse(!is.na(out$p_raw) & out$p_raw < 0.05, TRUE, FALSE)
  out$significant_fdr_0.05 <- ifelse(!is.na(out$p_fdr) & out$p_fdr < 0.05, TRUE, FALSE)
  out
}

fit_lucid_glmm <- function(formula, data, model_label, summary_title = NULL) {
  if (is.null(summary_title)) {
    summary_title <- paste0(model_label, " Summary (glmmTMB binomial)")
  }

  log_cat("Model formula:", deparse(formula), "\n")
  log_cat("Rows used:", nrow(data), "\n")
  log_cat("Participants:", length(unique(data$pid)), "\n")

  model <- glmmTMB(
    formula = formula,
    data = data,
    family = binomial(link = "logit")
  )

  log_cat("\n=====", summary_title, "=====\n")
  log_print(summary(model))

  log_cat("\n===== Odds Ratios =====\n")
  log_print(exp(fixef(model)$cond))

  results <- extract_glmm_results(model, model_label)

  log_cat("\n===== DHARMa Diagnostics =====\n")
  simulation_output <- simulateResiduals(model)
  invisible(list(model = model, results = results, simulation_output = simulation_output))
}

save_dharma_plot <- function(simulation_output, results_dir, script_name, timestamp, model_label) {
  safe_label <- gsub("[^A-Za-z0-9_]+", "_", model_label)
  plot_file <- file.path(results_dir, paste0(script_name, "_", safe_label, "_DHARMa_", timestamp, ".pdf"))
  pdf(plot_file)
  plot(simulation_output)
  dev.off()
  log_cat("Saved DHARMa plot to:", plot_file, "\n")
  invisible(plot_file)
}

save_glmm_outputs <- function(model_obj, results_dir, script_name, timestamp, model_label) {
  safe_label <- gsub("[^A-Za-z0-9_]+", "_", model_label)
  results_file <- file.path(
    results_dir,
    paste0(script_name, "_", safe_label, "_results_", timestamp, ".csv")
  )
  write.csv(model_obj$results, results_file, row.names = FALSE)
  log_cat("Saved results (including raw and FDR p-values) to:", results_file, "\n")
  invisible(results_file)
}

run_lucid_glmm <- function(
  formula,
  data,
  model_label,
  script_name,
  results_dir,
  timestamp,
  summary_title = NULL
) {
  model_obj <- fit_lucid_glmm(
    formula = formula,
    data = data,
    model_label = model_label,
    summary_title = summary_title
  )
  save_glmm_outputs(model_obj, results_dir, script_name, timestamp, model_label)
  save_dharma_plot(
    model_obj$simulation_output,
    results_dir,
    script_name,
    timestamp,
    model_label
  )
  invisible(model_obj)
}

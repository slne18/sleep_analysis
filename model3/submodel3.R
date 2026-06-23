#!/usr/bin/env Rscript

file_arg <- commandArgs(trailingOnly = FALSE)
file_arg <- file_arg[grepl("^--file=", file_arg)]
script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[1]) else "submodel1.R"
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
motion_raw <- read.csv(motion_file, stringsAsFactors = FALSE)
required_motion_cols <- c("pid", "night_number", "device_type")
if (!all(required_motion_cols %in% names(motion_raw))) {
  stop("motion_summary.csv must contain columns: pid, night_number, device_type")
}

pick_col <- function(df, candidates, label) {
  for (nm in candidates) {
    if (nm %in% names(df)) {
      return(df[[nm]])
    }
  }
  stop("Missing required column for ", label, ". Tried: ", paste(candidates, collapse = ", "))
}

base_df <- data.frame(
  lucid = suppressWarnings(as.numeric(pick_col(raw_df, c("lucid"), "lucid"))),
  totalCues = suppressWarnings(as.numeric(pick_col(raw_df, c("totalCues"), "totalCues"))),
  high_freq = suppressWarnings(as.numeric(pick_col(raw_df, c("high_freq"), "high_freq"))),
  low_freq = suppressWarnings(as.numeric(pick_col(raw_df, c("low_freq"), "low_freq"))),
  cue_delta_var = suppressWarnings(as.numeric(pick_col(raw_df, c("cue_delta_var"), "cue_delta_var"))),
  lucid_attempts_past_week = suppressWarnings(as.numeric(pick_col(raw_df, c("lucid_attempts_past_week"), "lucid_attempts_past_week"))),
  lucid_dreams_past_week = suppressWarnings(as.numeric(pick_col(raw_df, c("lucid_dreams_past_week"), "lucid_dreams_past_week"))),
  age = suppressWarnings(as.numeric(pick_col(raw_df, c("age"), "age"))),
  wake_up_duration_min = suppressWarnings(as.numeric(pick_col(raw_df, c("wake_up_duration_min", "wake_up_duration_num", "wake_up_duration"), "wake_up_duration_min"))),
  sleep_quality_num = suppressWarnings(as.numeric(pick_col(raw_df, c("sleep_quality_num", "sleep_quality"), "sleep_quality_num"))),
  wakeThresh = suppressWarnings(as.numeric(pick_col(raw_df, c("wakeThresh"), "wakeThresh"))),
  pid = as.factor(pick_col(raw_df, c("pid"), "pid")),
  stringsAsFactors = FALSE
) 

allowed_phone_only_keys <- motion_raw %>%
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

model_df <- base_df %>%
  mutate(
    pid_char = trimws(as.character(pid)),
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
  filter(
    lucid %in% c(0, 1),
    complete.cases(
      lucid, totalCues, high_freq, low_freq, cue_delta_var, lucid_attempts_past_week, lucid_dreams_past_week,
      age, wake_up_duration_min, sleep_quality_num, wakeThresh, pid
    )
  )
cat("Phone-only pid/night keys kept:", nrow(allowed_phone_only_keys), "\n")
cat("Phone-only unique PID count:", phone_only_pid_count, "\n")

# Log version on cued nights only.
cued_df <- model_df %>%
  filter(totalCues > 0)

scale_cols <- c("age", "wake_up_duration_min", "sleep_quality_num", "wakeThresh")

if (USE_Z_SCORING) {
  cued_df <- cued_df %>%
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
  for (col in scale_cols) cued_df[[paste0(col, "_raw")]] <- cued_df[[col]]
  predictor_suffix <- "_raw"
  cat("Predictor scaling mode: raw (no z-scoring)\n")
}

cat("Rows in model_df before cued subset:", nrow(model_df), "\n")
cat("Rows used in submodel1 (totalCues > 0):", nrow(cued_df), "\n")

model_data_file <- file.path(results_dir, paste0(script_name, "_model_data_", timestamp, ".csv"))
write.csv(cued_df, model_data_file, row.names = FALSE)
cat("Saved model input data to:", model_data_file, "\n")

continuous_predictors <- paste0(scale_cols, predictor_suffix)
formula_str <- paste0(
  "lucid ~ log(totalCues + 1) + ",
  "log(lucid_attempts_past_week + 1) + log(lucid_dreams_past_week + 1) + ",
  "log(high_freq + 1) + log(low_freq + 1) + ",
  "log(cue_delta_var + 1) + ",
  paste(continuous_predictors, collapse = " + "),
  " + (1 | pid)"
)
cat("Model formula:", formula_str, "\n")

submodel1 <- glmmTMB(
  as.formula(formula_str),
  data = cued_df,
  family = binomial()
)

cat("\n===== Submodel3 Summary (log version, glmmTMB binomial) =====\n")
print(summary(submodel1))

cat("\n===== Odds Ratios =====\n")
print(exp(fixef(submodel1)$cond))

plot_significant_associations <- function(model, data, outcome, script_name, timestamp, results_dir) {
  coef_tbl <- summary(model)$coefficients$cond
  if (is.null(coef_tbl) || nrow(coef_tbl) == 0) {
    cat("No conditional fixed effects found for plotting.\n")
    return(invisible(NULL))
  }
  p_col <- "Pr(>|z|)"
  if (!(p_col %in% colnames(coef_tbl))) {
    cat("P-value column not found in model summary; skipping significant-association plots.\n")
    return(invisible(NULL))
  }
  sig_terms <- rownames(coef_tbl)[!is.na(coef_tbl[, p_col]) & coef_tbl[, p_col] < 0.05]
  sig_terms <- setdiff(sig_terms, "(Intercept)")
  if (length(sig_terms) == 0) {
    cat("No significant fixed effects (p < 0.05); no association plots generated.\n")
    return(invisible(NULL))
  }

  cat("\n===== Significant Association Plots =====\n")
  for (term in sig_terms) {
    x_numeric <- tryCatch(with(data, eval(parse(text = term))), error = function(e) NULL)
    if (!is.null(x_numeric) && is.numeric(x_numeric)) {
      plot_df <- data.frame(x = x_numeric, y = data[[outcome]])
      plot_df <- plot_df[is.finite(plot_df$x) & is.finite(plot_df$y), , drop = FALSE]
      if (nrow(plot_df) >= 10) {
        out_file <- file.path(results_dir, paste0(script_name, "_sig_scatter_", gsub("[^A-Za-z0-9_]+", "_", term), "_", timestamp, ".png"))
        png(out_file, width = 900, height = 700)
        plot(
          plot_df$x, jitter(plot_df$y, amount = 0.04),
          pch = 16, col = rgb(0, 0, 0, 0.35),
          xlab = term, ylab = paste0(outcome, " (jittered 0/1)"),
          main = paste("Significant association:", term)
        )
        glm_fit <- tryCatch(glm(y ~ x, family = binomial(), data = plot_df), error = function(e) NULL)
        if (!is.null(glm_fit)) {
          x_grid <- seq(min(plot_df$x), max(plot_df$x), length.out = 200)
          y_hat <- predict(glm_fit, newdata = data.frame(x = x_grid), type = "response")
          lines(x_grid, y_hat, col = "red", lwd = 2)
        }
        dev.off()
        cat("Saved:", out_file, "\n")
      } else {
        cat("Skipped term (not enough plottable rows):", term, "\n")
      }
      next
    }

    base_factor <- sub("([A-Za-z0-9_\\.]+?)[0-9].*$", "\\1", term)
    if (base_factor %in% names(data) && is.factor(data[[base_factor]])) {
      plot_df <- data.frame(group = data[[base_factor]], y = data[[outcome]])
      plot_df <- plot_df[!is.na(plot_df$group) & is.finite(plot_df$y), , drop = FALSE]
      if (nrow(plot_df) >= 10) {
        out_file <- file.path(results_dir, paste0(script_name, "_sig_factor_", gsub("[^A-Za-z0-9_]+", "_", term), "_", timestamp, ".png"))
        png(out_file, width = 900, height = 700)
        boxplot(y ~ group, data = plot_df, xlab = base_factor, ylab = outcome, main = paste("Significant association:", term))
        stripchart(y ~ group, data = plot_df, vertical = TRUE, method = "jitter", pch = 16, col = rgb(0, 0, 0, 0.35), add = TRUE)
        dev.off()
        cat("Saved:", out_file, "\n")
      } else {
        cat("Skipped factor term (not enough plottable rows):", term, "\n")
      }
    } else {
      cat("Skipped term (unable to evaluate for plotting):", term, "\n")
    }
  }
}

plot_significant_associations(
  model = submodel1,
  data = cued_df,
  outcome = "lucid",
  script_name = script_name,
  timestamp = timestamp,
  results_dir = results_dir
)

cat("\n===== DHARMa Diagnostics =====\n")
simulationOutput <- simulateResiduals(submodel1)
plot_file <- file.path(results_dir, paste0(script_name, "_Rplots_", timestamp, ".pdf"))
pdf(plot_file)
plot(simulationOutput)
dev.off()
cat("Saved DHARMa plot to:", plot_file, "\n")

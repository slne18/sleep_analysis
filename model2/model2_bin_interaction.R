#!/usr/bin/env Rscript

file_arg <- commandArgs(trailingOnly = FALSE)
file_arg <- file_arg[grepl("^--file=", file_arg)]
script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[1]) else "model2_bin_interaction.R"
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
USE_Z_SCORING <- FALSE

input_file <- "/Users/solenenoize/Desktop/sleep_analysis/data/merge_data_cleaned.xlsx"
if (!file.exists(input_file)) {
  stop("Input file not found: ", input_file)
}

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
  lucid_dreams_past_week = suppressWarnings(as.numeric(pick_col(raw_df, c("lucid_dreams_past_week"), "lucid_dreams_past_week"))),
  overall_motion = suppressWarnings(as.numeric(pick_col(raw_df, c("overall_motion"), "overall_motion"))),
  stimulation_motion = suppressWarnings(as.numeric(pick_col(raw_df, c("stimulation_motion"), "stimulation_motion"))),
  had_arousal = suppressWarnings(as.numeric(pick_col(raw_df, c("had_arousal"), "had_arousal"))),
  cued_night = suppressWarnings(as.numeric(pick_col(raw_df, c("cued_night"), "cued_night"))),
  lucid_attempts_past_week = suppressWarnings(as.numeric(pick_col(raw_df, c("lucid_attempts_past_week"), "lucid_attempts_past_week"))),
  age = suppressWarnings(as.numeric(pick_col(raw_df, c("age"), "age"))),
  wake_up_duration_min = suppressWarnings(as.numeric(pick_col(raw_df, c("wake_up_duration_min", "wake_up_duration_num", "wake_up_duration"), "wake_up_duration_min"))),
  sleep_quality_num = suppressWarnings(as.numeric(pick_col(raw_df, c("sleep_quality_num", "sleep_quality"), "sleep_quality_num"))),
  wakeThresh = suppressWarnings(as.numeric(pick_col(raw_df, c("wakeThresh"), "wakeThresh"))),
  pid = as.factor(pick_col(raw_df, c("pid"), "pid")),
  stringsAsFactors = FALSE
)

df <- df %>%
  filter(lucid %in% c(0, 1))

model_df <- df %>%
  filter(complete.cases(
    lucid,
    lucid_dreams_past_week,
    overall_motion,
    stimulation_motion,
    had_arousal,
    cued_night,
    lucid_attempts_past_week,
    age,
    wake_up_duration_min,
    sleep_quality_num,
    wakeThresh,
    pid
  )) %>%
  mutate(
    had_arousal = as.factor(had_arousal),
    cued_night = as.factor(cued_night)
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

cat("Rows before complete-case filtering:", nrow(df), "\n")
cat("Rows used in model2_bin_interaction:", nrow(model_df), "\n")

continuous_predictors <- paste0(
  c(
    "lucid_attempts_past_week",
    "lucid_dreams_past_week",
    "age",
    "wake_up_duration_min",
    "sleep_quality_num",
    "wakeThresh"
  ),
  predictor_suffix
)
formula_str <- paste0(
  "lucid ~ cued_night * (had_arousal + ",
  paste(continuous_predictors, collapse = " + "),
  ") + ",
  "log(overall_motion + 1) + ",
  "cued_night:log(overall_motion + 1) + ",
  "(1 | pid)"
)
cat("Model formula:", formula_str, "\n")

logistic_model <- glmmTMB(
  as.formula(formula_str),
  data = model_df,
  family = binomial()
)

cat("\n===== Model 2 Bin + Interaction Summary (glmmTMB binomial) =====\n")
print(summary(logistic_model))

cat("\n===== Odds Ratios =====\n")
print(exp(fixef(logistic_model)$cond))

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
    if (grepl(":", term, fixed = TRUE)) {
      cat("Skipped interaction term for auto-plotting:", term, "\n")
      next
    }
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
  model = logistic_model,
  data = model_df,
  outcome = "lucid",
  script_name = script_name,
  timestamp = timestamp,
  results_dir = results_dir
)

cat("\n===== DHARMa Diagnostics =====\n")
simulationOutput <- simulateResiduals(logistic_model)
plot_file <- file.path(results_dir, paste0(script_name, "_Rplots_", timestamp, ".pdf"))
pdf(plot_file)
plot(simulationOutput)
dev.off()
cat("Saved DHARMa plot to:", plot_file, "\n")

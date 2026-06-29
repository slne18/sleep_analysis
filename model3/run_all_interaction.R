#!/usr/bin/env Rscript

script_dir <- local({
  file_arg <- commandArgs(trailingOnly = FALSE)
  file_arg <- file_arg[grepl("^--file=", file_arg)]
  script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[1]) else "run_all_interaction.R"
  normalizePath(dirname(script_path), winslash = "/", mustWork = FALSE)
})

model_scripts <- c(
  "fitbit_overall_interaction.R",
  "fitbit_stim_cued_interaction.R",
  "phone_overall_interaction.R",
  "phone_stim_cued_interaction.R"
)

for (script in model_scripts) {
  cat("\n==============================\n")
  cat("Running:", script, "\n")
  cat("==============================\n")
  status <- system2("Rscript", file.path(script_dir, script), stdout = "", stderr = "")
  if (!identical(status, 0L)) {
    stop("Script failed: ", script, " (exit status ", status, ")")
  }
}

cat("\nAll model3 interaction scripts completed.\n")

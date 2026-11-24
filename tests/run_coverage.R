#!/usr/bin/env Rscript
# Note: This file was generated with the assistance of AI.

# ==============================================================================
# CodingManageR Local Coverage Verification Script (covr + testthat + shinytest2)
# Usage: Rscript tests/run_coverage.R
# ==============================================================================
# Measures line coverage across all R/ source files (excluding MySQL/MariaDB-only
# blocks marked with `# nocov start` / `# nocov end`), prints a per-file report
# and any uncovered lines, and exits with a non-zero status if total coverage
# falls below the 95.0% threshold.
# ==============================================================================

if (!requireNamespace("covr", quietly = TRUE)) {
  stop("Package 'covr' is required. Install with: install.packages('covr')")
}
if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("Package 'testthat' is required. Install with: install.packages('testthat')")
}

threshold <- as.numeric(Sys.getenv("CODINGMANAGER_COV_THRESHOLD", unset = "95.0"))
cat(sprintf("Running CodingManageR test suite with covr (target >= %.1f%%)...\n", threshold))

cov <- covr::package_coverage(
  path = ".",
  type = "tests",
  quiet = FALSE,
  clean = TRUE
)

total_pct <- covr::percent_coverage(cov)
cat("\n====================================================================\n")
cat(sprintf("Total Package Code Coverage: %.2f%% (Target: >= %.1f%%)\n", total_pct, threshold))
cat("====================================================================\n\n")

# Per-file coverage summary
df_cov <- covr::tally_coverage(cov)
if (nrow(df_cov) > 0) {
  files <- unique(df_cov$filename)
  for (f in sort(files)) {
    sub <- df_cov[df_cov$filename == f, , drop = FALSE]
    pct <- 100 * mean(sub$value > 0)
    cat(sprintf("  %-35s : %6.2f%% (%d / %d lines)\n", f, pct, sum(sub$value > 0), nrow(sub)))
  }
}

# Report any uncovered lines for immediate inspection
zero_cov <- covr::zero_coverage(cov)
if (nrow(zero_cov) > 0) {
  cat("\nUncovered lines:\n")
  print(zero_cov[, c("filename", "line", "functions")], row.names = FALSE)
} else {
  cat("\nNo uncovered lines remaining!\n")
}

if (is.na(total_pct) || total_pct < threshold) {
  stop(sprintf(
    "Coverage gate failed: %.2f%% is below the required %.1f%% threshold.",
    total_pct,
    threshold
  ))
}

cat(sprintf("\nSUCCESS: Code coverage (%.2f%%) meets or exceeds %.1f%%.\n", total_pct, threshold))

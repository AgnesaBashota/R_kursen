# ==== src/diagnostics.R ====
suppressPackageStartupMessages({ library(dplyr); library(readr) })

run_diagnostics <- function(df, outdir = "outputs") {
  ensure_dir(outdir)
  
  desc <- df |>
    summarise(
      n = n(),
      pris_med = suppressWarnings(median(pris, na.rm = TRUE)),
      mil_med  = suppressWarnings(median(mil,  na.rm = TRUE)),
      ar_min   = suppressWarnings(safe_min(ar)),
      ar_max   = suppressWarnings(safe_max(ar))
    )
  write_csv(desc, file.path(outdir, "descriptives.csv"))
  write_csv(slice_head(df, n = 200), file.path(outdir, "preview_clean.csv"))
  
  n1 <- sum(complete.cases(df[, c("log_pris","log_mil","ar")]), na.rm = TRUE)
  n2 <- sum(complete.cases(df[, c("log_pris","log_mil")]),      na.rm = TRUE)
  n3 <- sum(complete.cases(df[, c("log_pris","ar")]),           na.rm = TRUE)
  n4 <- sum(complete.cases(df[, c("pris","mil","ar")]),         na.rm = TRUE)
  n5 <- sum(complete.cases(df[, c("pris","mil")]),              na.rm = TRUE)
  avail <- tibble(
    formula = c("log_pris ~ log_mil + ar", "log_pris ~ log_mil", "log_pris ~ ar",
                "pris ~ mil + ar", "pris ~ mil"),
    usable_rows = c(n1, n2, n3, n4, n5)
  )
  write_csv(avail, file.path(outdir, "model_data_availability.csv"))
}
 
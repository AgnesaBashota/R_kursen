# ==== src/modeling.R ====
suppressPackageStartupMessages({
  library(broom); library(ggplot2); library(readr)
})

fit_models <- function(df, outdir = "outputs") {
  ensure_dir(outdir)
  
  try_fit <- function(formula, dat, min_n = 5) {
    mf <- try(silent = TRUE, expr = model.frame(formula, data = dat))
    if (inherits(mf, "try-error")) return(NULL)
    ok <- stats::complete.cases(mf)
    mf_ok <- mf[ok, , drop = FALSE]
    if (nrow(mf_ok) < min_n) return(NULL)
    list(data = mf_ok, model = lm(formula, data = mf_ok))
  }
  
  # fler fallback-formler
  m <- try_fit(log_pris ~ log_mil + ar, df) %||%
    try_fit(log_pris ~ log_mil,        df) %||%
    try_fit(log_pris ~ ar,             df) %||%
    try_fit(pris ~ mil + ar,           df) %||%
    try_fit(pris ~ mil,                df)
  
  if (is.null(m)) {
    writeLines("För få kompletta rader för att passa någon modell.",
               file.path(outdir, "NO_MODEL.txt"))
    warning("För få rader för modell – hoppar över modellkörning.")
    return(invisible(NULL))
  }
  
  m1 <- m$model
  write_csv(tidy(m1),   file.path(outdir, "model_coefficients.csv"))
  write_csv(glance(m1), file.path(outdir, "model_glance.csv"))
  
  aug <- augment(m1)
  p <- ggplot(aug, aes(.fitted, .resid)) +
    geom_point(alpha = 0.4) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "Fitted", y = "Residuals", title = "Residualer (vald modell)")
  ggsave(filename = file.path(outdir, "model_residuals.png"),
         plot = p, width = 7, height = 5, dpi = 150)
  
  invisible(list(model = m1, n = nrow(m$data)))
}

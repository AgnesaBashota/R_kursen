suppressPackageStartupMessages({ library(ggplot2); library(dplyr) })

make_all_plots <- function(df, outdir = "outputs") {
  ensure_dir(outdir)
  
  # Filtrera bort NA innan plot:
  df_pris <- df %>% filter(is.finite(pris))
  p <- ggplot(df_pris, aes(pris)) +
    geom_histogram(bins = 40) +
    labs(title = "Pris", x = "Pris (SEK)", y = "Antal")
  ggsave(filename = file.path(outdir, "hist_pris.png"),
         plot = p, width = 7, height = 5, dpi = 150)
}

# ==== run_all.R ====

suppressPackageStartupMessages({
  library(tidyverse); library(readxl); library(janitor); library(lubridate); library(broom)
})

source("src/utils.R")
source("src/load_data.R")
source("src/clean_data.R")
source("src/modeling.R")
source("src/diagnostics.R")
source("src/plots.R")

ensure_dir("outputs")

# 1) Läs rådata
excel_path <- "data/data_insamling_volvo_blocket.xlsx"   # ändra om din fil ligger annorstädes
df_raw <- load_volvo_excel(excel_path)
saveRDS(df_raw, "outputs/01_raw.rds")

# 2) Rensa / feature engineering
df <- clean_volvo(df_raw)

# 3) Beskrivningar & preview
run_diagnostics(df, outdir = "outputs")

# 4) Plots
make_all_plots(df, outdir = "outputs")

# 5) Enkel modell
fit_models(df, outdir = "outputs")

message("KLART ✅  – Kolla mappen: outputs/")

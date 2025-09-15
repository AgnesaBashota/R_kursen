# ==== src/load_data.R ====

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(readr)
})

load_volvo_excel <- function(path_excel) {
  stopifnot(file.exists(path_excel))
  df <- readxl::read_excel(path_excel)
  return(df) 
}

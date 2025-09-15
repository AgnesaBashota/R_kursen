# ==== src/utils.R ====
suppressPackageStartupMessages({
  library(tidyverse)
  library(stringr)
  library(lubridate)
})

ensure_dir <- function(path) if (!dir.exists(path)) dir.create(path, recursive = TRUE)

# Svenska tal -> numeriskt
num_sv <- function(x){
  if (is.numeric(x)) return(x)
  x <- as.character(x)
  x <- str_replace_all(x, fixed("\u00A0"), " ")      # NBSP
  x <- str_replace_all(x, "[^0-9,\\.\\- ]", "")      # ta bort "kr" etc
  x <- str_replace_all(x, " ", "")                   # ta bort mellanslag (tusental)
  x <- str_replace_all(x, "\\.", "")                 # ta bort tusentalspunkt
  x <- str_replace_all(x, ",", ".")                  # komma -> punkt
  suppressWarnings(as.numeric(x))
}

# plocka första 4-siffriga årtal från text (1980..nu+1)
extract_year_from_text <- function(txt){
  if (is.null(txt)) return(NA_integer_)
  s <- paste(na.omit(as.character(txt)), collapse = " ")
  m <- stringr::str_extract_all(s, "(19|20)\\d{2}")[[1]]
  if (length(m) == 0) return(NA_integer_)
  yrs <- suppressWarnings(as.integer(m))
  this_year <- lubridate::year(Sys.Date())
  yrs <- yrs[yrs >= 1980 & yrs <= this_year + 1]
  if (length(yrs) == 0) return(NA_integer_)
  yrs[1]
}

# säkra min/max
safe_min <- function(x) if (all(is.na(x))) NA_real_ else suppressWarnings(min(x, na.rm = TRUE))
safe_max <- function(x) if (all(is.na(x))) NA_real_ else suppressWarnings(max(x, na.rm = TRUE))

`%||%` <- function(a,b) if (is.null(a)) b else a

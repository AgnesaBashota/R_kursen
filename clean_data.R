# src/clean_data.R  (ERSÄTT ALLT INNEHÅLL MED DET HÄR)

suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(lubridate)
  library(stringr)
})

# --- Hjälpfunktioner ---------------------------------------------------------

# Skapa mapp om den saknas
ensure_dir <- function(path) if (!dir.exists(path)) dir.create(path, recursive = TRUE)

# Ta första kolumn som finns, annars NA av rätt längd
get_col <- function(df, candidates) {
  for (nm in candidates) {
    if (nm %in% names(df)) return(df[[nm]])
  }
  return(rep(NA, nrow(df)))
}

# Svenska tal -> numeric (tar bort kr, mellanslag, NBSP, tusenpunkt, byter , till .)
num_sv <- function(x) {
  if (is.numeric(x)) return(x)
  x <- as.character(x)
  x <- str_replace_all(x, fixed("\u00A0"), " ")   # NBSP -> space
  x <- str_replace_all(x, "[^0-9,.-]", "")       # ta bort text (kr, SEK, etc.)
  x <- str_replace_all(x, "\\.", "")             # ta bort tusenpunkt
  x <- str_replace_all(x, ",", ".")              # byt decimaltecken
  x <- str_trim(x)
  x[x %in% c("", "-", ".", ",")] <- NA
  suppressWarnings(as.numeric(x))
}

# --- Huvudfunktion -----------------------------------------------------------

clean_volvo <- function(df_raw, outdir = "outputs") {
  
  df <- df_raw %>%
    clean_names()  # "Försäljningspris" -> "forsaljningspris", "Modellår" -> "modellar", osv.
  
  # Hämta kända kolumnvarianter
  pris_raw <- get_col(df, c("forsaljningspris","pris","saljpris","pris_sek"))
  mil_raw  <- get_col(df, c("miltal","mil","mil_tal","mileage"))
  ar_raw   <- get_col(df, c("modellar","ar","modell_ar","arsmodell","model_ar"))
  hk_raw   <- get_col(df, c("hastkrafter","hk","effekt_hk"))
  cc_raw   <- get_col(df, c("motorstorlek","cc","slagvolym"))
  bransle  <- get_col(df, c("bransle","drivmedel","fuel"))
  vxl      <- get_col(df, c("vaxellada","vxl","gearbox"))
  driv     <- get_col(df, c("drivning","driv","drive"))
  datum_trafik_raw <- get_col(df, c("datum_i_trafik","datumitrafik","datum_trafik","forsta_reg"))
  
  marke    <- get_col(df, c("marke","brand"))
  modell   <- get_col(df, c("modell","model"))
  biltyp   <- get_col(df, c("biltyp","kaross","body"))
  sallare  <- get_col(df, c("saljare","säljare","saljtyp","seller"))
  farg     <- get_col(df, c("farg","färg","color"))
  
  # Bygg en ren, konsekvent tabell
  df_clean <- tibble(
    pris        = num_sv(pris_raw),
    mil         = num_sv(mil_raw),
    ar          = suppressWarnings(as.numeric(ar_raw)),
    hk          = num_sv(hk_raw),
    cc          = num_sv(cc_raw),
    bransle     = na_if(as.character(bransle), ""),
    vaxellada   = na_if(as.character(vxl), ""),
    drivning    = na_if(as.character(driv), ""),
    datum_i_trafik = suppressWarnings(ymd(datum_trafik_raw)),
    marke       = na_if(as.character(marke), ""),
    modell      = na_if(as.character(modell), ""),
    biltyp      = na_if(as.character(biltyp), ""),
    saljare     = na_if(as.character(sallare), ""),
    farg        = na_if(as.character(farg), "")
  )
  
  # Grundläggande rimlighetsfilter (behåll NA – filtrera bara uppenbart fel)
  df_clean <- df_clean %>%
    mutate(
      # om årsmodell saknas: försök härleda från datum_i_trafik
      ar = if_else(is.na(ar) & !is.na(datum_i_trafik), year(datum_i_trafik), ar)
    ) %>%
    filter(
      is.na(pris) | (pris >= 1000 & pris <= 2e6),
      is.na(mil)  | (mil  >= 0    & mil  <= 5e5),
      is.na(ar)   | (ar   >= 1980 & ar   <= year(Sys.Date()) + 1)
    )
  
  # Härledda variabler som vissa analyser behöver
  df_clean <- df_clean %>%
    mutate(
      alder = if_else(!is.na(ar), year(Sys.Date()) - ar, NA_real_),
      log_pris = if_else(!is.na(pris) & pris > 0, log(pris), NA_real_),
      log_mil  = if_else(!is.na(mil)  & mil  > 0, log(mil),  NA_real_)
    )
  
  # Spara mellanresultat om utmappen finns/önskas
  ensure_dir(outdir)
  saveRDS(df_clean, file.path(outdir, "02_clean.rds"))
  
  message(
    sprintf("Rader totalt: %d | Efter filtrering: %d | med pris: %d | med mil: %d",
            nrow(df), nrow(df_clean),
            sum(!is.na(df_clean$pris)),
            sum(!is.na(df_clean$mil)))
  )
  
  return(df_clean)
}

# Exempel: om du vill kunna köra filen direkt för snabb test (frivilligt)
if (sys.nframe() == 0) {
  # Försök läsa 01_raw om den finns och kör städningen
  if (file.exists("outputs/01_raw.rds")) {
    raw <- readRDS("outputs/01_raw.rds")
    invisible(clean_volvo(raw, outdir = "outputs"))
    message("KLART ✅ – Kolla mappen outputs/")
  } else {
    message("Tips: kör först run_all.R för att skapa outputs/01_raw.rds")
  }
} 

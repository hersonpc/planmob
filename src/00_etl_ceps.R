library(tidyverse)

is_empty <- function(x) {
    return(is.na(x) | x == "NULL" | x == "")
}

ceps <- readr::read_csv2("data/RAW_CEPS_GOIANIA.csv")
ceps2 <-
    ceps |> 
    filter(!is_empty(NUMR_CEP) & !is_empty(x_coord) & !is_empty(y_coord)) |>
    mutate(NUMR_CEP = NUMR_CEP |> as.numeric(),
           LOG = x_coord |> as.numeric(),
           LAT = as.numeric(y_coord)) |> 
    filter(NUMR_CEP > 10000000) |> 
    select(-x_coord, -y_coord) |> 
    arrange(NUMR_CEP)

ceps_geo <-
    ceps2 |> 
    group_by(NUMR_CEP) |> 
    summarize(n = n(),
              min_lat = min(LAT),
              max_lat = max(LAT),
              avg_lat = mean(LAT),
              min_log = min(LOG),
              max_log = min(LOG),
              avg_log = mean(LOG)) |> 
    select(CEP = NUMR_CEP,
           LAT = min_lat,
           LOG = min_log) |> 
    arrange(CEP)

readr::write_csv2(ceps_geo, file = "data/CEPS_GEO_GOIANIA.csv")

dados <- readr::read_csv2("Base_Pesquisa.csv") # Trocar por uma chamada a API

OD_GEO <- 
    dados |> 
    select(CEP, CEP_DESTINO) |> 
    filter(!is_empty(CEP) & !is_empty(CEP_DESTINO)) |>
    mutate(CEP = CEP |> as.numeric(),
           CEP_DESTINO = CEP_DESTINO |> as.numeric()) |> 
    left_join(ceps_geo, by = c("CEP" = "CEP")) |> 
    left_join(ceps_geo, by = c("CEP_DESTINO" = "CEP")) |> 
    select(CEP_ORIGEM = CEP, LAT_ORIGEM = LAT.x, LOG_ORIGEM = LOG.x,
           CEP_DESTINO, LAT_DESTINO = LAT.y, LOG_DESTINO = LOG.y) |> 
    filter(!is_empty(LAT_ORIGEM) & !is_empty(LOG_ORIGEM) &
               !is_empty(LAT_DESTINO) & !is_empty(LOG_DESTINO))

readr::write_csv2(OD_GEO, file = "data/OD_GOIANIA.csv")

library(tidyverse)

## Funções utilitarias ----
is_empty <- function(x) {
    return(is.na(x) | x == "NULL" | x == "")
}

## Importando os dados base ----
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

## Gravando dados tratados ----
readr::write_csv2(ceps_geo, file = "data/CEPS_GEO_GOIANIA.csv")

# Trabalhando as respostas da pesquisa ----
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

## Gravando analise ----
readr::write_csv2(OD_GEO, file = "data/OD_GOIANIA.csv")


# Plotando as origens e destinos no mapa ----
library(sf)
library(ggplot2)

# https://ipeagit.github.io/geobr/
# install.packages("devtools")
# devtools::install_github("ipeaGIT/geobr", subdir = "r-package")
library(geobr)

## Obtendo mapa
goiania_cod_ibge = 5208707 # https://ibge.gov.br/explica/codigos-dos-municipios.php
goiania <- read_municipality(code_muni = goiania_cod_ibge, 
                             year = 2020)

## Plot
OD_GEO |> 
    ggplot() + 
    geom_segment(aes(x = LOG_ORIGEM, xend = LOG_DESTINO,
                     y = LAT_ORIGEM, yend = LAT_DESTINO), alpha = .3) +
    geom_point(aes(x = LOG_ORIGEM, y = LAT_ORIGEM), size = 2, alpha = .4, col = "darkgreen") +
    geom_point(aes(x = LOG_DESTINO, y = LAT_DESTINO), size = 1.5, alpha = .4, col = "darkred") +
    geom_sf(data=goiania, fill="transparent", color="black", size=0.8, show.legend = FALSE) +
    theme_bw() +
    labs(title = "Pesquisa OD", 
         subtitle = "Visualização previa dos pontos válidos (Origem > Destino)",
         x = "", 
         y = "", 
         caption = stringr::str_glue("Fonte: SICTEC em ", as.character(lubridate::now())))

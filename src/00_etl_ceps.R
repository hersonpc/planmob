library(tidyverse)

## Funções utilitarias ----
is_empty <- function(x) {
    return(is.na(x) | x == "NULL" | x == "")
}

## Importando os dados base ----
## Lendo o arquivo de CEPs da base do municipio de Goiânia
ceps <- readr::read_csv2("data/csv/RAW_CEPS_GOIANIA.csv")
ceps2 <-
    ceps |> 
    # eliminar dados inconsistentes
    filter(!is_empty(NUMR_CEP) & !is_empty(x_coord) & !is_empty(y_coord)) |>
    # modificar o tipo de dado das colunas
    mutate(NUMR_CEP = NUMR_CEP |> as.numeric(),
           LOG = x_coord |> as.numeric(),
           LAT = as.numeric(y_coord)) |> 
    # eliminar dados inconsistentes
    filter(NUMR_CEP > 10000000) |> 
    # eliminar variáveis desnecessárias
    select(-x_coord, -y_coord) |> 
    # ordenar dados por cep
    arrange(NUMR_CEP)

ceps_geo <-
    ceps2 |> 
    # agrupar pelo cep
    group_by(NUMR_CEP) |> 
    # calcular estatísticas
    summarize(n = n(),
              min_lat = min(LAT),
              max_lat = max(LAT),
              avg_lat = mean(LAT),
              min_log = min(LOG),
              max_log = min(LOG),
              avg_log = mean(LOG)) |> 
    # selecionar campos escolhidos
    select(CEP = NUMR_CEP,
           LAT = min_lat,
           LOG = min_log) |> 
    # ordenar dados por cep
    arrange(CEP)

## Gravando dados tratados ----
readr::write_csv2(ceps_geo, file = "data/csv/CEPS_GEO_GOIANIA.csv")

# Trabalhando as respostas da pesquisa ----
dados <- readr::read_csv2("Base_Pesquisa.csv") # Trocar por uma chamada a API

OD_GEO <- 
    dados |> 
    # selecionar variáveis de interesse
    select(CEP, CEP_DESTINO) |> 
    # eliminar dados inconsistentes
    filter(!is_empty(CEP) & !is_empty(CEP_DESTINO)) |>
    # modificar tipo de dado das colunas
    mutate(CEP = CEP |> as.numeric(),
           CEP_DESTINO = CEP_DESTINO |> as.numeric()) |> 
    # buscar coordenadas correspondentes ao cep de origem
    left_join(ceps_geo, by = c("CEP" = "CEP")) |> 
    # buscar coordenadas correspondentes ao cep de destino
    left_join(ceps_geo, by = c("CEP_DESTINO" = "CEP")) |> 
    # selecionar variáveis de interesse
    select(CEP_ORIGEM = CEP, LAT_ORIGEM = LAT.x, LOG_ORIGEM = LOG.x,
           CEP_DESTINO, LAT_DESTINO = LAT.y, LOG_DESTINO = LOG.y) |> 
    # filtrar dados inconsistentes para a analise OD
    filter(!is_empty(LAT_ORIGEM) & !is_empty(LOG_ORIGEM) &
               !is_empty(LAT_DESTINO) & !is_empty(LOG_DESTINO))

## Gravando analise ----
readr::write_csv2(OD_GEO, file = "data/csv/OD_GOIANIA.csv")


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
    # desenhar as linhas entre a origem e o destino
    geom_segment(aes(x = LOG_ORIGEM, xend = LOG_DESTINO,
                     y = LAT_ORIGEM, yend = LAT_DESTINO), alpha = .3) +
    # desenhar os pontos de origem
    geom_point(aes(x = LOG_ORIGEM, y = LAT_ORIGEM), size = 2, alpha = .4, col = "darkgreen") +
    # desenhar os pontos de destino
    geom_point(aes(x = LOG_DESTINO, y = LAT_DESTINO), size = 1.5, alpha = .4, col = "darkred") +
    # desenhar as bordas do municipio de goiania
    geom_sf(data=goiania, fill="transparent", color="black", size=0.8, show.legend = FALSE) +
    # alterar o fundo padrão
    theme_bw() +
    # especificar os titulos e descrições da imagem
    labs(title = "Pesquisa OD", 
         subtitle = "Visualização previa dos pontos válidos (Origem > Destino)",
         x = "", 
         y = "", 
         caption = stringr::str_glue("Fonte: SICTEC em ", as.character(lubridate::now())))

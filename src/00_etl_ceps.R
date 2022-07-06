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
    # elimina OD que tenham o mesmo CEP
    filter(CEP != CEP_DESTINO) |> 
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

## Tratamento das coordenadas e vinculo as zonas de trafego ----
library(sf)

zonas_trafego <- sf::st_read("data/shapefiles/zonas_trafego/zonas_trafego.shp")

ceps_na_pesquisa <-
    # Combinando os cep de origem e destino
    rbind(select(OD_GEO, CEP = CEP_ORIGEM, LAT = LAT_ORIGEM, LOG = LOG_ORIGEM),
          select(OD_GEO, CEP = CEP_DESTINO, LAT = LAT_DESTINO, LOG = LOG_DESTINO)) |> 
    # agrupar pelo cep origem
    group_by(CEP) |> 
    # calcular estatísticas
    summarize(n = n(),
              LAT = min(LAT),
              LOG = min(LOG)) |> 
    # ordenar dados por cep
    arrange(CEP) |> 
    # removendo colunas desnecessárias
    select(-n)

pontos_od_zoneados <- 
    sf::st_as_sf(ceps_na_pesquisa, 
                 coords = c('LOG', 'LAT'), 
                 crs = st_crs(zonas_trafego)) |> 
    mutate(
        intersection = as.integer(st_intersects(geometry, zonas_trafego)), 
        area = if_else(is.na(intersection), '', zonas_trafego$Name[intersection])
    )
    

OD_GEO_ZONAS <-
    OD_GEO |> 
    left_join(pontos_od_zoneados, by = c("CEP_ORIGEM" = "CEP")) |> 
    rename(AREA_ORIGEM = area,
           POINT_ORIGEM = geometry,
           INTERSECCAO_ORIGEM = intersection) |> 
    left_join(pontos_od_zoneados, by = c("CEP_DESTINO" = "CEP")) |> 
    rename(AREA_DESTINO = area,
           POINT_DESTINO = geometry,
           INTERSECCAO_DESTINO = intersection)

## Gravando tabela de CEPs com os respectivos zoneamentos ----
readr::write_rds(pontos_od_zoneados, file = "data/rds/pontos_od_zoneados.rds")

## Gravando tabela da pesquisa OD com os respectivos zoneamentos ----
readr::write_csv2(OD_GEO_ZONAS, file = "data/csv/OD_GOIANIA_ZONEADO.csv")




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

# ggplot2::ggplot() +
#     geom_sf(data=sf::st_read("data/shapefiles/zonas_trafego/zonas_trafego.shp"),
#             mapping = aes(fill = Name), show.legend = FALSE) +
#     scale_fill_viridis_d() +
#     facet_wrap(~Name)

# zonas_trafego = sf::st_read("data/shapefiles/zonas_trafego/zonas_trafego.shp")
# for (i in 1:nrow(OD_GEO |> head(5))) {
#     # plot(zonas_trafego[i, ], add = TRUE)
#     item <- OD_GEO[i, ]
#     print(stringr::str_glue("Verificando o ponto {i}: {item$LAT_ORIGEM}, {item$LOG_ORIGEM}"))
#     for (z in 1:nrow(zonas_trafego)) {
#         zona <- zonas_trafego[z, ]
#     }
# }

# p0 <- st_point(c(-49.2741, -16.77050))
# pontos <- st_sfc(st_point(c(item$LAT_ORIGEM, item$LOG_ORIGEM)))
# pnts_sf <- st_as_sf(OD_GEO, coords = c('LOG_ORIGEM', 'LAT_ORIGEM'), crs = st_crs(zonas_trafego))


        
# pontos_od %>% sf::st_coordinates() %>% as.data.frame()

# pontos_od_zoneados |> 
#     filter(area == "89")
# 
# ggplot2::ggplot() +
#     geom_sf(data=zonas_trafego,
#             mapping = aes(fill = Name), show.legend = FALSE) +
#     geom_point(data=pnts |> sf::st_coordinates() |> as.data.frame(), 
#                aes(x = X, 
#                    y = Y), 
#                size = 1)


# calculando estatisticas por zona de tráfego
od_por_zonas <-
    (rename(zonas_trafego, AREA = Name) |> as.data.frame()) |> 
    left_join(
        merge(
            OD_GEO_ZONAS |> 
                group_by(AREA = AREA_ORIGEM) |> 
                summarise(ORIGINADO = n()) |> 
                arrange(desc(ORIGINADO)),
            
            OD_GEO_ZONAS |> 
                group_by(AREA = AREA_DESTINO) |> 
                summarise(DESTINADO = n()) |> 
                arrange(desc(DESTINADO)),
            by = "AREA"
        )
    ) |>
    tibble() |> 
    mutate(TOTAL = ORIGINADO + DESTINADO) |> 
    select(AREA, ORIGINADO, DESTINADO, TOTAL, geometry) |> 
    arrange(desc(TOTAL))
    
# Gravando dados da pesquisa OD por áreas ----
readr::write_csv2(select(od_por_zonas, -geometry), "data/csv/OD_POR_AREA.csv")

## Plot
OD_GEO_ZONAS |> 
    filter(AREA_DESTINO == "90C") |> 
    ggplot() + 
    # desenhar as bordas do municipio de goiania
    # geom_sf(data=goiania, fill="transparent", color="black", size=0.8, show.legend = FALSE) +
    geom_polygon(data = raster::shapefile("data/shapefiles/zonas_trafego/zonas_trafego"),
                 aes(x = long, y = lat, group = group),
                 colour = "black",
                 fill = NA) +
    # desenhar as linhas entre a origem e o destino
    geom_segment(aes(x = LOG_ORIGEM, xend = LOG_DESTINO,
                     y = LAT_ORIGEM, yend = LAT_DESTINO), alpha = .3) +
    # desenhar os pontos de origem
    geom_point(aes(x = LOG_ORIGEM, y = LAT_ORIGEM), size = 1, alpha = .4, col = "darkgreen") +
    # desenhar os pontos de destino
    geom_point(aes(x = LOG_DESTINO, y = LAT_DESTINO), size = 1, alpha = .4, col = "darkred") +
    # alterar o fundo padrão
    theme_bw() +
    # especificar os titulos e descrições da imagem
    labs(title = "Pesquisa OD", 
         subtitle = "Visualização previa dos pontos válidos (Origem > Destino)",
         x = "", 
         y = "", 
         caption = stringr::str_glue("Fonte: SICTEC em ", as.character(lubridate::now())))

# Importando bibliotecas
library(dplyr)
library(purrr)
library(flightsbr)

#Importando bases de dados
load("~/DataMining-R-CEFET/Grupo 6 - Tarefa Data Mining/data/bfd_2018.rdata")
airports_all <- flightsbr::read_airports(type = 'all', showProgress = FALSE)

voos_2018 <- read_flights(
  date = 2018,
  type = "basica",
  showProgress = TRUE,
  select = NULL,
  cache = TRUE
)
View(voos_2018)

#O nome da tabela é bfd, o qual deve ser usado durante a análise

#Amostra e visualização dos dados
##Análise Vôos
View(bfd)
dim(bfd)

##Visualizando quantas variações das classes temos nos voos
view_variations_bfd <- bfd %>%
  select(where(~ is.character(.) || is.factor(.))) %>%
  map_df(~ list(
    num_unicos = n_distinct(.),
    valores = paste(unique(.), collapse = ", ")
  ), .id = "coluna")
View(view_variations_bfd)

##Analise Aerodromos
View(airports_all)




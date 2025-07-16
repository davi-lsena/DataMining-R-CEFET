##Visualizando quantas variações das classes temos nos voos
#view_variations_bfd <- bfd %>%
#  select(where(~ is.character(.) || is.factor(.))) %>%
#  map_df(~ list(
#    num_unicos = n_distinct(.),
#    valores = paste(unique(.), collapse = ", ")
#  ), .id = "coluna")
#View(view_variations_bfd)

#voos_2018 <- read_flights(
#  date = 2018,
#  type = "basica",
#  showProgress = TRUE,
#  select = NULL,
#  cache = TRUE
#)
#View(voos_2018)

# Importando bibliotecas
library(dplyr)
library(flightsbr)
library(lubridate)
library(ggplot2)
library(stringr)
library(caret)

#Importando bases de dados
load("~/DataMining-R-CEFET/Grupo 6 - Tarefa Data Mining/data/bfd_2018.rdata")
aeroportos <- flightsbr::read_airports(type = 'all', showProgress = FALSE)

aeroportos_info <- aeroportos %>%
  select(codigo_oaci, ciad, nome, municipio, uf, longitude, latitude)

#O nome da tabela é bfd, o qual deve ser usado durante a análise
#Visualização dos dados
View(bfd)
dim(bfd)


#Junta informações do aeroporto de chegada/partida
bfd_aero <- bfd %>%
  # Join com o aeroporto de chegada
  left_join(aeroportos_info, by = c("arrival" = "codigo_oaci")) %>%
  rename(
    arrival_ciad = ciad,
    arrival_nome = nome,
    arrival_municipio = municipio,
    arrival_uf = uf,
    arrival_longitude = longitude,
    arrival_latitude = latitude
  ) %>%
  # Join com o aeroporto de partida
  left_join(aeroportos_info, by = c("depart" = "codigo_oaci")) %>%
  rename(
    depart_ciad = ciad,
    depart_nome = nome,
    depart_municipio = municipio,
    depart_uf = uf,
    depart_longitude = longitude,
    depart_latitude = latitude
  )

bfd_filtrado <- bfd_aero %>%
  filter(
    company %in% c("TAM", "GLO", "AZU"),
    depart_uf == 'São Paulo',
    arrival_uf == 'São Paulo',
    !is.na(status_depart),
    !is.na(status_arrival)
  )

bfd_filtrado <- bfd_filtrado %>%
  mutate(
    status_depart_group = case_when(
      str_detect(status_depart, regex("Atraso", ignore_case = TRUE)) ~ "Atraso",
      str_detect(status_depart, regex("Pontual", ignore_case = TRUE)) ~ "Pontual",
      str_detect(status_depart, regex("Antecipado", ignore_case = TRUE)) ~ "Antecipado",
      TRUE ~ "Outro"
    ),
    status_arrival_group = case_when(
      str_detect(status_arrival, regex("Atraso", ignore_case = TRUE)) ~ "Atraso",
      str_detect(status_arrival, regex("Pontual", ignore_case = TRUE)) ~ "Pontual",
      str_detect(status_arrival, regex("Antecipado", ignore_case = TRUE)) ~ "Antecipado",
      TRUE ~ "Outro"
    )
  )

bfd_filtrado <- bfd_filtrado %>%
  mutate(
    status_depart_group = factor(status_depart_group, levels = c("Pontual", "Atraso", "Antecipado", "Outro")),
    status_arrival_group = factor(status_arrival_group, levels = c("Pontual", "Atraso", "Antecipado", "Outro"))
  )


View(bfd_filtrado)


ggplot(bfd_filtrado, aes(x = depart_nome, fill = status_depart_group)) +
  geom_bar(position = "dodge") +
  labs(title = "Status de Partida por Aeroporto de Origem", x = "Aeroporto de Partida", y = "Número de Voos") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(bfd_filtrado, aes(x = arrival_nome, fill = status_arrival_group)) +
  geom_bar(position = "dodge") +
  labs(title = "Status de Chegada por Aeroporto de Destino", x = "Aeroporto de Chegada", y = "Número de Voos") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

bfd_filtrado %>%
  filter(company == "AZU") %>%
  count(status_arrival_group, status_depart_group) %>%
  ggplot(aes(x = status_arrival_group, y =status_depart_group, fill = n)) +
  geom_tile(color = "white") +
  geom_text(aes(label = n), color = "black", size = 4) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Matriz de Status: Partida x Chegada",
       x = "Status de Chegada", y = "Status de  Partida", fill = "Número de Voos") +
  theme_minimal()

conf_matrix <- confusionMatrix(
  data = bfd_filtrado$status_depart_group,
  reference = bfd_filtrado$status_arrival_group
)

print(conf_matrix)


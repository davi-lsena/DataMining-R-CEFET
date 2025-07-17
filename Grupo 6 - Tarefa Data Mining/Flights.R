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
{
library(dplyr)
library(flightsbr)
library(lubridate)
library(ggplot2)
library(stringr)
library(caret)
library(purrr)
library(tibble)
library(reshape)
library(corrplot)
library(WVPlots)
library(GGally)
library(aplpack)
library(DataExplorer)
}

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


# 1. Criar bins manualmente (largura de 10 minutos, como no histograma)
bfd_filtrado <- bfd_filtrado %>%
  mutate(
    delay_depart_bin = cut(delay_depart, breaks = seq(-1000, 1000, by = 10), right = FALSE),
    delay_arrival_bin = cut(delay_arrival, breaks = seq(-1000, 1000, by = 10), right = FALSE)
  )

bfd_tam <- bfd_filtrado %>%
  filter(
    company == "TAM",
  )

bfd_gol <- bfd_filtrado %>%
  filter(
    company == "GLO",
  )

bfd_azul <- bfd_filtrado %>%
  filter(
    company == "AZU",
  )

# 2. Contar número de ocorrências por bin
bins_df_depart <- bfd_gol %>%
  count(delay_depart_bin, status_depart_group) %>%
  filter(n > 1)  # <- Aqui você define o mínimo de ocorrências

bins_df_arrival <- bfd_gol %>%
  count(delay_arrival_bin, status_depart_group) %>%
  filter(n > 1)  # <- Aqui você define o mínimo de ocorrências

# 3. Plotar com ggplot2
ggplot(bins_df_depart, aes(x = delay_depart_bin, y = n, fill = status_depart_group)) +
  geom_col(position = "stack", color = "black") +
  labs(
    title = "GOL - Tempo de espera de partida por Status",
    x = "Faixa de tempo (min)",
    y = "Número de Voos",
    fill = "Status"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(bins_df_arrival, aes(x = delay_arrival_bin, y = n, fill = status_depart_group)) +
  geom_col(position = "stack", color = "black") +
  labs(
    title = "GOL - Tempo de espera de chegada por Status",
    x = "Faixa de tempo (min)",
    y = "Número de Voos",
    fill = "Status"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

bfd_filtrado %>%
  distinct(depart_nome)

#Mapa de calor por status de partida
bfd_filtrado %>%
  count(status_arrival_group, status_depart_group) %>%
  ggplot(aes(x = status_arrival_group, y =status_depart_group, fill = n)) +
  geom_tile(color = "white") +
  geom_text(aes(label = n), color = "black", size = 4) +
  scale_fill_gradient(low = "white", high = "green") +
  labs(title = "Matriz de Status: Partida x Chegada",
       x = "Status de Chegada", y = "Status de  Partida", fill = "Número de Voos") +
  theme_minimal()

conf_matrix <- confusionMatrix(
  data = bfd_filtrado$status_depart_group,
  reference = bfd_filtrado$status_arrival_group
)

bfd_filtrado %>%
  #filter(delay_arrival >= -180, delay_arrival <= 180) %>%  # evita outliers extremos
  ggplot(aes(x = company, y = delay_arrival, fill = company)) +
  geom_boxplot(outlier.shape = 0.5) +
  labs(title = "Boxplot de Atraso na Chegada por Companhia",
       x = "Companhia", y = "Atraso na Chegada (minutos)") +
  theme_minimal()

iqr_stats_depart_atrasos <- bfd_filtrado %>%
  filter(status_depart_group == 'Atraso') %>%
  summarise(
    Q1 = quantile(delay_depart, 0.25, na.rm = TRUE),
    Q3 = quantile(delay_depart, 0.75, na.rm = TRUE)
  ) %>%
  mutate(
    IQR = Q3 - Q1,
    lim_inf = Q1 - 1.5 * IQR,
    lim_sup = Q3 + 1.5 * IQR
  )
print(iqr_stats_depart_atrasos)

bfd_iqr_depart_atrasos <- bfd_filtrado %>%
  filter(
    status_depart_group == 'Atraso',
    delay_depart >= iqr_stats_depart_atrasos$lim_inf,
    delay_depart <= iqr_stats_depart_atrasos$lim_sup
  )

View(bfd_iqr_depart_atrasos)

#Número de outliers removidos
nrow(bfd_filtrado) - nrow(bfd_iqr_depart_atrasos)

#Boxplot por companhia
bfd_iqr_filtrado %>%
  ggplot(aes(x = company, y = delay_depart, fill = company)) +
  geom_boxplot(outlier.shape = 1) +
  labs(title = "Boxplot de Atraso na Chegada por Companhia",
       x = "Companhia", y = "Atraso na Partida (minutos)") +
  theme_minimal()

#Boxplot por aeroporto
bfd_iqr_filtrado %>%
  ggplot(aes(x = depart_nome, y = delay_depart, fill = depart_nome)) +
  geom_boxplot(outlier.shape = 1) +
  labs(title = "Boxplot de Atraso na Chegada por Aeroporto",
       x = "Aeroporto", y = "Atraso na Partida (minutos)") +
  theme_minimal()

bfd_z_filtrado <- bfd_filtrado %>%
  mutate(
    z_delay_depart = scale(delay_depart)
  ) %>%
  filter(abs(z_delay_depart) <= 3)  # ⬅️ Mantém apenas valores dentro de 3 desvios padrão

print(bfd_z_filtrado)

conf_matrix_tam <- confusionMatrix(
  data = bfd_tam$status_depart_group,
  reference = bfd_tam$status_arrival_group
)

conf_matrix_azul <- confusionMatrix(
  data = bfd_azul$status_depart_group,
  reference = bfd_azul$status_arrival_group
)

conf_matrix_gol <- confusionMatrix(
  data = bfd_gol$status_depart_group,
  reference = bfd_gol$status_arrival_group
)

print(conf_matrix_tam)
print(conf_matrix_azul)
print(conf_matrix_gol)

##Visualizando quantas variações das classes temos nos voos
view_variations_bfd <- bfd_filtrado %>%
  select(where(~ is.character(.) || is.factor(.))) %>%
  map_df(~ list(
    num_unicos = n_distinct(.),
    valores = paste(unique(.), collapse = ", ")
  ), .id = "coluna")
View(view_variations_bfd)


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
library(patchwork)
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

aeroportos_arrival <- aeroportos_info %>%
  rename_with(~paste0("arrival_", .), -codigo_oaci)

aeroportos_depart <- aeroportos_info %>%
  rename_with(~paste0("depart_", .), -codigo_oaci)

# Faz os joins com as tabelas renomeadas
bfd_aero <- bfd %>%
  left_join(aeroportos_arrival, by = c("arrival" = "codigo_oaci")) %>%
  left_join(aeroportos_depart, by = c("depart" = "codigo_oaci"))
View(bfd_aero)

cores_personalizadas <- c(
  "GOL" = "#F58220",
  "AZUL" = "#ADD8E6",
  "LATAM" = "#A6192E"
)

status_cores <- c(
  "Atraso" = "#D7263D",
  "Antecipado" = "#1B9AAA",
  "Pontual" = "#3F784C"
)

# Agrupa e seleciona as 5 maiores companhias
top_5_companhias <- bfd_aero %>%
  filter(!is.na(company)) %>%
  group_by(company) %>%
  summarise(total_voos = n()) %>%
  arrange(desc(total_voos)) %>%
  slice_head(n = 5)

# Cria o gráfico
ggplot(top_5_companhias, aes(x = reorder(company, total_voos), y = total_voos, fill = company)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 5 Estados em número de voos de 2018",
    x = "Estado",
    y = "Total de Voos"
  ) +
  theme_minimal()

top_5_estados <- bfd_aero %>%
  filter(!is.na(depart_uf)) %>%
  group_by(depart_uf) %>%
  summarise(total_voos = n()) %>%
  arrange(desc(total_voos)) %>%
  slice_head(n = 5)

ggplot(top_5_estados, aes(x = reorder(depart_uf, total_voos), y = total_voos, fill = depart_uf)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 5 Estados em número de voos de 2018",
    x = "Estado",
    y = "Total de Voos"
  ) +
  theme_minimal()

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

bfd_filtrado <- bfd_filtrado %>%
  mutate(
    company = case_when(
      company == "GLO" ~ "GOL",
      company == "AZU" ~ "AZUL",
      company == "TAM" ~ "LATAM",
      TRUE ~ company
    )
  )
View(bfd_filtrado)


# 1. Criar bins manualmente (largura de 10 minutos, como no histograma)
bfd_filtrado <- bfd_filtrado %>%
  mutate(
    delay_depart_bin = cut(delay_depart, breaks = seq(-1000, 1000, by = 10), right = FALSE),
    delay_arrival_bin = cut(delay_arrival, breaks = seq(-1000, 1000, by = 10), right = FALSE)
  )

# Função para criar os dados binned por companhia e tipo (depart/arrival)
## Função para agrupar dados por bin e status
create_bins_df <- function(df, time_col, status_col) {
  df %>%
    filter(!is.na({{time_col}}), !is.na({{status_col}})) %>%
    count({{time_col}}, {{status_col}}) %>%
    filter(n > 1) %>%
    mutate({{time_col}} := factor({{time_col}}, levels = sort(unique({{time_col}}))))
}

## Função para criar gráfico
create_plot <- function(df, time_col, status_col, title) {
  ggplot(df, aes(x = {{time_col}}, y = n + 1, fill = {{status_col}})) +
    geom_col(position = "stack", color = "black") +
    scale_fill_manual(values = status_cores, name = "Status do voo") +  # <-- NÃO usar guide = "none"
    scale_y_log10() +
    geom_text(aes(label = n), position = position_stack(vjust = 0.5), size = 3) +
    labs(
      title = title,
      x = "Faixa de tempo (min)",
      y = "Número de Voos (escala log)"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
## Lista para armazenar os gráficos
plots <- list()

## Vetor de companhias para análise
companhias <- c("GOL", "LATAM", "AZUL")

## Loop para gerar gráficos por companhia
for (cia in companhias) {
  df_cia <- bfd_filtrado %>% filter(company == cia)

  ### Criar dados binned
  bins_depart <- create_bins_df(df_cia, delay_depart_bin, status_depart_group)
  bins_arrival <- create_bins_df(df_cia, delay_arrival_bin, status_depart_group)

  ### Criar gráficos
  p_depart <- create_plot(
    bins_depart, delay_depart_bin, status_depart_group,
    paste0(cia, " - Minutos até a partida segmentado por status")
  )

  p_arrival <- create_plot(
    bins_arrival, delay_arrival_bin, status_depart_group,
    paste0(cia, " - Minutos até a chegada segmentado por status")
  )

  ### Salvar gráficos na lista
  plots[[paste0(cia, "_depart")]] <- p_depart
  plots[[paste0(cia, "_arrival")]] <- p_arrival
}

## Combinar os gráficos em um grid 2x3
final_plot <- (plots[["GOL_depart"]] + plots[["LATAM_depart"]] + plots[["AZUL_depart"]]) /
  (plots[["GOL_arrival"]] + plots[["LATAM_arrival"]] + plots[["AZUL_arrival"]]) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

## Exibir o grid
print(final_plot)

#Mapa de calor geral
bfd_filtrado %>%
  count(status_arrival_group, status_depart_group) %>%
  ggplot(aes(x = status_arrival_group, y = status_depart_group, fill = n)) +
  geom_tile(color = "white") +
  geom_text(aes(label = n), color = "black", size = 3) +
  scale_fill_gradient(low = "white", high = "green") +
  labs(
    title = "Mapa de c
    alor: Status de Partida x Status Chegada",
    x = "Status de Chegada", y = "Status de Partida", fill = "Número de Voos"
  ) +
  theme_minimal()

### Mapa de calor por companhia
companhias_destacadas <- names(cores_personalizadas)

dados_por_companhia <- bfd_filtrado %>%
  filter(company %in% companhias_destacadas) %>%
  count(company, status_arrival_group, status_depart_group)

# Função para gerar o gráfico com gradiente personalizado
plot_por_companhia <- function(companhia, cor) {
  dados <- dados_por_companhia %>% filter(company == companhia)

  ggplot(dados, aes(x = status_arrival_group, y = status_depart_group, fill = n)) +
    geom_tile(color = "white") +
    geom_text(aes(label = n), color = "black", size = 3) +
    scale_fill_gradient(low = "white", high = cor) +
    labs(
      title = companhia,
      x = "Status de Chegada",
      y = "Status de Partida",
      fill = "Número de Voos"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

# Gera os gráficos
g_gol   <- plot_por_companhia("GOL", cores_personalizadas["GOL"])
g_azul  <- plot_por_companhia("AZUL", cores_personalizadas["AZUL"])
g_latam <- plot_por_companhia("LATAM", cores_personalizadas["LATAM"])

# Junta tudo em uma grade com patchwork (3 colunas)
(g_gol | g_azul | g_latam) +
  plot_annotation(title = "Mapa de calor por companhia: Status de Partida x Status Chegada")

##Boxplot
###Boxplot partida
iqr_stats_depart <- bfd_filtrado %>%
  group_by(company) %>%
  summarise(
    Q1 = quantile(delay_depart, 0.25, na.rm = TRUE),
    Q3 = quantile(delay_depart, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    IQR = Q3 - Q1,
    lim_inf = Q1 - 1.5 * IQR,
    lim_sup = Q3 + 1.5 * IQR
  )

bfd_iqr_depart <- bfd_filtrado %>%
  left_join(iqr_stats_depart, by = "company") %>%
  filter(
    delay_depart >= lim_inf,
    delay_depart <= lim_sup
  )

bfd_iqr_depart %>%
  ggplot(aes(x = company, y = delay_depart, fill = company)) +
  geom_boxplot(outlier.shape = 1) +
  labs(title = "Partida - Distribuição do tempo de espera por companhia",
       x = "Companhia", y = "Tempo de espera (min)") +
  theme_minimal()+
  scale_fill_manual(values = cores_personalizadas)

###Boxplot chegada
# Cálculo do IQR por companhia para delay de chegada
iqr_stats_arrival <- bfd_filtrado %>%
  group_by(company) %>%
  summarise(
    Q1 = quantile(delay_arrival, 0.25, na.rm = TRUE),
    Q3 = quantile(delay_arrival, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    IQR = Q3 - Q1,
    lim_inf = Q1 - 1.5 * IQR,
    lim_sup = Q3 + 1.5 * IQR
  )

# Filtragem dos dados usando os limites do IQR
bfd_iqr_arrival <- bfd_filtrado %>%
  left_join(iqr_stats_arrival, by = "company") %>%
  filter(
    delay_arrival >= lim_inf,
    delay_arrival <= lim_sup
  )

# Geração do gráfico
bfd_iqr_arrival %>%
  ggplot(aes(x = company, y = delay_arrival, fill = company)) +
  geom_boxplot(outlier.shape = 1) +
  labs(
    title = "Chegada - Distribuição do tempo de espera por companhia",
    x = "Companhia", y = "Tempo de espera (min)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = cores_personalizadas)

###Unindo chegada e partida
g_depart <- bfd_iqr_depart %>%
  ggplot(aes(x = company, y = delay_depart, fill = company)) +
  geom_boxplot(outlier.shape = 1) +
  labs(title = "Partida - Distribuição do tempo de espera por companhia",
       x = "Companhia", y = "Tempo de espera (min)") +
  theme_minimal() +
  scale_fill_manual(values = cores_personalizadas)

g_arrival <- bfd_iqr_arrival %>%
  ggplot(aes(x = company, y = delay_arrival, fill = company)) +
  geom_boxplot(outlier.shape = 1) +
  labs(title = "Chegada - Distribuição do tempo de espera por companhia",
       x = "Companhia", y = "Tempo de espera (min)") +
  theme_minimal() +
  scale_fill_manual(values = cores_personalizadas)

# Juntando os dois gráficos em uma linha
g_depart | g_arrival


# Calcula estatísticas IQR atrasos na partida
iqr_stats_late_depart <- bfd_filtrado %>%
  filter(status_depart_group == "Atraso") %>%
  group_by(company) %>%
  summarise(
    Q1 = quantile(delay_depart, 0.25, na.rm = TRUE),
    Q3 = quantile(delay_depart, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    IQR = Q3 - Q1,
    lim_inf = Q1 - 1.5 * IQR,
    lim_sup = Q3 + 1.5 * IQR
  )

# Junta os limites e filtra para manter só os valores dentro do intervalo (sem outliers extremos)
bfd_atrasos_depart <- bfd_filtrado %>%
  filter(status_depart_group == "Atraso") %>%
  left_join(iqr_stats_late_depart, by = "company") %>%
  filter(delay_depart >= lim_inf, delay_depart <= lim_sup)

# Calcula estatísticas IQR atrasos na chegada
iqr_stats_late_arrival <- bfd_filtrado %>%
  filter(status_arrival_group == "Atraso") %>%
  group_by(company) %>%
  summarise(
    Q1 = quantile(delay_arrival, 0.25, na.rm = TRUE),
    Q3 = quantile(delay_arrival, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    IQR = Q3 - Q1,
    lim_inf = Q1 - 1.5 * IQR,
    lim_sup = Q3 + 1.5 * IQR
  )

# Junta os limites e filtra para manter só os valores dentro do intervalo (sem outliers extremos)
bfd_atrasos_arrival <- bfd_filtrado %>%
  filter(status_arrival_group == "Atraso") %>%
  left_join(iqr_stats_late_arrival, by = "company") %>%
  filter(delay_arrival >= lim_inf, delay_arrival <= lim_sup)

# Gráfico atrasos partida (sem outliers extremos)
g_depart <- ggplot(bfd_atrasos_depart, aes(x = company, y = delay_depart, fill = company)) +
  geom_boxplot(outlier.shape = 1) +  # remove os pontos outliers (já filtrados)
  scale_fill_manual(values = cores_personalizadas) +
  labs(
    title = "Tempo de atraso na Partida por companhia",
    x = "Companhia", y = "Atraso na Partida (minutos)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Gráfico atrasos chegada (sem outliers extremos)
g_arrival <- ggplot(bfd_atrasos_arrival, aes(x = company, y = delay_arrival, fill = company)) +
  geom_boxplot(outlier.shape = 1) +  # remove os pontos outliers (já filtrados)
  scale_fill_manual(values = cores_personalizadas) +
  labs(
    title = "Tempo de atraso na chegada por companhia",
    x = "Companhia", y = "Atraso na Chegada (minutos)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Combina os dois gráficos em grid 1 linha, 2 colunas
g_depart + g_arrival + plot_layout(ncol = 2) +
  plot_annotation(title = "Distribuição de tempo dos voos em atraso - Partida e Chegada")


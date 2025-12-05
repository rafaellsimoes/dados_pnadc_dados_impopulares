###############################################################
# Definição de diretório de trabalho
###############################################################

# Mostra diretório atual
getwd()

# Define o diretório onde estão os dados
setwd("C:\\Users\\orafa\\OneDrive\\Documents\\dados impopulares\\dados da PNAD")

###############################################################
# Carregando pacotes necessários
###############################################################

library(PNADcIBGE)   # Para baixar e manipular dados da PNAD Contínua
library(dplyr)       # Manipulação de dados
library(tidyr)       # Transformações como pivot_wider e separate
library(viridis)     # Paleta de cores
library(geobr)       # Mapas do Brasil em formato sf
library(ggspatial)   # Elementos para mapas
library(ggplot2)     # Visualizações
library(sf)          # Dados espaciais
library(survey)      # Designs amostrais e estimações (svyby, svymean, etc.)

###############################################################
# Carrega painel salvo anteriormente (opcional)
###############################################################

load(file = "C:\\Users\\orafa\\OneDrive\\Documents\\dados impopulares\\dados da PNAD\\dados_em_painel.RData")

###############################################################
# PROCESSAMENTO DA PNAD CONTÍNUA – ANO 2024
###############################################################

# Baixa microdados da PNADc 2024 (1ª entrevista)
p24 <- PNADcIBGE::get_pnadc(
  year = 2024, interview = 1,
  labels = FALSE
)

# Cria dummies a partir de VD4013 (não usadas diretamente, mas mantidas)
p24 <- update(
  p24,
  horas_trabalhadas1 = as.numeric(VD4013 == "1"),
  horas_trabalhadas2 = as.numeric(VD4013 == "2"),
  horas_trabalhadas3 = as.numeric(VD4013 == "3"),
  horas_trabalhadas4 = as.numeric(VD4013 == "4"),
  horas_trabalhadas5 = as.numeric(VD4013 == "5")
)

# Estima renda média (V403312) por UF × Escolaridade × Faixa VD4014
renda_media_2024 <- svyby(
  ~V403312,
  ~interaction(UF, VD3004, VD4014, drop = TRUE),
  subset(p24, !is.na(VD3004) & !is.na(VD4014)),
  svymean,
  na.rm = TRUE
)

# Separa coluna de interação em três variáveis
renda_media_2024 <- renda_media_2024 |>
  separate(
    col = `interaction(UF, VD3004, VD4014, drop = TRUE)`,
    into = c("UF","VD3004","VD4014"),
    sep = "\\.",
    remove = TRUE
  ) |>
  mutate(
    UF = as.integer(UF),
    VD3004 = as.integer(VD3004),
    VD4014 = as.integer(VD4014)
  )

# Classifica níveis de escolaridade (VD3004)
renda_media_2024 <- renda_media_2024 |>
  mutate(
    escolaridade = case_when(
      VD3004 == 1 ~ "Sem Instrução Formal",
      VD3004 > 1 & VD3004 <= 3 ~ "Ensino Fundamental",
      VD3004 > 3 & VD3004 <= 5 ~ "Ensino Médio",
      VD3004 > 5 & VD3004 <= 7 ~ "Ensino Superior",
      TRUE ~ NA_character_
    )
  )

# Cria rótulos descritivos da faixa de horas trabalhadas (VD4014)
renda_media_2024 <- renda_media_2024 |>
  mutate(
    faixa_horas_trabalhadas = case_when(
      VD4014 == 1 ~ "Até 14 Horas",
      VD4014 == 2 ~ "15 até 39 Horas",
      VD4014 == 3 ~ "40 até 44 Horas",
      VD4014 == 4 ~ "44 até 48 Horas",
      VD4014 == 5 ~ "49 Horas +"
    )
  )

# Adiciona coluna do ano
renda_media_2024$ano <- 2024

# Organiza colunas finais
renda_media_2024 <- renda_media_2024 |>
  select(
    ano, UF, VD3004, escolaridade,
    VD4014, faixa_horas_trabalhadas,
    V403312, se
  )

###############################################################
# PROCESSAMENTO DA PNAD CONTÍNUA – ANO 2023
###############################################################

p23 <- PNADcIBGE::get_pnadc(
  year = 2023, interview = 1,
  labels = FALSE
)

# Criação de dummies (como no 2024)
p23 <- update(
  p23,
  horas_trabalhadas1 = as.numeric(VD4013 == "1"),
  horas_trabalhadas2 = as.numeric(VD4013 == "2"),
  horas_trabalhadas3 = as.numeric(VD4013 == "3"),
  horas_trabalhadas4 = as.numeric(VD4013 == "4"),
  horas_trabalhadas5 = as.numeric(VD4013 == "5")
)

# Uso correto de VD4014 para faixas de horas
renda_media_2023 <- svyby(
  ~V403312,
  ~interaction(UF, VD3004, VD4014, drop = TRUE),
  subset(p23, !is.na(VD3004) & !is.na(VD4014)),
  svymean,
  na.rm = TRUE
)

# Separa interação em variáveis distintas
renda_media_2023 <- renda_media_2023 |>
  separate(
    col = `interaction(UF, VD3004, VD4014, drop = TRUE)`,
    into = c("UF","VD3004","VD4014"),
    sep = "\\.",
    remove = TRUE
  ) |>
  mutate(
    UF = as.integer(UF),
    VD3004 = as.integer(VD3004),
    VD4014 = as.integer(VD4014)
  )

# Categorias de escolaridade
renda_media_2023 <- renda_media_2023 |>
  mutate(
    escolaridade = case_when(
      VD3004 == 1 ~ "Sem Instrução Formal",
      VD3004 > 1 & VD3004 <= 3 ~ "Ensino Fundamental",
      VD3004 > 3 & VD3004 <= 5 ~ "Ensino Médio",
      VD3004 > 5 & VD3004 <= 7 ~ "Ensino Superior",
      TRUE ~ NA_character_
    )
  )

# Categorias de faixa de horas
renda_media_2023 <- renda_media_2023 |>
  mutate(
    faixa_horas_trabalhadas = case_when(
      VD4014 == 1 ~ "Até 14 Horas",
      VD4014 == 2 ~ "15 até 39 Horas",
      VD4014 == 3 ~ "40 até 44 Horas",
      VD4014 == 4 ~ "44 até 48 Horas",
      VD4014 == 5 ~ "49 Horas +",
      TRUE ~ NA_character_
    )
  )

# Adiciona ano
renda_media_2023$ano <- 2023

# Seleção final
renda_media_2023 <- renda_media_2023 |>
  select(
    ano, UF, VD3004, escolaridade,
    VD4014, faixa_horas_trabalhadas,
    V403312, se
  )

###############################################################
# PROCESSAMENTO DA PNAD CONTÍNUA – ANO 2022
###############################################################

p22 <- PNADcIBGE::get_pnadc(
  year = 2022, interview = 1,
  labels = FALSE
)

# Dummies das horas (mesmo padrão)
p22 <- update(
  p22,
  horas_trabalhadas1 = as.numeric(VD4013 == "1"),
  horas_trabalhadas2 = as.numeric(VD4013 == "2"),
  horas_trabalhadas3 = as.numeric(VD4013 == "3"),
  horas_trabalhadas4 = as.numeric(VD4013 == "4"),
  horas_trabalhadas5 = as.numeric(VD4013 == "5")
)

# Renda média usando VD4014
renda_media_2022 <- svyby(
  ~V403312,
  ~interaction(UF, VD3004, VD4014, drop = TRUE),
  subset(p22, !is.na(VD3004) & !is.na(VD4014)),
  svymean,
  na.rm = TRUE
)

# Separa interação
renda_media_2022 <- renda_media_2022 |>
  separate(
    col = `interaction(UF, VD3004, VD4014, drop = TRUE)`,
    into = c("UF","VD3004","VD4014"),
    sep = "\\.",
    remove = TRUE
  ) |>
  mutate(
    UF = as.integer(UF),
    VD3004 = as.integer(VD3004),
    VD4014 = as.integer(VD4014)
  )

# Escolaridade
renda_media_2022 <- renda_media_2022 |>
  mutate(
    escolaridade = case_when(
      VD3004 == 1 ~ "Sem Instrução Formal",
      VD3004 > 1 & VD3004 <= 3 ~ "Ensino Fundamental",
      VD3004 > 3 & VD3004 <= 5 ~ "Ensino Médio",
      VD3004 > 5 & VD3004 <= 7 ~ "Ensino Superior",
      TRUE ~ NA_character_
    )
  )

# Faixa de horas
renda_media_2022 <- renda_media_2022 |>
  mutate(
    faixa_horas_trabalhadas = case_when(
      VD4014 == 1 ~ "Até 14 Horas",
      VD4014 == 2 ~ "15 até 39 Horas",
      VD4014 == 3 ~ "40 até 44 Horas",
      VD4014 == 4 ~ "44 até 48 Horas",
      VD4014 == 5 ~ "49 Horas +",
      TRUE ~ NA_CHARACTER_
    )
  )

# Adiciona ano
renda_media_2022$ano <- 2022

# Seleção final
renda_media_2022 <- renda_media_2022 |>
  select(
    ano, UF, VD3004, escolaridade,
    VD4014, faixa_horas_trabalhadas,
    V403312, se
  )

###############################################################
# CONSOLIDAÇÃO DOS DADOS (2024, 2023, 2022)
###############################################################

# Empilha todas as tabelas
dados_consolidados <- bind_rows(
  renda_media_2024,
  renda_media_2023,
  renda_media_2022
)

###############################################################
# TRANSFORMAÇÃO PARA FORMATO WIDE
###############################################################

dados_consolidados_wider <- pivot_wider(
  dados_consolidados,
  names_from = c("VD4014", "faixa_horas_trabalhadas"),
  values_from = c("V403312", "se")
)

###############################################################
# RESUMO: MÉDIA DO RENDIMENTO POR ANO × ESCOLARIDADE × FAIXAS
###############################################################

dados_consolidados_wider_sumary <- dados_consolidados_wider |>
  group_by(ano, escolaridade) |>
  summarise(
    across(
      .cols = c(
        "V403312_1_Até 14 Horas",
        "V403312_2_15 até 39 Horas",
        "V403312_3_40 até 44 Horas",
        "V403312_4_44 até 48 Horas",
        "V403312_5_49 Horas +"
      ),
      .fns = ~ mean(.x, na.rm = TRUE),
      .names = "combine_{.col}"
    ),
    .groups = "drop"
  )

# Renomeia colunas para algo mais amigável
dados_consolidados_wider_sumary <- dados_consolidados_wider_sumary |>
  rename(
    rendimento_bruto_ate_14_horas    = `combine_V403312_1_Até 14 Horas`,
    rendimento_bruto_15_ate_39_horas = `combine_V403312_2_15 até 39 Horas`,
    rendimento_bruto_40_ate_44_horas = `combine_V403312_3_40 até 44 Horas`,
    rendimento_bruto_44_ate_48_horas = `combine_V403312_4_44 até 48 Horas`,
    rendimento_bruto_49_mais         = `combine_V403312_5_49 Horas +`
  )

# Visualiza
head(dados_consolidados_wider_sumary)

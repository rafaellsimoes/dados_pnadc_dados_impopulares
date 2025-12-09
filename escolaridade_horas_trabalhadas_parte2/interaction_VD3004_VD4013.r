getwd()
setwd("C:\\Users\\orafa\\OneDrive\\Documents\\dados impopulares\\dados da PNAD")
### Trabalho com a VD4014
library(PNADcIBGE)   
library(dplyr)       
library(tidyr)       
library(viridis)     
library(geobr)       
library(ggspatial)   
library(ggplot2)     
library(sf)          
library(survey)  
library(readr)

p24 <- PNADcIBGE::get_pnadc(
  interview = 1,
  year = 2024,
  labels = FALSE
)

p24 <- update(
  p24,
  horas_trabalhadas = as.numeric(VD4013)
)

p24 <- update(
   p24,
   escolaridade = as.numeric(VD3004)
 )

p24 <- update(
  p24,
  renda_bruta = as.numeric(V403312)
)

p24_atualizado <- subset(
  p24,
  !is.na(horas_trabalhadas) & 
    !is.na(escolaridade) & 
    !is.na(renda_bruta) &
    horas_trabalhadas %in% 1:5 &
    escolaridade %in% 1:7
)

renda_media_2024 <- svyby(
  ~renda_bruta,
  ~interaction(UF, VD3004, VD4013, drop = TRUE),
  subset(p24, !is.na(VD3004) & !is.na(VD4013) & !is.na(renda_bruta)),
  svymean,
  na.rm = TRUE
)

colnames(renda_media_2024)
renda_media_2024 <- renda_media_2024 |>
  separate(
    col = `interaction(UF, VD3004, VD4013, drop = TRUE)`,
    into = c("UF","escolaridade","horas_trabalhadas"),
    sep = "\\.",
    remove = TRUE
  )

renda_media_2024 <- renda_media_2024 |>
  dplyr::mutate(
    escolaridade_adj = case_when(
      escolaridade == 1 ~ "Sem Instrução Formal",
      escolaridade > 1 & escolaridade <= 3 ~ "Ensino Fundamental",
      escolaridade > 3 & escolaridade <= 5 ~ "Ensino Médio",
      escolaridade > 5 & escolaridade <= 7 ~ "Ensino Superior",
      TRUE ~ NA_character_
    )
  )

renda_media_2024 <- renda_media_2024 |>
  dplyr::mutate(
    faixa_horas_trabalhadas = case_when(
      horas_trabalhadas == 1 ~ "Até 14 Horas",
      horas_trabalhadas == 2 ~ "15 até 39 Horas",
      horas_trabalhadas == 3 ~ "40 até 44 Horas",
      horas_trabalhadas == 4 ~ "44 até 48 Horas",
      horas_trabalhadas == 5 ~ "49 Horas +"
    )
  )

head(renda_media_2024)


p23 <-PNADcIBGE::get_pnadc(
  year = 2023,
  interview = 1,
  labels = FALSE
)

p23 <- update(
  p23,
  horas_trabalhadas = as.numeric(VD4013)
)

p23 <- update(
  p23,
  escolaridade = as.numeric(VD3004)
)

p23 <- update(
  p23,
  renda_bruta = as.numeric(V403312)
)

p23_atualizado <- subset(
  p23,
  !is.na(horas_trabalhadas) & 
    !is.na(escolaridade) & 
    !is.na(renda_bruta) &
    horas_trabalhadas %in% 1:5 &
    escolaridade %in% 1:7
)

renda_media_2023 <- svyby(
  ~renda_bruta,
  ~interaction(UF, VD3004, VD4013, drop = TRUE),
  subset(p23, !is.na(VD3004) & !is.na(VD4013) & !is.na(renda_bruta)),
  svymean,
  na.rm = TRUE
)
head(renda_media_2023)
colnames(renda_media_2023)

renda_media_2023 <- renda_media_2023 |>
  separate(
    col = `interaction(UF, VD3004, VD4013, drop = TRUE)`,
    into = c("UF","VD3004","VD4013"),
    sep = "\\."
  ) |>
  dplyr::mutate(
    UF = as.integer(UF),
    VD3004 = as.integer(VD3004),
    VD4013 = as.integer(VD4013)
  )
colnames(renda_media_2023)
renda_media_2023 <- renda_media_2023 |>
  dplyr::rename(
    escolaridade = VD3004
  )|>
  dplyr::rename(
    horas_trabalhadas = VD4013
  )

renda_media_2023 <- renda_media_2023 |>
  dplyr::mutate(
    escolaridade_adj = case_when(
      escolaridade == 1 ~ "Sem Instrução Formal",
      escolaridade > 1 & escolaridade <= 3 ~ "Ensino Fundamental",
      escolaridade > 3 & escolaridade <= 5 ~ "Ensino Médio",
      escolaridade > 5 & escolaridade <= 7 ~ "Ensino Superior",
      TRUE ~ NA_character_
    )
  )

renda_media_2023 <- renda_media_2023 |>
  dplyr::mutate(
    faixa_horas_trabalhadas = case_when(
      horas_trabalhadas == 1 ~ "Até 14 Horas",
      horas_trabalhadas == 2 ~ "15 até 39 Horas",
      horas_trabalhadas == 3 ~ "40 até 44 Horas",
      horas_trabalhadas == 4 ~ "44 até 48 Horas",
      horas_trabalhadas == 5 ~ "49 Horas +"
    )
  )
head(renda_media_2023)
head(renda_media_2024)

p22 <- PNADcIBGE::get_pnadc(
  year = 2022,
  interview = 1,
  labels = FALSE
)

p22 <- update(
  p22,
  horas_trabalhadas = as.numeric(VD4013)
)

p22 <- update(
  p22,
  escolaridade = as.numeric(VD3004)
)

p22 <- update(
  p22,
  renda_bruta = as.numeric(V403312)
)

p22_atualizado <- subset(
  p22,
  !is.na(horas_trabalhadas) & 
    !is.na(escolaridade) & 
    !is.na(renda_bruta) &
    horas_trabalhadas %in% 1:5 &
    escolaridade %in% 1:7
)

renda_media_2022 <- svyby(
  ~renda_bruta,
  ~interaction(UF, VD3004, VD4013, drop = TRUE),
  subset(p22, !is.na(VD3004) & !is.na(VD4013) & !is.na(renda_bruta)),
  svymean,
  na.rm = TRUE
)

renda_media_2022 <- renda_media_2022 |>
  separate(
    col = `interaction(UF, VD3004, VD4013, drop = TRUE)`,
    into = c("UF","escolaridade","horas_trabalhadas"),
    sep = "\\."
  )
renda_media_2022 <- renda_media_2022 |>
  dplyr::mutate(
    escolaridade_adj = case_when(
      escolaridade == 1 ~ "Sem Instrução Formal",
      escolaridade > 1 & escolaridade <= 3 ~ "Ensino Fundamental",
      escolaridade > 3 & escolaridade <= 5 ~ "Ensino Médio",
      escolaridade > 5 & escolaridade <= 7 ~ "Ensino Superior",
      TRUE ~ NA_character_
    )
  )

renda_media_2022 <- renda_media_2022 |>
  dplyr::mutate(
    faixa_horas_trabalhadas = case_when(
      horas_trabalhadas == 1 ~ "Até 14 Horas",
      horas_trabalhadas == 2 ~ "15 até 39 Horas",
      horas_trabalhadas == 3 ~ "40 até 44 Horas",
      horas_trabalhadas == 4 ~ "44 até 48 Horas",
      horas_trabalhadas == 5 ~ "49 Horas +"
    )
  )

head(renda_media_2022)
colnames(renda_media_2022) == colnames(renda_media_2023)
colnames(renda_media_2022) == colnames(renda_media_2024)
colnames(renda_media_2024) == colnames(renda_media_2022)

renda_media_2024$ano <- 2024
colnames(renda_media_2024)
renda_media_2024 <- renda_media_2024 |>
  dplyr::mutate(
    UF = as.integer(UF),
    escolaridade = as.integer(escolaridade),
    horas_trabalhadas = as.integer(horas_trabalhadas),
    renda_bruta = as.double(renda_bruta)
  )
renda_media_2023$ano <- 2023
renda_media_2023 <- renda_media_2023 |>
  dplyr::mutate(
    UF = as.integer(UF),
    escolaridade = as.integer(escolaridade),
    horas_trabalhadas = as.integer(horas_trabalhadas),
    renda_bruta = as.double(renda_bruta)
  )
renda_media_2022$ano <- 2022
renda_media_2022 <- renda_media_2022 |>
  dplyr::mutate(
    UF = as.integer(UF),
    escolaridade = as.integer(escolaridade),
    horas_trabalhadas = as.integer(horas_trabalhadas),
    renda_bruta = as.double(renda_bruta)
  )

dados_consolidados <- dplyr::bind_rows(renda_media_2024, renda_media_2023)
dados_consolidados <- dplyr::bind_rows(dados_consolidados, renda_media_2022)
dados_consolidados
unique(dados_consolidados$ano)

colnames(dados_consolidados)

dados_consolidados <- dados_consolidados |>
  dplyr::select(
    ano, UF, escolaridade, escolaridade_adj, horas_trabalhadas,
    faixa_horas_trabalhadas, renda_bruta, se
  )

colnames(dados_consolidados)
dados_consolidados_wider <- tidyr::pivot_wider(
  dados_consolidados,
  names_from = c("horas_trabalhadas","faixa_horas_trabalhadas"),
  values_from = c("renda_bruta","se")
)

readr::write_csv2(dados_consolidados, "dados_consolidados_parte2.csv")
readr::write_csv2(dados_consolidados_wider, "dados_consolidados_wider_parte2.csv")
readr::write_csv2(renda_media_2022,"renda_media_2022_parte2.csv")
readr::write_csv2(renda_media_2023,"renda_media_2023_parte2.csv")
readr::write_csv2(renda_media_2024,"renda_media_2024_parte2.csv")
save.image("C:\\Users\\orafa\\OneDrive\\Documents\\dados impopulares\\dados da PNAD\\painel_continuacao.RData")
load(file = "C:\\Users\\orafa\\OneDrive\\Documents\\dados impopulares\\dados da PNAD\\painel_continuacao.RData")

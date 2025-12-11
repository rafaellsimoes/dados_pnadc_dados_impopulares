# =======================================================================
#   PNAD CONTÍNUA 2024 – RENDIMENTO MÉDIO POR REGIÃO, ESCOLARIDADE E HORAS
#   Autor: Rafael Simões
#   Data: 2024
# =======================================================================

# Pacotes ----------------------------------------------------------------

library(PNADcIBGE)
library(dplyr)
library(tidyr)
library(survey)
library(readr)
library(ggplot2)
library(geobr)
library(viridis)
library(sf)
library(ggspatial)
library(here)

# =======================================================================
#   1. Importação dos dados da PNADC 2024
# =======================================================================

p24 <- get_pnadc(
  year = 2024,
  interview = 1,
  labels = FALSE
)

# =======================================================================
#   2. Variáveis derivadas: Escolaridade, Região, Horas e Rendimento
# =======================================================================

# Escolaridade numérica
p24 <- update(
  p24,
  escolaridade_adj = case_when(
    VD3004_adj <= 3 ~ 1,
    VD3004_adj > 3 & VD3004_adj <= 6 ~ 2,
    VD3004_adj > 6 ~ 3
  )
)

# Escolaridade nominal
p24 <- update(
  p24,
  escolaridade = case_when(
    VD3004_adj <= 3 ~ "Ensino Fundamental",
    VD3004_adj > 3 & VD3004_adj <= 6 ~ "Ensino Médio",
    VD3004_adj > 6 ~ "Ensino Superior"
  )
)

# UF numérico
p24 <- update(
  p24, 
  UF_adj = as.integer(UF)
)

# Macroregião numérica
p24 <- update(
  p24,
  grande_regiao = case_when(
    UF_adj < 20 ~ 1,
    UF_adj >= 20 & UF_adj < 30 ~ 2,
    UF_adj >= 30 & UF_adj < 40 ~ 3,
    UF_adj >= 40 & UF_adj < 50 ~ 4,
    UF_adj >= 50 ~ 5
  )
)

# Macroregião nominal
p24 <- update(
  p24,
  GR = case_when(
    grande_regiao == 1 ~ "NORTE",
    grande_regiao == 2 ~ "NORDESTE",
    grande_regiao == 3 ~ "SUDESTE",
    grande_regiao == 4 ~ "SUL",
    grande_regiao == 5 ~ "CENTRO-OESTE"
  )
)

# Horas trabalhadas
p24 <- update(
  p24,
  VD4013_adj = as.integer(VD4013)
)

p24 <- update(
  p24,
  horas_habituais_adj = case_when(
    VD4013_adj <= 3 ~ 1,
    VD4013_adj == 4 ~ 2,
    VD4013_adj > 4 ~ 3
  )
)

# Rendimento
p24 <- update(
  p24,
  rendimento_bruto = as.double(V403312)
)

# =======================================================================
#   3. Estatísticas: Renda média ajustada
# =======================================================================

renda_media_2024 <- svyby(
  ~rendimento_bruto,
  ~interaction(grande_regiao, escolaridade_adj, horas_habituais_adj, drop = TRUE),
  design = subset(p24, !is.na(escolaridade_adj) & !is.na(horas_habituais_adj) & !is.na(rendimento_bruto)),
  svymean,
  vartype = c("se", "cv"),
  na.rm = TRUE,
  multicore = TRUE
)

# =======================================================================
#   4. Pós-processamento da tabela
# =======================================================================

renda_media_2024 <- renda_media_2024 %>%
  separate(
    col = `interaction(grande_regiao, escolaridade_adj, horas_habituais_adj, drop = TRUE)`,
    into = c("grande_regiao","escolaridade_adj","horas_habituais_adj"),
    sep = "\\."
  ) %>%
  rename(coef_cv = cv.rendimento_bruto) %>%
  mutate(
    escolaridade_adj = as.integer(escolaridade_adj),
    horas_habituais_adj = as.integer(horas_habituais_adj),
    grande_regiao = as.integer(grande_regiao)
  ) %>%
  mutate(
    escolaridade = case_when(
      escolaridade_adj == 1 ~ "Ensino Fundamental",
      escolaridade_adj == 2 ~ "Ensino Médio",
      escolaridade_adj == 3 ~ "Ensino Superior"
    ),
    horas_habituais = case_when(
      horas_habituais_adj == 1 ~ "Até 39 horas de trabalho",
      horas_habituais_adj == 2 ~ "Entre 40 à 44 horas de trabalho",
      horas_habituais_adj == 3 ~ "Mais de 44 horas de trabalho"
    ),
    GR = case_when(
      grande_regiao == 1 ~ "NORTE",
      grande_regiao == 2 ~ "NORDESTE",
      grande_regiao == 3 ~ "SUDESTE",
      grande_regiao == 4 ~ "SUL",
      grande_regiao == 5 ~ "CENTRO-OESTE"
    )
  ) %>%
  select(
    grande_regiao, GR,
    escolaridade_adj, escolaridade,
    horas_habituais_adj, horas_habituais,
    rendimento_bruto, se, coef_cv
  )

# Tabela wide
renda_media_2024_wider <- renda_media_2024 %>%
  tidyr::pivot_wider(
    names_from = c("horas_habituais","horas_habituais_adj"),
    values_from = c("rendimento_bruto","se","coef_cv")
  )

# =======================================================================
#   5. Exportação reprodutível (sem caminhos locais)
# =======================================================================

save(
  p24,
  renda_media_2024,
  renda_media_2024_wider,
  file = here("dados", "pnadc_2024_subocupacao.RData")
)


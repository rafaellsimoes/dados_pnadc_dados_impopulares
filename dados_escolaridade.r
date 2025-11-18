# Diretório de trabalho
getwd()
setwd("data/pnad")   # pasta dentro do repositório

library(PNADcIBGE)
library(survey)
library(dplyr)
library(tidyr)
library(viridis)
library(geobr)
library(ggspatial)
library(ggplot2)
library(sf)

# Baixando os dados da PNAD
p24 <- get_pnadc(
  year = 2024,
  interview = 1,
  labels = FALSE,
  savedir = "data/pnad"   # diretório relativo
)

# Salvando e carregando imagem com caminhos relativos
save.image("data/pnad/dados_mapas2.RData")
load("data/pnad/dados_mapas2.RData")

populacao_total <- sum(svytotal(~UF, p24))

populacao_por_estado <- as.data.frame(svytotal(~UF, p24))
head(populacao_por_estado)
str(populacao_por_estado)
populacao_por_estado$UF <- rownames(populacao_por_estado)

populacao_por_estado <- populacao_por_estado[, c("UF", "total", "SE")]

renda_media_escolaridade <- svyby(
  ~V403312, ~VD3004,
  p24, svymean, na.rm = TRUE
)

renda_media_por_estado <- svyby(
  ~V403312, ~interaction(UF, VD3004),
  p24, svymean, na.rm = TRUE
)

renda_media_por_estado <- renda_media_por_estado |>
  separate(
    col = `interaction(UF, VD3004)`,
    into = c("UF","VD3004"),
    sep = "\\.",
    remove = TRUE
  ) |>
  mutate(
    UF = as.integer(UF),
    VD3004 = as.integer(VD3004),
    renda_media = V403312
  ) |>
  select(UF, VD3004, renda_media, se)

unique(populacao_por_estado$UF)

renda_media_por_estado <- renda_media_por_estado |>
  mutate(
    escolaridade = case_when(
      VD3004 == 1 ~ "Sem Instrução",
      VD3004 == 2 ~ "Fundamental Incompleto",
      VD3004 == 3 ~ "Fundamental Completo",
      VD3004 == 4 ~ "Médio Incompleto",
      VD3004 == 5 ~ "Superior Incompleto",
      VD3004 == 6 ~ "Superior Incompleto",
      VD3004 == 7 ~ "Superior Completo"
    )
  ) |>
  select(UF, escolaridade, renda_media, se)

renda_media_por_estado_wider <- renda_media_por_estado |>
  pivot_wider(
    names_from = escolaridade,
    values_from = c(renda_media, se),
    values_fn = mean
  )

populacao_por_estado <- populacao_por_estado |> 
  mutate(ESTADO = case_when(
    UF == "UF11" ~ 11,
    UF == "UF12" ~ 12,  
    UF == "UF13" ~ 13,
    UF == "UF14" ~ 14,
    UF == "UF15" ~ 15,
    UF == "UF16" ~ 16,
    UF == "UF17" ~ 17,
    UF == "UF21" ~ 21,
    UF == "UF22" ~ 22,
    UF == "UF23" ~ 23,
    UF == "UF24" ~ 24,
    UF == "UF25" ~ 25,
    UF == "UF26" ~ 26,
    UF == "UF27" ~ 27,
    UF == "UF28" ~ 28,
    UF == "UF29" ~ 29,
    UF == "UF31" ~ 31,
    UF == "UF32" ~ 32,
    UF == "UF33" ~ 33,
    UF == "UF35" ~ 35,
    UF == "UF41" ~ 41,
    UF == "UF42" ~ 42,
    UF == "UF43" ~ 43,
    UF == "UF50" ~ 50,
    UF == "UF51" ~ 51,
    UF == "UF52" ~ 52,
    UF == "UF53" ~ 53
  ))

# Variáveis de cor e renda
p24 <- update(
  p24,
  cor = ifelse(
    V2010 == 1, "Branca",
    ifelse(V2010 == 3, "Amarela",
           ifelse(V2010 == 2 | V2010 == 4, "Negros", "Outros"))
  )
)

p24 <- update(p24,
              isento       = ifelse(is.na(V403312), NA, ifelse(V403312 <= 5000, 1, 0)),
              sem.desconto = ifelse(is.na(V403312), NA, ifelse(V403312 > 7350, 1, 0)),
              super.rico   = ifelse(is.na(V403312), NA, ifelse(V403312 > 50000, 1, 0))
)

p24 <- update(
  p24,
  sem.instrucao        = ifelse(is.na(VD3004), NA, ifelse(VD3004 == 1, 1, 0)),
  fundamental.incompleto = ifelse(is.na(VD3004), NA, ifelse(VD3004 == 2, 1, 0)),
  fundamental.completo   = ifelse(is.na(VD3004), NA, ifelse(VD3004 == 3, 1, 0)),
  medio.incompleto       = ifelse(is.na(VD3004), NA, ifelse(VD3004 == 4, 1, 0)),
  medio.completo         = ifelse(is.na(VD3004), NA, ifelse(VD3004 == 5, 1, 0)),
  superior.incompleto    = ifelse(is.na(VD3004), NA, ifelse(VD3004 == 6, 1, 0)),
  superior.completo      = ifelse(is.na(VD3004), NA, ifelse(VD3004 == 7, 1, 0))
)

# Isento
isento = svyby(~isento, ~UF, subset(p24,!is.na(V403312)), 
               svymean, na.rm = TRUE, keep.var = TRUE)
isento_cor = svyby(~isento,~cor,subset(p24,!is.na(V403312)), 
               svymean, na.rm = TRUE, keep.var = TRUE)

isento_sem.escolaridade = svyby(~isento,~sem.instrucao,subset(p24,!is.na(VD3004)), 
                            svymean, na.rm = TRUE, keep.var = TRUE)

isento_fundamental.completo = svyby(
  ~isento, ~fundamental.completo,
  subset(p24,!is.na(VD3004)),
  svymean, na.rm = TRUE, keep.var = TRUE
)

isento_medio.incompleto = svyby(
  ~isento, ~medio.incompleto,
  subset(p24,!is.na(VD3004)),
  svymean, na.rm = TRUE, keep.var = TRUE
)

isento_medio.completo = svyby(
  ~isento, ~medio.completo,
  subset(p24,!is.na(VD3004)),
  svymean, na.rm = TRUE, keep.var = TRUE
)

isento_superior.incompleto = svyby(
  ~isento, ~superior.incompleto,
  subset(p24,!is.na(VD3004)),
  svymean, na.rm = TRUE, keep.var = TRUE
)

isento_superior.completo = svyby(
  ~isento, ~superior.completo,
  subset(p24,!is.na(VD3004)),
  svymean, na.rm = TRUE, keep.var = TRUE
)

BR_UF <- geobr::read_state(year = 2020) |>
  rename(UF = code_state) |> 
  mutate(UF = as.integer(UF))

isento <- isento |>
  mutate(UF = as.numeric(gsub("UF", "", UF)))

dados_isento <- BR_UF |>
  full_join(isento, by = "UF") |>
  st_as_sf()

quantile(dados_isento$isento)

dados_isento <- dados_isento |>
  mutate(
    quartil = ntile(isento, 4),
    quartil = factor(
      quartil,
      labels = c("Até 87%", "87%-91%", "91%-93.8%", "Mais de 93.8%")
    )
  )

dados_isento |>
  ggplot() +
  geom_sf(
    aes(fill = quartil),
    color = "#969696",
    size = 0.2,
    show.legend = TRUE
  ) +
  scale_fill_brewer(
    palette = "PuBuGn",
    name = "Percentual de isentos"
  ) +
  labs(
    title = "Distribuição de trabalhadores com renda até R$ 5 mil",
    subtitle = "Percentual de trabalhadores isentos por Unidade da Federação",
    x = "Longitude",
    y = "Latitude",
    caption = "Fonte: PNADc 2024 — IBGE"
  ) +
  annotation_scale(location = "br", width_hint = 0.4) +
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = north_arrow_fancy_orienteering(text_size = 6)
  ) +
  geom_sf_text(
    aes(label = sprintf("%.1f", isento * 100)),
    size = 2.6,
    color = "gray10",
    fontface = "bold"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    panel.background = element_rect(fill = "gray97", color = NA),
    plot.background = element_rect(fill = "gray98", color = NA)
  )

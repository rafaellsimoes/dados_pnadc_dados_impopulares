getwd()
setwd("path_to_file")
library(PNADcIBGE)
library(survey)
library(dplyr)
library(tidyr)
library(viridis)
library(geobr)
library(ggspatial)
library(geobr)
library(ggplot2)
library(sf)
library(ggspatial)
library(survey)

#Baixando os dados da pnad
p24 <- get_pnadc(year =2024, interview = 1, labels = F,
                 savedir = "path_to_file")
save.image("path_to_file\\dados_mapas.RData")
load(file = "path_to_file")
populacao_total <- sum(svytotal(~UF,p24))

populacao_por_estado <- as.data.frame(svytotal(~UF,p24))
head(populacao_por_estado)
str(populacao_por_estado)
populacao_por_estado$UF <- rownames(populacao_por_estado)


populacao_por_estado <- populacao_por_estado[, c("UF", "total", "SE")]
renda_media_escolaridade <- svyby(
  ~V403312, ~VD3004,
  p24, svymean, na.rm = TRUE
)


renda_media_por_estado <- svyby(~V403312,  ~interaction(UF, VD3004),
                                p24,svymean,na.rm = TRUE)

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
  dplyr::mutate(
    escolaridade = case_when(
      VD3004 == 1 ~"Sem Instrução",
      VD3004 == 2 ~"Fundamental Incompleto",
      VD3004 == 3 ~"Fundamental Completo",
      VD3004 == 4 ~"Médio Incompleto",
      VD3004 == 5 ~"Superior Incompleto",
      VD3004 == 6 ~"Superior Incompleto",
      VD3004 == 7 ~"Superior Completo"
    )
  ) |>
  dplyr::select(UF,escolaridade,renda_media,se)


renda_media_por_estado_wider <- renda_media_por_estado |>
  tidyr::pivot_wider(
    names_from = escolaridade,
    values_from = c(renda_media, se),
    values_fn = mean  
  )  
  
populacao_por_estado <- populacao_por_estado |> 
  dplyr::mutate(ESTADO = dplyr::case_when(
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

p24 <- update(
  p24,
  cor = ifelse(
    V2010 == 1, 'Branca',
    ifelse(V2010 == 3, 'Amarela',
           ifelse(V2010 == 2 | V2010 == 4, 'Negros', 'Outros'))
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

#Isento
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
  full_join(isento, by = "UF")

dados_isento = st_as_sf(dados_isento)

quantile(dados_isento$isento)

dados_isento <- dados_isento |>
  dplyr::mutate(
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
  annotation_scale(
    location = "br",
    width_hint = 0.4,
    line_width = 0.5
  ) +
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = north_arrow_fancy_orienteering(text_size = 6)
  ) +
  geom_sf_text(
    data = dados_isento,
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
    legend.key.height = unit(0.8, "cm"),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(
      fill = "gray97",
      color = NA
    ),
    plot.background = element_rect(
      fill = "gray98",
      color = NA
    )
  )


#Mapas temáticos sobre a renda em função da escolaridade

#renda_media1 se refere a renda média das pessoas sem instrução conforme do dicionário de dados da PNADc
#se1 é o desvio padrão para a variávek renda_media1 dessa amostra
renda_media_por_estado_wider
colnames(renda_media_por_estado_wider)
renda_media_por_estado_sem_instrucao <- renda_media_por_estado_wider |>
  dplyr::select(UF,`renda_media_Sem Instrução`,`se_Sem Instrução`) |>
  dplyr::rename(renda_media1 = `renda_media_Sem Instrução`,) |>
  dplyr::rename(se1 = `se_Sem Instrução`) |>
  dplyr::mutate(UF = as.integer(UF))

BR_UF2 <- geobr::read_state(year = 2020)

BR_UF2 <- BR_UF2 |>
  dplyr::rename(UF = code_state) |>
  dplyr::mutate(UF = as.integer(UF))

mapa_renda1 <- dplyr::full_join(renda_media_por_estado_sem_instrucao,
                                BR_UF2,by = c("UF"))
quantile(mapa_renda1$renda_media1)
quantile(renda_media_por_estado_sem_instrucao$renda_media1)

mapa_renda1 <- st_as_sf(mapa_renda1)

mapa_renda1 <- mapa_renda1 |>
  dplyr::mutate(
    quartil = ntile(isento, 4),
    quartil = factor(
      quartil,
      labels = c(
        "Até 1089.96",
        "1089.96 – 1630.94",
        "1630.94 – 2066.45",
        "Mais de 2066.45"
      )
    )
  )

mapa_renda1_plot <- mapa_renda1 |> 
  st_transform(31983)  

mapa_renda1_plot |>
  ggplot() +
  geom_sf(
    aes(fill = quartil),
    color = "#969696",
    size = 0.2,
    show.legend = TRUE
  ) +
  scale_fill_brewer(
    palette = "PuBuGn",
    name = "Renda dos Trabalhadores sem Instrução formal"
  ) +
  labs(
    title = "Distribuição de trabalhadores sem renda instrução formal",
    subtitle = "Renda Média dos trabalhadores sem renda instrução formal por Unidade da Federação",
    x = "Longitude",
    y = "Latitude",
    caption = "Fonte: PNADc 2024 — IBGE"
  ) +
  annotation_scale(
    location = "br",
    width_hint = 0.4,
    line_width = 0.5
  ) +
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = north_arrow_fancy_orienteering(text_size = 6)
  ) +
  geom_sf_text(
    data = mapa_renda1_plot,
    aes(label = sprintf("%.1f", renda_media1)),
    size = 2.6,
    color = "gray10",
    fontface = "bold"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.key.height = unit(0.8, "cm"),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(
      fill = "gray97",
      color = NA
    ),
    plot.background = element_rect(
      fill = "gray98",
      color = NA
    )
  )

#Mapa temático das pessoas com ensino fundamental incompleto por unidade
#renda_media2 se refere a média da renda da população com ensino fundamental incompleto
renda_media2 <- renda_media_por_estado_wider |>
  dplyr::select(UF, `renda_media_Fundamental Incompleto`, `se_Fundamental Incompleto`) |>
  dplyr::rename(renda2 = `renda_media_Fundamental Incompleto`,
                se2 = `se_Fundamental Incompleto`)

BR_UF3 <- geobr::read_state(year = 2020) |>
  dplyr::rename(UF = code_state) |>
  dplyr::mutate(UF = as.integer(UF))

mapa_renda2 <- dplyr::full_join(
  renda_media2,
  BR_UF3,
  by = c("UF")
)

mapa_renda2 <- st_as_sf(mapa_renda2)

mapa_renda2 <- mapa_renda2 |>
  dplyr::mutate(
    quartil = ntile(renda2, 4),
    quartil = factor(
      quartil,
      labels = c(
        "Até 1222.28",
        "1222.28 – 1675.21",
        "1675.21 – 2109.02",
        "Mais de 2109.02"
      )
    )
  )

mapa_renda2_plot <- mapa_renda2 |>
  st_transform(31983)

mapa_renda2_plot |>
  ggplot() +
  geom_sf(
    aes(fill = quartil),
    color = "#969696",
    size = 0.2,
    show.legend = TRUE
  ) +
  scale_fill_brewer(
    palette = "PuBuGn",
    name = "Renda dos Trabalhadores com Ensino Fundamental Incompleto"
  ) +
  labs(
    title = "Distribuição de trabalhadores com Ensino Fundamental Incompleto",
    x = "Longitude",
    y = "Latitude",
    caption = "Fonte: PNADc 2024 — IBGE"
  ) +
  annotation_scale(
    location = "br",
    width_hint = 0.4,
    line_width = 0.5
  ) +
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = north_arrow_fancy_orienteering(text_size = 6)
  ) +
  geom_sf_text(
    data = mapa_renda2_plot,
    aes(label = sprintf("%.1f", renda2)),
    size = 2.6,
    color = "gray10",
    fontface = "bold"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.key.height = unit(0.8, "cm"),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "gray97", color = NA),
    plot.background = element_rect(fill = "gray98", color = NA)
  )

#Mapa temático das pessoas com ensino fundamental completo por unidade
#renda_media3 se refere a média da renda da população com ensino fundamental completo

renda_media_por_estado_wider
colnames(renda_media_por_estado_wider)

renda_media3 <- renda_media_por_estado_wider |>
  dplyr::select(UF,`renda_media_Fundamental Completo`,`se_Fundamental Completo`) |>
  dplyr::rename(renda_media3 = `renda_media_Fundamental Completo`) |>
  dplyr::rename(se3 = `se_Fundamental Completo`)

BR_UF3 <- geobr::read_state(year = 2020)
BR_UF3 <- BR_UF3 |>
  dplyr::rename(UF = code_state) |>
  dplyr::mutate(UF = as.integer(UF))
mapa_renda3 <- dplyr::full_join(
  renda_media3,
  BR_UF3,
  by = c("UF")
)
mapa_renda3 <- st_as_sf(mapa_renda3)
quantile(mapa_renda3$renda_media3)
str(mapa_renda3)

mapa_renda3 <- mapa_renda3 |>
  dplyr::mutate(
    quartil = ntile(renda_media3, 4),
    quartil = factor(
      quartil,
      labels = c(
        "Até 1497.51",
        "1497.51 – 1851.80",
        "1851.80 – 2320.67",
        "Mais de 2320.67"
      )
    )
  )

mapa_renda3_plot <- mapa_renda3 |>
  st_transform(31983)


mapa_renda3_plot |>
  ggplot() +
  geom_sf(
    aes(fill = quartil),
    color = "#969696",
    size = 0.2,
    show.legend = TRUE
  ) +
  scale_fill_brewer(
    palette = "PuBuGn",
    name = "Renda dos Trabalhadores com Ensino Fundamental Completo"
  ) +
  labs(
    title = "Distribuição de trabalhadores com Ensino Fundamental Completo",
    x = "Longitude",
    y = "Latitude",
    caption = "Fonte: PNADc 2024 — IBGE"
  ) +
  annotation_scale(
    location = "br",
    width_hint = 0.4,
    line_width = 0.5
  ) +
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = north_arrow_fancy_orienteering(text_size = 6)
  ) +
  geom_sf_text(
    data = mapa_renda3_plot,
    aes(label = sprintf("%.1f", renda_media3)),
    size = 2.6,
    color = "gray10",
    fontface = "bold"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.key.height = unit(0.8, "cm"),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray30"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "gray97", color = NA),
    plot.background = element_rect(fill = "gray98", color = NA)
  )



#Mapa temático das pessoas com ensino médio incompleto por unidade
#renda_media4 se refere a média da renda da população com ensino médio incompleto
renda_media4 <- renda_media_por_estado_wider |>
  dplyr::select(UF, `renda_media_Médio Incompleto`, `se_Médio Incompleto`) |>
  dplyr::rename(renda_media4 = `renda_media_Médio Incompleto`) |>
  dplyr::rename(se4 = `se_Médio Incompleto`) |>
  dplyr::mutate(UF = as.integer(UF))

BR_UF4 <- geobr::read_state(year = 2020)
BR_UF4 <- BR_UF4 |>
  dplyr::rename(UF = code_state) |>
  dplyr::mutate(UF = as.integer(UF))

mapa_renda4 <- dplyr::full_join(
  renda_media4,
  BR_UF4,
  by = "UF"
)

mapa_renda4 <- mapa_renda4 |>
  dplyr::mutate(
    quartil = ntile(renda_media4, 4),
    quartil = factor(
      quartil,
      labels = c(
        "Até 1423.30",
        "1423.30 – 1660.16",
        "1660.16 – 2166.86",
        "Mais de 2166.86"
      )
    )
  )

mapa_renda4 <- st_as_sf(mapa_renda4)

mapa_renda4_plot <- mapa_renda4 |>
  st_transform(31983)

mapa_renda4_plot |>
  ggplot() +
  geom_sf(
    aes(fill = quartil),
    color = "#969696",
    size = 0.2,
    show.legend = TRUE
  ) +
  scale_fill_brewer(
    palette = "PuBuGn",
    name = "Renda dos Trabalhadores com Ensino Médio Incompleto"
  ) +
  labs(
    x = "Longitude",
    y = "Latitude",
    caption = "Fonte: PNADc 2024 — IBGE"
  ) +
  annotation_scale(
    location = "br",
    width_hint = 0.4,
    line_width = 0.5
  ) +
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = north_arrow_fancy_orienteering(text_size = 6)
  ) +
  geom_sf_text(
    data = mapa_renda4_plot,
    aes(label = sprintf("%.1f", renda_media4)),
    size = 2.6,
    color = "gray10",
    fontface = "bold"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.key.height = unit(0.8, "cm"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "gray97", color = NA),
    plot.background = element_rect(fill = "gray98", color = NA)
  )

head(mapa_renda4)    

#Mapa temático das pessoas com ensino superior incompleto por unidade
#renda_media5 se refere a média da renda da população com ensino superior incompleto

renda_media_por_estado_wider
str(renda_media_por_estado_wider)
renda_media5 <- renda_media_por_estado_wider |>
  dplyr::select(UF, `renda_media_Superior Incompleto`,`se_Superior Incompleto`)|>
  dplyr::rename(renda5 = `renda_media_Superior Incompleto`) |>
  dplyr::rename(se5 = `se_Superior Incompleto`)

BR_UF5 <- geobr::read_state(year = 2020)
BR_UF5 <- BR_UF5 |>
  dplyr::rename(UF = code_state) |>
  dplyr::mutate(UF = as.integer(UF))

mapa_renda5 <- dplyr::full_join(
  renda_media5,
  BR_UF5,
  by = c("UF")
)
quantile(mapa_renda5$renda5)

mapa_renda5 <- mapa_renda5 |>
  dplyr::mutate(
    quartil = ntile(renda5, 4),
    quartil = factor(
      quartil,
      labels = c(
        "Até 2099.17",
        "2099.17 – 2369.65",
        "2369.65 – 2773.43",
        "Mais de 2773.43"
      )
    )
  )

mapa_renda5 <- st_as_sf(mapa_renda5)

mapa_renda5_plot <- mapa_renda5 |>
  st_transform(31983)

mapa_renda5_plot |>
  ggplot() +
  geom_sf(
    aes(fill = quartil),
    color = "#969696",
    size = 0.2,
    show.legend = TRUE
  ) +
  scale_fill_brewer(
    palette = "PuBuGn",
    name = "Renda dos Trabalhadores com Ensino Superior Incompleto"
  ) +
  labs(
    x = "Longitude",
    y = "Latitude",
    caption = "Fonte: PNADc 2024 — IBGE"
  ) +
  annotation_scale(
    location = "br",
    width_hint = 0.4,
    line_width = 0.5
  ) +
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = north_arrow_fancy_orienteering(text_size = 6)
  ) +
  geom_sf_text(
    data = mapa_renda5_plot,
    aes(label = sprintf("%.1f", renda5)),
    size = 2.6,
    color = "gray10",
    fontface = "bold"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid = element_blank(),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.key.height = unit(0.8, "cm"),
    plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "gray97", color = NA),
    plot.background = element_rect(fill = "gray98", color = NA)
  )

#Mapa temático das pessoas com ensino superior completo por unidade
#renda_media6 se refere a média da renda da população com ensino superior incompleto

renda_media_por_estado_wider
colnames(renda_media_por_estado_wider)
renda_media6 <- renda_media_por_estado_wider |>
  dplyr::select(UF,`renda_media_Superior Completo`,
                `se_Superior Completo`) |>
  dplyr::rename(renda_media6 = `renda_media_Superior Completo`) |>
  dplyr::rename(se6 = `se_Superior Completo`)

BR_UF6 <- geobr::read_state(year = 2020)
BR_UF6 <- BR_UF6 |>
  dplyr::rename(UF = code_state) |>
  dplyr::mutate(UF = as.integer(UF))
#####12 20
sem.desconto = svyby(~sem.desconto,~UF,subset(p24,!is.na(V403312)),
                     svymean, na.rm = TRUE, keep.var = TRUE)

sem_desconto_cor = svyby(~sem.desconto, ~cor, subset(p24,!is.na(V403312)),
                          svymean, na.rm = TRUE, keep.var = TRUE) 


sem_desconto_sem.instrucao = svyby(~sem.desconto, ~sem.instrucao,subset(p24,!is.na(VD3004)),
                                   svymean, na.rm=TRUE, keep.var = TRUE)

sem_desconto_fundamental.incompleto = svyby(~sem.desconto, ~fundamental.incompleto, subset(p24,!is.na(VD3004)),
                                            svymean, na.rm = TRUE, keep.var = TRUE)

sem_desconto_fundamental.completo = svyby(~sem.desconto, ~fundamental.completo, subset(p24,!is.na(VD3004)),
                                          svymean, na.rm = TRUE, keep.var = TRUE)

sem.desconto_medio.incompleto = svyby(~sem.desconto, ~medio.incompleto, subset(p24,!is.na(VD3004)),
                                      svymean, na.rm = TRUE, keep.var = TRUE)

sem.desconto_medio.completo = svyby(~sem.desconto, ~medio.completo, subset(p24,!is.na(VD3004)),
                                    svymean, na.rm = TRUE, keep.var = TRUE)

sem.desconto_superior.incompleto = svyby(~sem.desconto, ~superior.incompleto, subset(p24,!is.na(VD3004)),
                                       svymean, na.rm = TRUE, keep.var = TRUE)

sem.desconto_superior.completo = svyby(~sem.desconto, ~superior.completo, subset(p24,!is.na(VD3004)),
                                       svymean, na.rm = TRUE, keep.var = TRUE)


# Super-Ricos

super.rico = svyby(~super.rico,~UF,subset(p24,!is.na(V403312)),
                   svymean, na.rm = TRUE, keep.var = TRUE)
super.rico_cor = svyby(~super.rico, ~cor, subset(p24,!is.na(V403312)),
                   svymean, na.rm = TRUE, keep.var = TRUE) 


super_rico.sem.instrucao = svyby(~super.rico, ~sem.instrucao,subset(p24,!is.na(VD3004)),
                                 svymean, na.rm=TRUE, keep.var = TRUE)

super_rico.fundamental.incompleto = svyby(~super.rico, ~fundamental.incompelto,subset(p24,!is.na(VD3004)),
                                          svymean, na.rm=TRUE, keep.var = TRUE)

super_rico.fundamental.completo = svyby(~super.rico, ~fundamental.completo,subset(p24,!is.na(VD3004)),
                                        svymean, na.rm=TRUE, keep.var = TRUE)

super_rico.medio_incompleto = svyby(~super.rico, ~medio.incompleto,subset(p24,!is.na(VD3004)),
                                    svymean, na.rm=TRUE, keep.var = TRUE)


super_rico.medio_completo = svyby(~super.rico, ~medio.completo,subset(p24,!is.na(VD3004)),
                                  svymean, na.rm=TRUE, keep.var = TRUE)


super_rico.superior_incompleto = svyby(~super.rico, ~superior.incompleto,subset(p24,!is.na(VD3004)),
                                       svymean, na.rm=TRUE, keep.var = TRUE)

super_rico.superior_completo = svyby(~super.rico, ~superior.completo,subset(p24,!is.na(VD3004)),
                                     svymean, na.rm=TRUE, keep.var = TRUE)


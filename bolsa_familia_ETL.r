# =============================================================================
# ANÁLISE DE SÉRIES TEMPORAIS - PROGRAMA BOLSA FAMÍLIA (PBF)
# =============================================================================

# Carregar bibliotecas
library(dplyr)
library(readr)
library(httr)
library(tidyr)
library(ggplot2)
library(readxl)
library(stringr)
library(tidyverse)   
library(readxl)      
library(forecast)    
library(TSA)         
library(trend) 



# Definir diretório de trabalho
setwd("/home/rafaelsimoes/Documentos/topicos_especiais")

# Salvar e carregar workspace
save.image("/home/rafaelsimoes/Documentos/topicos_especiais/dados_ajustados_pbf.RData")
load(file="/home/rafaelsimoes/Documentos/topicos_especiais/dados_ajustados_pbf.RData")

# =============================================================================
# COLETA DE DADOS - 2026
# =============================================================================

url2026 <- "https://aplicacoes.mds.gov.br/sagi/servicos/misocial/?fq=anomes_s:2026*&fl=codigo_ibge%2Canomes_s%2Cqtd_familias_beneficiarias_bolsa_familia_s%2Cvalor_repassado_bolsa_familia_s%2Cpbf_vlr_medio_benef_f&fq=valor_repassado_bolsa_familia_s%3A*&q=*%3A*&rows=100000&sort=anomes_s%20desc%2C%20codigo_ibge%20asc&wt=csv"
resp2026 <- httr::GET(url2026)
dados_pbf_2026 <- readr::read_csv(file = url2026, locale = readr::locale(encoding = "UTF-8"))

# =============================================================================
# COLETA DE DADOS - 2025
# =============================================================================

url2025 <- "https://aplicacoes.mds.gov.br/sagi/servicos/misocial/?fq=anomes_s:2025*&fl=codigo_ibge%2Canomes_s%2Cqtd_familias_beneficiarias_bolsa_familia_s%2Cvalor_repassado_bolsa_familia_s%2Cpbf_vlr_medio_benef_f&fq=valor_repassado_bolsa_familia_s%3A*&q=*%3A*&rows=100000&sort=anomes_s%20desc%2C%20codigo_ibge%20asc&wt=csv"
resp2025 <- httr::GET(url2025)
dados_pbf_2025 <- readr::read_csv(file = url2025, locale = readr::locale(encoding = "UTF-8"))

# Combinar 2026 e 2025
dados_pbf <- dplyr::bind_rows(dados_pbf_2026, dados_pbf_2025)

# =============================================================================
# COLETA DE DADOS - 2024
# =============================================================================

url2024 <- "https://aplicacoes.mds.gov.br/sagi/servicos/misocial/?fq=anomes_s:2024*&fl=codigo_ibge%2Canomes_s%2Cqtd_familias_beneficiarias_bolsa_familia_s%2Cvalor_repassado_bolsa_familia_s%2Cpbf_vlr_medio_benef_f&fq=valor_repassado_bolsa_familia_s%3A*&q=*%3A*&rows=100000&sort=anomes_s%20desc%2C%20codigo_ibge%20asc&wt=csv"
resp2024 <- httr::GET(url2024)
dados_pbf_2024 <- readr::read_csv(file = url2024, locale = readr::locale(encoding = "UTF-8"))

dados_pbf <- dplyr::bind_rows(dados_pbf, dados_pbf_2024)

# =============================================================================
# COLETA DE DADOS - 2023
# =============================================================================

url2023 <- "https://aplicacoes.mds.gov.br/sagi/servicos/misocial/?fq=anomes_s:2023*&fl=codigo_ibge%2Canomes_s%2Cqtd_familias_beneficiarias_bolsa_familia_s%2Cvalor_repassado_bolsa_familia_s%2Cpbf_vlr_medio_benef_f&fq=valor_repassado_bolsa_familia_s%3A*&q=*%3A*&rows=100000&sort=anomes_s%20desc%2C%20codigo_ibge%20asc&wt=csv"
resp2023 <- httr::GET(url2023)
dados_pbf_2023 <- readr::read_csv(file = url2023, locale = readr::locale(encoding = "UTF-8"))

dados_pbf <- dplyr::bind_rows(dados_pbf, dados_pbf_2023)

# =============================================================================
# COLETA DE DADOS - 2021
# =============================================================================

url2021 <- "https://aplicacoes.mds.gov.br/sagi/servicos/misocial?fq=anomes_s:2021*&fq=tipo_s:mes_mu&wt=csv&q=*&fl=ibge:codigo_ibge,anomes:anomes_s,qtd_familias_beneficiarias_bolsa_familia,valor_repassado_bolsa_familia&rows=10000000&sort=anomes_s%20asc,%20codigo_ibge%20asc"
resp2021 <- httr::GET(url2021)
dados_pbf_2021 <- readr::read_csv(file = url2021, locale = readr::locale(encoding = "UTF-8"))

# Renomear colunas para padronização
dados_pbf <- dados_pbf |>
  dplyr::rename(
    ibge = codigo_ibge,
    anomes = anomes_s,
    qtd_familias_beneficiarias_bolsa_familia = qtd_familias_beneficiarias_bolsa_familia_s,
    valor_repassado_bolsa_familia = valor_repassado_bolsa_familia_s,
    pbf_vlr_medio_benef = pbf_vlr_medio_benef_f
  )

# Calcular valor médio para 2021
dados_pbf_2021 <- dados_pbf_2021 |>
  dplyr::mutate(
    pbf_vlr_medio_benef = valor_repassado_bolsa_familia / qtd_familias_beneficiarias_bolsa_familia
  )

# Converter tipos de dados para 2021
dados_pbf_2021 <- dados_pbf_2021 |>
  dplyr::mutate(
    ibge = as.character(ibge),
    anomes = as.character(anomes),
    qtd_familias_beneficiarias_bolsa_familia = as.numeric(qtd_familias_beneficiarias_bolsa_familia),
    valor_repassado_bolsa_familia = as.numeric(valor_repassado_bolsa_familia),
    pbf_vlr_medio_benef = as.numeric(pbf_vlr_medio_benef)
  )

# Converter tipos de dados para dados_pbf
dados_pbf <- dados_pbf |>
  dplyr::mutate(
    ibge = as.character(ibge),
    anomes = as.character(anomes),
    qtd_familias_beneficiarias_bolsa_familia = as.numeric(qtd_familias_beneficiarias_bolsa_familia),
    valor_repassado_bolsa_familia = as.numeric(valor_repassado_bolsa_familia),
    pbf_vlr_medio_benef = as.numeric(pbf_vlr_medio_benef)
  )

# Combinar com 2021
dados_pbf <- dplyr::bind_rows(dados_pbf, dados_pbf_2021)

# =============================================================================
# COLETA DE DADOS - 2020
# =============================================================================

url2020 <- "https://aplicacoes.mds.gov.br/sagi/servicos/misocial?fq=anomes_s:2020*&fq=tipo_s:mes_mu&wt=csv&q=*&fl=ibge:codigo_ibge,anomes:anomes_s,qtd_familias_beneficiarias_bolsa_familia,valor_repassado_bolsa_familia&rows=10000000&sort=anomes_s%20asc,%20codigo_ibge%20asc"
resp2020 <- httr::GET(url2020)

dados_pbf_2020 <- readr::read_csv(
  file = url2020,
  locale = readr::locale(encoding = "UTF-8")
)

colnames(dados_pbf_2020)

dados_pbf_2020 <- dados_pbf_2020 |>
  dplyr::mutate(
    pbf_vlr_medio_benef = valor_repassado_bolsa_familia/qtd_familias_beneficiarias_bolsa_familia
  )

dados_pbf_2020 <- dados_pbf_2020 |>
  dplyr::mutate(
    ibge = as.character(ibge),
    anomes = as.character(anomes),
    qtd_familias_beneficiarias_bolsa_familia = as.numeric(qtd_familias_beneficiarias_bolsa_familia),
    valor_repassado_bolsa_familia = as.numeric(valor_repassado_bolsa_familia),
    pbf_vlr_medio_benef = as.numeric(pbf_vlr_medio_benef)
  )

dados_pbf <- dplyr::bind_rows(dados_pbf, dados_pbf_2020)

# =============================================================================
# COLETA DE DADOS - 2019
# =============================================================================

url2019 <- "https://aplicacoes.mds.gov.br/sagi/servicos/misocial?fq=anomes_s:2019*&fq=tipo_s:mes_mu&wt=csv&q=*&fl=ibge:codigo_ibge,anomes:anomes_s,qtd_familias_beneficiarias_bolsa_familia,valor_repassado_bolsa_familia&rows=10000000&sort=anomes_s%20asc,%20codigo_ibge%20asc"
resp2019 <- httr::GET(url2019)

dados_pbf_2019 <- readr::read_csv(
  file = url2019,
  locale = readr::locale(encoding = "UTF-8")
)

dados_pbf_2019 <- dados_pbf_2019 |>
  dplyr::mutate(
    pbf_vlr_medio_benef = valor_repassado_bolsa_familia / qtd_familias_beneficiarias_bolsa_familia
  )

dados_pbf_2019 <- dados_pbf_2019 |>
  dplyr::mutate(
    ibge = as.character(ibge),
    anomes = as.character(anomes),
    qtd_familias_beneficiarias_bolsa_familia = as.numeric(qtd_familias_beneficiarias_bolsa_familia),
    valor_repassado_bolsa_familia = as.numeric(valor_repassado_bolsa_familia),
    pbf_vlr_medio_benef = as.numeric(pbf_vlr_medio_benef)
  )

dados_pbf <- dplyr::bind_rows(dados_pbf,dados_pbf_2019)

# =============================================================================
# COLETA DE DADOS - 2018
# =============================================================================

url2018 <- "https://aplicacoes.mds.gov.br/sagi/servicos/misocial?fq=anomes_s:2018*&fq=tipo_s:mes_mu&wt=csv&q=*&fl=ibge:codigo_ibge,anomes:anomes_s,qtd_familias_beneficiarias_bolsa_familia,valor_repassado_bolsa_familia&rows=10000000&sort=anomes_s%20asc,%20codigo_ibge%20asc"
resp2018 <- httr::GET(url2018)

dados_pbf_2018 <- readr::read_csv(
  file = url2018,
  locale = readr::locale(encoding = "UTF-8")
)

dados_pbf_2018 <- dados_pbf_2018 |>
  dplyr::mutate(
    pbf_vlr_medio_benef = valor_repassado_bolsa_familia / qtd_familias_beneficiarias_bolsa_familia
  )

dados_pbf_2018 <- dados_pbf_2018 |>
  dplyr::mutate(
    ibge = as.character(ibge),
    anomes = as.character(anomes),
    qtd_familias_beneficiarias_bolsa_familia = as.numeric(qtd_familias_beneficiarias_bolsa_familia),
    valor_repassado_bolsa_familia = as.numeric(valor_repassado_bolsa_familia),
    pbf_vlr_medio_benef = as.numeric(pbf_vlr_medio_benef)
  )

dados_pbf <- dplyr::bind_rows(dados_pbf,dados_pbf_2018)

# =============================================================================
# COLETA DE DADOS - 2017
# =============================================================================

url2017 <- "https://aplicacoes.mds.gov.br/sagi/servicos/misocial?fq=anomes_s:2017*&fq=tipo_s:mes_mu&wt=csv&q=*&fl=ibge:codigo_ibge,anomes:anomes_s,qtd_familias_beneficiarias_bolsa_familia,valor_repassado_bolsa_familia&rows=10000000&sort=anomes_s%20asc,%20codigo_ibge%20asc"
resp2017 <- httr::GET(url2017)

dados_pbf_2017 <- readr::read_csv(
  file = url2017,
  locale = readr::locale(encoding = "UTF-8")
)

dados_pbf_2017 <- dados_pbf_2017 |>
  dplyr::mutate(
    pbf_vlr_medio_benef = valor_repassado_bolsa_familia / qtd_familias_beneficiarias_bolsa_familia
  )

dados_pbf_2017 <- dados_pbf_2017 |>
  dplyr::mutate(
    ibge = as.character(ibge),
    anomes = as.character(anomes),
    qtd_familias_beneficiarias_bolsa_familia = as.numeric(qtd_familias_beneficiarias_bolsa_familia),
    valor_repassado_bolsa_familia = as.numeric(valor_repassado_bolsa_familia),
    pbf_vlr_medio_benef = as.numeric(pbf_vlr_medio_benef)
  )

dados_pbf <- dplyr::bind_rows(dados_pbf, dados_pbf_2017)

# =============================================================================
# COLETA DE DADOS - 2016
# =============================================================================

url2016 <- "https://aplicacoes.mds.gov.br/sagi/servicos/misocial?fq=anomes_s:2016*&fq=tipo_s:mes_mu&wt=csv&q=*&fl=ibge:codigo_ibge,anomes:anomes_s,qtd_familias_beneficiarias_bolsa_familia,valor_repassado_bolsa_familia&rows=10000000&sort=anomes_s%20asc,%20codigo_ibge%20asc"
resp2016 <- httr::GET(url2016)

dados_pbf_2016 <- readr::read_csv(
  file = url2016,
  locale = readr::locale(encoding = "UTF-8")
)

dados_pbf_2016 <- dados_pbf_2016 |>
  dplyr::mutate(
    pbf_vlr_medio_benef = valor_repassado_bolsa_familia / qtd_familias_beneficiarias_bolsa_familia
  )

dados_pbf_2016 <- dados_pbf_2016 |>
  dplyr::mutate(
    ibge = as.character(ibge),
    anomes = as.character(anomes),
    qtd_familias_beneficiarias_bolsa_familia = as.numeric(qtd_familias_beneficiarias_bolsa_familia),
    valor_repassado_bolsa_familia = as.numeric(valor_repassado_bolsa_familia),
    pbf_vlr_medio_benef = as.numeric(pbf_vlr_medio_benef)
  )

dados_pbf <- dplyr::bind_rows(dados_pbf,dados_pbf_2016)

# =============================================================================
# COLETA DE DADOS - 2015
# =============================================================================

url2015 <- "https://aplicacoes.mds.gov.br/sagi/servicos/misocial?fq=anomes_s:2015*&fq=tipo_s:mes_mu&wt=csv&q=*&fl=ibge:codigo_ibge,anomes:anomes_s,qtd_familias_beneficiarias_bolsa_familia,valor_repassado_bolsa_familia&rows=10000000&sort=anomes_s%20asc,%20codigo_ibge%20asc"

dados_pbf_2015 <- readr::read_csv(
  file = url2015,
  locale = readr::locale(encoding = "UTF-8")
)

dados_pbf_2015 <- dados_pbf_2015 |>
  dplyr::mutate(
    pbf_vlr_medio_benef = valor_repassado_bolsa_familia / qtd_familias_beneficiarias_bolsa_familia
  )

dados_pbf_2015 <- dados_pbf_2015 |>
  dplyr::mutate(
    ibge = as.character(ibge),
    anomes = as.character(anomes),
    qtd_familias_beneficiarias_bolsa_familia = as.numeric(qtd_familias_beneficiarias_bolsa_familia),
    valor_repassado_bolsa_familia = as.numeric(valor_repassado_bolsa_familia),
    pbf_vlr_medio_benef = as.numeric(pbf_vlr_medio_benef)
  )

dados_pbf <- dplyr::bind_rows(dados_pbf,dados_pbf_2015)

# =============================================================================
# COLETA DE DADOS - 2014
# =============================================================================

url2014 <- "https://aplicacoes.mds.gov.br/sagi/servicos/misocial?fq=anomes_s:2014*&fq=tipo_s:mes_mu&wt=csv&q=*&fl=ibge:codigo_ibge,anomes:anomes_s,qtd_familias_beneficiarias_bolsa_familia,valor_repassado_bolsa_familia&rows=10000000&sort=anomes_s%20asc,%20codigo_ibge%20asc"
resp2014 <- httr::GET(url2014)

dados_pbf_2014 <- readr::read_csv(
  file = url2014,
  locale = readr::locale(encoding = "UTF-8")
)

dados_pbf_2014 <- dados_pbf_2014 |>
  dplyr::mutate(
    pbf_vlr_medio_benef = valor_repassado_bolsa_familia/qtd_familias_beneficiarias_bolsa_familia
  )

dados_pbf_2014 <- dados_pbf_2014 |>
  dplyr::mutate(
    ibge = as.character(ibge),
    anomes = as.character(anomes),
    qtd_familias_beneficiarias_bolsa_familia = as.numeric(qtd_familias_beneficiarias_bolsa_familia),
    valor_repassado_bolsa_familia = as.numeric(valor_repassado_bolsa_familia),
    pbf_vlr_medio_benef = as.numeric(pbf_vlr_medio_benef)
  )

dados_pbf <- dplyr::bind_rows(dados_pbf,dados_pbf_2014) 

# =============================================================================
# COLETA DE DADOS - 2013
# =============================================================================

url2013 <- "https://aplicacoes.mds.gov.br/sagi/servicos/misocial?fq=anomes_s:2013*&fq=tipo_s:mes_mu&wt=csv&q=*&fl=ibge:codigo_ibge,anomes:anomes_s,qtd_familias_beneficiarias_bolsa_familia,valor_repassado_bolsa_familia&rows=10000000&sort=anomes_s%20asc,%20codigo_ibge%20asc"
resp2013 <- httr::GET(url2013)

dados_pbf_2013 <- readr::read_csv(
  file = url2013,
  locale = readr::locale(encoding = "UTF-8")
)

dados_pbf_2013 <- dados_pbf_2013 |>
  dplyr::mutate(
    pbf_vlr_medio_benef = valor_repassado_bolsa_familia / qtd_familias_beneficiarias_bolsa_familia
  )

dados_pbf_2013 <- dados_pbf_2013 |>
  dplyr::mutate(
    ibge = as.character(ibge),
    anomes = as.character(anomes),
    qtd_familias_beneficiarias_bolsa_familia = as.numeric(qtd_familias_beneficiarias_bolsa_familia),
    valor_repassado_bolsa_familia = as.numeric(valor_repassado_bolsa_familia),
    pbf_vlr_medio_benef = as.numeric(pbf_vlr_medio_benef)
  )

dados_pbf <- dplyr::bind_rows(dados_pbf, dados_pbf_2013)

# =============================================================================
# COLETA DE DADOS - 2012
# =============================================================================

url2012 <- "https://aplicacoes.mds.gov.br/sagi/servicos/misocial?fq=anomes_s:2012*&fq=tipo_s:mes_mu&wt=csv&q=*&fl=ibge:codigo_ibge,anomes:anomes_s,qtd_familias_beneficiarias_bolsa_familia,valor_repassado_bolsa_familia&rows=10000000&sort=anomes_s%20asc,%20codigo_ibge%20asc"
resp2012 <- httr::GET(url2012)

dados_pbf_2012 <- readr::read_csv(
  file = url2012,
  locale = readr::locale(encoding = "UTF-8")
)

dados_pbf_2012 <- dados_pbf_2012 |>
  dplyr::mutate(
    pbf_vlr_medio_benef = valor_repassado_bolsa_familia / qtd_familias_beneficiarias_bolsa_familia
  )

dados_pbf_2012 <- dados_pbf_2012 |>
  dplyr::mutate(
    ibge = as.character(ibge),
    anomes = as.character(anomes),
    qtd_familias_beneficiarias_bolsa_familia = as.numeric(qtd_familias_beneficiarias_bolsa_familia),
    valor_repassado_bolsa_familia = as.numeric(valor_repassado_bolsa_familia),
    pbf_vlr_medio_benef = as.numeric(pbf_vlr_medio_benef)
  )

dados_pbf <- dplyr::bind_rows(dados_pbf,dados_pbf_2012)

# =============================================================================
# COLETA DE DADOS - 2011
# =============================================================================

url2011 <- "https://aplicacoes.mds.gov.br/sagi/servicos/misocial?fq=anomes_s:2011*&fq=tipo_s:mes_mu&wt=csv&q=*&fl=ibge:codigo_ibge,anomes:anomes_s,qtd_familias_beneficiarias_bolsa_familia,valor_repassado_bolsa_familia&rows=10000000&sort=anomes_s%20asc,%20codigo_ibge%20asc"
resp2011 <- httr::GET(url2011)

dados_pbf_2011 <- readr::read_csv(
  file = url2011,w
  locale = readr::locale(encoding = "UTF-8")
)

dados_pbf_2011 <- dados_pbf_2011 |>
  dplyr::mutate(
    pbf_vlr_medio_benef = valor_repassado_bolsa_familia/qtd_familias_beneficiarias_bolsa_familia
  )

dados_pbf_2011 <- dados_pbf_2011 |>
  dplyr::mutate(
    ibge = as.character(ibge),
    anomes = as.character(anomes),
    qtd_familias_beneficiarias_bolsa_familia = as.numeric(qtd_familias_beneficiarias_bolsa_familia),
    valor_repassado_bolsa_familia = as.numeric(valor_repassado_bolsa_familia),
    pbf_vlr_medio_benef = as.numeric(pbf_vlr_medio_benef)
  )

dados_pbf <- dplyr::bind_rows(dados_pbf,dados_pbf_2011)

# =============================================================================
# COLETA DE DADOS - 2010
# =============================================================================

url2010 <- "https://aplicacoes.mds.gov.br/sagi/servicos/misocial?fq=anomes_s:2010*&fq=tipo_s:mes_mu&wt=csv&q=*&fl=ibge:codigo_ibge,anomes:anomes_s,qtd_familias_beneficiarias_bolsa_familia,valor_repassado_bolsa_familia&rows=10000000&sort=anomes_s%20asc,%20codigo_ibge%20asc"
resp2010 <- httr::GET(url2010)

dados_pbf_2010 <- readr::read_csv(
  file = url2010,
  locale = readr::locale(encoding = "UTF-8")
)

dados_pbf_2010

dados_pbf_2010 <- dados_pbf_2010 |>
  dplyr::mutate(
    pbf_vlr_medio_benef = valor_repassado_bolsa_familia / qtd_familias_beneficiarias_bolsa_familia
  )

dados_pbf_2010 <- dados_pbf_2010 |>
  dplyr::mutate(
    ibge = as.character(ibge),
    anomes = as.character(anomes),
    qtd_familias_beneficiarias_bolsa_familia = as.numeric(qtd_familias_beneficiarias_bolsa_familia),
    valor_repassado_bolsa_familia = as.numeric(valor_repassado_bolsa_familia),
    pbf_vlr_medio_benef = as.numeric(pbf_vlr_medio_benef)
  )

dados_pbf_2010
dados_pbf <- dplyr::bind_rows(dados_pbf,dados_pbf_2010)

dados_pbf

# =============================================================================
# COLETA DE DADOS - 2009
# =============================================================================

url2009 <- "https://aplicacoes.mds.gov.br/sagi/servicos/misocial?fq=anomes_s:2009*&fq=tipo_s:mes_mu&wt=csv&q=*&fl=ibge:codigo_ibge,anomes:anomes_s,qtd_familias_beneficiarias_bolsa_familia,valor_repassado_bolsa_familia&rows=10000000&sort=anomes_s%20asc,%20codigo_ibge%20asc"
resp2009 <- httr::GET(url2009)

dados_pbf_2009 <- readr::read_csv(
  file = url2009,
  locale = readr::locale(encoding = "UTF-8")
)

dados_pbf_2009

dados_pbf_2009 <- dados_pbf_2009 |>
  dplyr::mutate(
    pbf_vlr_medio_benef = valor_repassado_bolsa_familia/qtd_familias_beneficiarias_bolsa_familia
  )

dados_pbf_2009 <- dados_pbf_2009 |>
  dplyr::mutate(
    ibge = as.character(ibge),
    anomes = as.character(anomes),
    qtd_familias_beneficiarias_bolsa_familia = as.numeric(qtd_familias_beneficiarias_bolsa_familia),
    valor_repassado_bolsa_familia = as.numeric(valor_repassado_bolsa_familia),
    pbf_vlr_medio_benef = as.numeric(pbf_vlr_medio_benef)
  )

dados_pbf <- dplyr::bind_rows(dados_pbf,dados_pbf_2009)
dados_pbf

# =============================================================================
# SALVAR DADOS COMPLETOS
# =============================================================================

write.csv2(
  dados_pbf,
  file="dados_pbf.csv",
  row.names = FALSE
)

colnames(dados_pbf)

# =============================================================================
# PREPARAÇÃO DOS DADOS PARA ANÁLISE - MATO GROSSO
# =============================================================================

# Extrair código da UF
dados_pbf <- dados_pbf |>
  dplyr::mutate(
    cod_ibge_uf = stringr::str_sub(ibge,start = 1, end = 2)
  )

head(dados_pbf)
colnames(dados_pbf)

# Selecionar colunas relevantes
dados_pbf <- dados_pbf |>
  dplyr::select(
    cod_ibge_uf, ibge,anomes, qtd_familias_beneficiarias_bolsa_familia,
    valor_repassado_bolsa_familia, pbf_vlr_medio_benef
  )

dados_pbf

# Converter tipos
dados_pbf <- dados_pbf |>
  dplyr::mutate(
    cod_ibge_uf = as.integer(cod_ibge_uf)
  ) |>
  dplyr::mutate(
    ibge = as.integer(ibge)
  )

head(dados_pbf)

# Filtrar Mato Grosso (UF = 51)
dados_pbf_mt <- dados_pbf |>
  dplyr::filter(
    cod_ibge_uf == 51
  )

# Extrair ano e mês
dados_pbf_mt <- dados_pbf_mt |>
  dplyr::mutate(
    ano = stringr::str_sub(anomes,start = 1,end = 4)
  )

dados_pbf_mt <- dados_pbf_mt |>
  dplyr::mutate(
    mes = stringr::str_sub(anomes,start = 5, end = 6)
  )

colnames(dados_pbf_mt)

# Converter ano e mês para inteiro
dados_pbf_mt <- dados_pbf_mt |>
  dplyr::mutate(
    ano = as.integer(ano)
  ) |>
  dplyr::mutate(
    mes = as.integer(mes)
  )

dados_pbf_mt
colnames(dados_pbf_mt)

# Selecionar colunas finais
dados_pbf_mt <- dados_pbf_mt |>
  dplyr::select(
    ano,mes,cod_ibge_uf,ibge,qtd_familias_beneficiarias_bolsa_familia,
    valor_repassado_bolsa_familia,pbf_vlr_medio_benef
  )

dados_pbf_mt

# Salvar dados do MT
write_csv2(
  dados_pbf_mt,
  file = "dados_pbf_mt.csv"
)

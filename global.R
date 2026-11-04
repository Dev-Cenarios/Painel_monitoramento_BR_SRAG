# ==============================================================================
# Gripe Brasil | global.R
# Ambiente, dados (Parquets agregados do SIVEP-Gripe), dicionario geografico,
# agentes etiologicos, nowcast, Rt e helpers. Executado UMA vez,
# antes de ui.R e server.R.
#
# Irmao nacional do gripe_ride (estrutura do chik_br). Mesmos graficos, recorte Brasil -> UF ->
# municipio. O que muda por causa da escala (5.570 municipios):
#
#   - DADOS (em memoria) e a base por UF: semana x UF x classificacao x faixa.
#     Brasil e estados saem dela com um dplyr, como no painel da RIDE.
#   - A base municipal (~1 mi de linhas) fica no disco (DS_MUN, arrow
#     Dataset sobre um Parquet ordenado por UF/municipio) e e lida sob
#     demanda: so o municipio em foco, ou a UF inteira quando o mapa, os
#     quadrantes e o ranking precisam dos municipios dela. Cada leitura fica
#     em cache na memoria do processo.
#   - "Unidades" do recorte: no Brasil o mapa, os quadrantes e o ranking
#     mostram as 27 UFs; num estado (ou municipio), os municipios da UF.
#   - Nowcast com atraso de digitacao POR UF (a digitacao varia muito entre
#     estados); UF com pouca base usa o atraso nacional.
#   - Sem a quebra por Regiao Administrativa do DF do gripe_ride: no Brasil o
#     DF e um municipio, como nos outros paineis nacionais.
# ==============================================================================

library(shiny)
library(bslib)
library(dplyr)
library(echarts4r)
library(DT)

if (dir.exists("app_cache") && file.access("app_cache", 2) == 0) {
  options(sass.cache = sass::sass_file_cache(file.path("app_cache", "sass")))
}

PAINEL_NOME <- "Gripe Brasil"

# ------------------------------------------------------------ fonte de dados --
arq_dado <- function(sufixo) {
  Sys.getenv(paste0("GRIPE_BR_", toupper(sufixo)), file.path("data", sprintf("srag_br_%s.parquet", sufixo)))
}
PARQUET_UF     <- arq_dado("uf")
PARQUET_MUN    <- arq_dado("mun")
PARQUET_ATRASO <- arq_dado("atraso")
PARQUET_POP    <- arq_dado("pop")

for (.p in c(PARQUET_UF, PARQUET_MUN, PARQUET_ATRASO, PARQUET_POP)) {
  if (!file.exists(.p)) {
    stop("Parquet nao encontrado em '", .p, "'. Rode `Rscript etl_gripe_br.R` ",
         "(precisa do .Renviron com PG_*).", call. = FALSE)
  }
}
rm(.p)

# Segunda medida do mapa e do ranking (coluna `extra` de casos_por_unidade).
MEDIDA_SECUNDARIA <- "obitos"
ROTULO_SECUNDARIA <- "Óbitos"

MEDIDAS <- c("casos", "obitos", "uti", "vacinado_gripe", "coinfeccao")

#' Padroniza uma base lida do Parquet (UF ou municipal).
preparar_base <- function(d) {
  d <- as.data.frame(d)
  d$data_ini_se <- as.Date(d$data_ini_se)
  d$se     <- as.integer(d$se)
  d$ano    <- d$se %/% 100L
  d$semana <- d$se %%  100L
  d$uf_ibge <- as.integer(d$uf_ibge)
  if ("municipio_geocodigo" %in% names(d)) d$municipio_geocodigo <- as.integer(d$municipio_geocodigo)
  for (m in intersect(MEDIDAS, names(d))) d[[m]] <- dplyr::coalesce(as.numeric(d[[m]]), 0)
  d
}

DADOS  <- preparar_base(arrow::read_parquet(PARQUET_UF))
DS_MUN <- arrow::open_dataset(PARQUET_MUN)

ATRASO <- arrow::read_parquet(PARQUET_ATRASO) |>
  as.data.frame() |>
  dplyr::mutate(data_ini_se = as.Date(data_ini_se), uf_ibge = as.integer(uf_ibge))

ROTULO_BASE <- "Brasil"

message(sprintf("[%s] Fonte: %s (%s casos de SRAG; %s linhas por UF em memória).",
                PAINEL_NOME, PARQUET_UF, format(sum(DADOS$casos), big.mark = ".", decimal.mark = ","),
                format(nrow(DADOS), big.mark = ".", decimal.mark = ",")))

# ------------------------------------------------- dicionario geografico -----
UF_LOOKUP <- data.frame(
  uf_ibge  = c(11L, 12L, 13L, 14L, 15L, 16L, 17L, 21L, 22L, 23L, 24L, 25L, 26L, 27L,
               28L, 29L, 31L, 32L, 33L, 35L, 41L, 42L, 43L, 50L, 51L, 52L, 53L),
  uf_sigla = c("RO", "AC", "AM", "RR", "PA", "AP", "TO", "MA", "PI", "CE", "RN", "PB", "PE", "AL",
               "SE", "BA", "MG", "ES", "RJ", "SP", "PR", "SC", "RS", "MS", "MT", "GO", "DF"),
  uf_nome  = c("Rondônia", "Acre", "Amazonas", "Roraima", "Pará", "Amapá", "Tocantins",
               "Maranhão", "Piauí", "Ceará", "Rio Grande do Norte", "Paraíba", "Pernambuco",
               "Alagoas", "Sergipe", "Bahia", "Minas Gerais", "Espírito Santo", "Rio de Janeiro",
               "São Paulo", "Paraná", "Santa Catarina", "Rio Grande do Sul", "Mato Grosso do Sul",
               "Mato Grosso", "Goiás", "Distrito Federal"),
  regiao   = c(rep("Norte", 7), rep("Nordeste", 9), rep("Sudeste", 4), rep("Sul", 3),
               rep("Centro-Oeste", 4)),
  stringsAsFactors = FALSE
)

# Populacao por municipio e ano (todos os municipios do IBGE, inclusive os que
# nunca notificaram - o ETL ja completa os anos sem estimativa).
POP_MUN_ANO <- arrow::read_parquet(PARQUET_POP) |>
  as.data.frame() |>
  dplyr::mutate(municipio_geocodigo = as.integer(municipio_geocodigo),
                uf_ibge = as.integer(uf_ibge), ano = as.integer(ano), pop = as.numeric(pop))

GEO_DICT <- POP_MUN_ANO |>
  dplyr::distinct(uf_ibge, municipio_geocodigo, municipio_nome) |>
  # O InfoDengue traz alguns nomes em minuscula ("dourados"): 1a letra maiuscula.
  dplyr::mutate(municipio_nome = sub("^(.)", "\\U\\1", municipio_nome, perl = TRUE)) |>
  dplyr::left_join(UF_LOOKUP, by = "uf_ibge") |>
  dplyr::arrange(uf_nome, municipio_nome)

POP_MUN_ANO <- POP_MUN_ANO |> dplyr::select(municipio_geocodigo, uf_ibge, ano, pop)

POP_UF_ANO <- POP_MUN_ANO |>
  dplyr::group_by(uf_ibge, ano) |>
  dplyr::summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop")

UF_DISPONIVEIS <- UF_LOOKUP |> dplyr::arrange(uf_nome)

# ------------------------------------------------- cobertura temporal --------
DATA_MIN <- min(DADOS$data_ini_se)
DATA_MAX <- max(DADOS$data_ini_se)
ANO_MIN  <- min(DADOS$ano)
ANO_MAX  <- max(DADOS$ano)

# ------------------------------------------------------ agentes etiologicos --
# Ordem = ordem de empilhamento nos graficos (de baixo para cima). Cores:
# familia violeta para influenza (o "dado" deste painel), vinho para covid,
# verde-agua para VSR, verdes/terrosos para os demais virus respiratorios e
# neutros para os casos sem agente definido - assim o que NAO tem
# diagnostico nunca compete visualmente com o que tem.
#
# Os "outros virus" (CLASSI_FIN = 2 sem VSR) vem quebrados pelo ETL, com
# hierarquia para coinfeccao (ver etl_gripe_ride.R). `grupo` agrega o que o
# KPI de agente predominante trata como um so (as tres influenzas).
AGENTES <- data.frame(
  agente = c("influenza_a", "influenza_b", "influenza_nd", "vsr", "covid",
             "rinovirus", "metapneumovirus", "adenovirus", "parainfluenza", "outros_virus",
             "outro_agente", "nao_especificado", "em_investigacao"),
  rotulo = c("Influenza A", "Influenza B", "Influenza (tipo n/i)", "VSR", "Covid-19",
             "Rinovírus", "Metapneumovírus", "Adenovírus", "Parainfluenza", "Outros vírus resp.",
             "Outro agente", "Não especificada", "Em investigação"),
  grupo  = c("influenza", "influenza", "influenza", "vsr", "covid",
             "rinovirus", "metapneumovirus", "adenovirus", "parainfluenza", "outros_virus",
             "outro", "nao_especificado", "em_investigacao"),
  cor    = c("#5B3F9E", "#9A80D0", "#7C66B8", "#1B9AAA", "#8E3B46",
             "#5A9E2E", "#B08A1E", "#2E5E8C", "#A3C962", "#7E9C8C",
             "#8C6D4F", "#B8C4CC", "#DDE5EA"),
  stringsAsFactors = FALSE
)
COR_AGENTE    <- stats::setNames(AGENTES$cor, AGENTES$agente)
ROTULO_AGENTE <- stats::setNames(AGENTES$rotulo, AGENTES$agente)
ROTULO_GRUPO  <- c(influenza = "Influenza", vsr = "VSR", covid = "Covid-19",
                   rinovirus = "Rinovírus", metapneumovirus = "Metapneumovírus",
                   adenovirus = "Adenovírus", parainfluenza = "Parainfluenza",
                   outros_virus = "Outros vírus", outro = "Outro agente")

# Virus respiratorios que nao sao influenza, VSR nem covid (CLASSI_FIN = 2).
OUTROS_VIRUS <- c("rinovirus", "metapneumovirus", "adenovirus", "parainfluenza", "outros_virus")

# Opcoes do seletor "Agente" (rotulo -> chave). Cada chave expande para um
# conjunto de agentes em agentes_do_filtro().
FILTROS_AGENTE <- c(
  "Toda SRAG"               = "todos",
  "Influenza (A + B)"       = "influenza",
  "Influenza A"             = "influenza_a",
  "Influenza B"             = "influenza_b",
  "VSR"                     = "vsr",
  "Covid-19"                = "covid",
  "Outros vírus (todos)"    = "outros_virus_todos",
  "Rinovírus"               = "rinovirus",
  "Metapneumovírus"         = "metapneumovirus",
  "Adenovírus"              = "adenovirus",
  "Parainfluenza"           = "parainfluenza",
  "SRAG não especificada"   = "nao_especificado"
)

agentes_do_filtro <- function(chave) {
  switch(
    chave,
    todos              = AGENTES$agente,
    influenza          = c("influenza_a", "influenza_b", "influenza_nd"),
    outros_virus_todos = OUTROS_VIRUS,
    chave
  )
}

rotulo_filtro_agente <- function(chave) names(FILTROS_AGENTE)[FILTROS_AGENTE == chave]

FAIXAS <- c("0-1", "2-4", "5-14", "15-49", "50-64", "65+")
ROTULO_FAIXA <- c(`0-1` = "< 2 anos", `2-4` = "2–4", `5-14` = "5–14",
                  `15-49` = "15–49", `50-64` = "50–64", `65+` = "65+", ign = "Ignorada")

# ------------------------------------------------------------------ paleta --
# Mesma interface dos paineis de dengue (azul Cenarios, Poppins/DM Sans). A
# rampa de magnitude troca a terracota da dengue por um violeta, cor desta
# doenca no painel - ColorBrewer BuPu, sequencial e segura para daltonismo.
TONS_AZUL <- c(
  `50`  = "#ECF7FE", `100` = "#CFE9F9", `200` = "#A8D5F1", `300` = "#7ABEE6",
  `400` = "#4FA4D3", `500` = "#3891C0", `600` = "#0E7BA8", `700` = "#00638C",
  `800` = "#004565", `900` = "#00324C"
)

TONS_GRIPE <- c(
  `50`  = "#F4F1FA", `100` = "#E3DCF2", `200` = "#C9BCE6", `300` = "#AC98D8",
  `400` = "#8F76C8", `500` = "#7459B4", `600` = "#5B3F9E", `700` = "#472E83",
  `800` = "#352066", `900` = "#24154A"
)

PALETA <- c(
  fundo       = "#F2F6F9",
  superficie  = "#FFFFFF",
  linha       = "#D8E3EA",
  tinta       = "#12222B",
  texto       = "#4A4A4A",
  sub         = "#7A8B95",
  neutro      = "#B8C4CC",
  azul        = TONS_AZUL[["500"]],
  azul_acao   = TONS_AZUL[["600"]],
  azul_titulo = TONS_AZUL[["700"]],
  gripe       = TONS_GRIPE[["600"]],
  gripe_forte = TONS_GRIPE[["800"]],
  nowcast     = "#E03131",
  ok          = "#17895B",
  atencao     = "#DCA744",
  grave       = "#C65D26",
  critico     = "#8F2639"
)

ESCALA_MAPA <- c("#EDF8FB", "#BFD3E6", "#9EBCDA", "#8C96C6", "#8856A7", "#810F7C")

FONTE_GRAFICO <- "'DM Sans', system-ui, sans-serif"

cor_alpha <- function(hex, alpha) {
  rgb <- grDevices::col2rgb(hex)[, 1]
  sprintf("rgba(%d,%d,%d,%s)", rgb[1], rgb[2], rgb[3], alpha)
}

# ------------------------------------------------------------------- malhas --
# GeoJSON do IBGE simplificados (os mesmos do dengue_ride): BR_UF para a
# visao Brasil e MUN_<uf> para a visao de um estado.
#
# DESEMPENHO: a malha vai para o navegador como TEXTO JSON cru, marcado com
# htmlwidgets::JS() (o htmlwidgets avalia o texto no cliente e o
# echarts.registerMap recebe o objeto). Passar a malha como lista R obrigava o
# jsonlite a serializar milhares de coordenadas a cada renderizacao do mapa:
# ~11-14 s so para Minas Gerais. O texto de cada UF e lido uma vez e fica em
# cache.
GEO_DIR <- file.path("data", "geo")

ler_texto <- function(arq) paste(readLines(arq, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

MALHA_UF_TXT <- ler_texto(file.path(GEO_DIR, "BR_UF.geojson"))
MALHA_UF     <- htmlwidgets::JS(MALHA_UF_TXT)
# Feicoes das UFs como lista R, so para extrair o contorno de cada uma.
.FEICOES_UF  <- jsonlite::fromJSON(MALHA_UF_TXT, simplifyVector = FALSE)$features

.cache_malha <- new.env(parent = emptyenv())

#' Malha dos municipios de uma UF, com o contorno da UF anexado ao FIM
#' (codarea "CONTORNO"): desenhado por ultimo, por cima dos municipios, e
#' por ser parte do mesmo mapa acompanha zoom e arraste. O contorno (uma
#' feicao so) e serializado em R; os municipios seguem como texto cru e a
#' juncao acontece no navegador.
malha_municipios <- function(uf) {
  chave <- as.character(uf)
  if (!is.null(.cache_malha[[chave]])) return(.cache_malha[[chave]])
  txt <- ler_texto(file.path(GEO_DIR, sprintf("MUN_%d.geojson", uf)))
  contorno <- Filter(function(f) identical(as.character(f$properties$codarea), chave), .FEICOES_UF)
  m <- if (length(contorno)) {
    contorno[[1]]$properties$codarea <- "CONTORNO"
    htmlwidgets::JS(sprintf("(function(){ var m = %s; m.features.push(%s); return m; })()",
                            txt, jsonlite::toJSON(contorno[[1]], auto_unbox = TRUE, digits = NA)))
  } else htmlwidgets::JS(txt)
  .cache_malha[[chave]] <- m
  m
}

# ------------------------------------------------- leitura municipal ---------
# Cache simples por processo (chave -> data.frame), com teto de entradas para
# nao crescer sem limite num servidor que fica dias no ar.
.cache_mun <- new.env(parent = emptyenv())
CACHE_MUN_MAX <- 60L

cache_get <- function(chave, calcular) {
  if (!is.null(.cache_mun[[chave]])) return(.cache_mun[[chave]])
  v <- calcular()
  if (length(ls(.cache_mun)) >= CACHE_MUN_MAX) rm(list = ls(.cache_mun), envir = .cache_mun)
  .cache_mun[[chave]] <- v
  v
}

#' Base completa (todas as classes e faixas) de um municipio.
dados_municipio <- function(cod) {
  cache_get(paste0("mun_", cod), function() {
    cod <- as.integer(cod)
    preparar_base(DS_MUN |> dplyr::filter(municipio_geocodigo == cod) |> dplyr::collect())
  })
}

#' Casos e MEDIDA_SECUNDARIA (`extra`) por municipio e semana numa UF, para um conjunto de
#' classes (mapa, quadrantes, ranking).
dados_mun_uf <- function(uf, classes) {
  cache_get(sprintf("uf_%d_%s", uf, paste(sort(classes), collapse = "+")), function() {
    uf <- as.integer(uf)
    DS_MUN |>
      dplyr::filter(uf_ibge == uf, agente %in% classes) |>
      dplyr::group_by(municipio_geocodigo, data_ini_se) |>
      dplyr::summarise(casos = sum(casos), extra = sum(!!rlang::sym(MEDIDA_SECUNDARIA)),
                       obitos = sum(obitos), .groups = "drop") |>
      dplyr::collect() |>
      dplyr::mutate(municipio_geocodigo = as.integer(municipio_geocodigo),
                    data_ini_se = as.Date(data_ini_se),
                    dplyr::across(c(casos, extra, obitos), as.numeric))
  })
}

# ------------------------------------------------------------------ nowcast --
# Chain ladder empirico sobre DT_DIGITA, igual ao gripe_ride, mas com F(k)
# POR UF: F(k) = fracao dos casos de uma semana ja digitados k semanas
# depois, nas semanas maduras (>= 26 semanas) dos ultimos 2 anos com pelo
# menos 20 casos. UF com menos de 26 semanas de base usa o F(k)
# nacional (chave 0).
NOWCAST_MAX_K <- 8L
NOWCAST_MIN_K <- 0L   # como no gripe_ride: SRAG tem volume para corrigir a semana corrente
NOWCAST_JANELA <- 104L
NOWCAST_MIN_CASOS <- 20
NOWCAST_MIN_SEMANAS <- 26L

DATA_REF_ARQUIVO <- max(ATRASO$data_ini_se + 7L * ATRASO$atraso_sem)

estimar_f <- function(at) {
  maduras <- at |>
    dplyr::filter(data_ini_se <= DATA_REF_ARQUIVO - 26L * 7L) |>
    dplyr::filter(data_ini_se >  max(data_ini_se) - NOWCAST_JANELA * 7L)
  por_semana <- maduras |>
    dplyr::group_by(data_ini_se) |>
    dplyr::summarise(
      total = sum(casos),
      cum   = list(vapply(0:NOWCAST_MAX_K, function(k) sum(casos[atraso_sem <= k]), numeric(1))),
      .groups = "drop"
    ) |>
    dplyr::filter(total >= NOWCAST_MIN_CASOS)
  if (nrow(por_semana) < NOWCAST_MIN_SEMANAS) return(NULL)
  mat <- do.call(rbind, por_semana$cum) / por_semana$total
  data.frame(
    k     = 0:NOWCAST_MAX_K,
    f     = colSums(do.call(rbind, por_semana$cum)) / sum(por_semana$total),
    f_q10 = apply(mat, 2, stats::quantile, 0.10),
    f_q90 = apply(mat, 2, stats::quantile, 0.90),
    n_semanas = nrow(por_semana)
  )
}

NOWCAST_F_UF <- local({
  nac <- estimar_f(ATRASO |> dplyr::group_by(data_ini_se, atraso_sem) |>
                     dplyr::summarise(casos = sum(casos), .groups = "drop"))
  por_uf <- lapply(UF_LOOKUP$uf_ibge, function(u) {
    f <- estimar_f(ATRASO[ATRASO$uf_ibge == u, , drop = FALSE])
    if (is.null(f)) { f <- nac; attr(f, "nacional") <- TRUE }
    f
  })
  stats::setNames(c(list(nac), por_uf), c("0", UF_LOOKUP$uf_ibge))
})
NOWCAST_F <- NOWCAST_F_UF[["0"]]   # nacional (texto da aba "Sobre")

UFS_NOWCAST_NACIONAL <- UF_LOOKUP$uf_sigla[vapply(as.character(UF_LOOKUP$uf_ibge),
  function(u) isTRUE(attr(NOWCAST_F_UF[[u]], "nacional")), logical(1))]

message(sprintf("[%s] Nowcast: arquivo até %s; F(1) nacional = %.0f%%; %d UF(s) com F(k) nacional.",
                PAINEL_NOME, format(DATA_REF_ARQUIVO, "%d/%m/%Y"), 100 * NOWCAST_F$f[2],
                length(UFS_NOWCAST_NACIONAL)))

#' F(k) (coluna `col`) para vetores de UF (0 = Brasil) e de idade k.
f_nowcast <- function(uf, k, col = "f") {
  uf <- rep_len(as.character(uf), length(k))
  out <- rep(NA_real_, length(k))
  for (u in unique(uf)) {
    tab <- NOWCAST_F_UF[[u]] %||% NOWCAST_F
    i <- uf == u
    out[i] <- tab[[col]][match(k[i], tab$k)]
  }
  out
}

#' Aplica o nowcast a uma serie semanal (colunas data_ini_se, casos).
#' Acrescenta casos_est, casos_est_min, casos_est_max (NA fora da janela).
aplicar_nowcast <- function(d, uf = 0L) {
  k <- as.integer((DATA_REF_ARQUIVO - d$data_ini_se) / 7)
  k[k < NOWCAST_MIN_K | k > NOWCAST_MAX_K] <- NA_integer_
  f   <- f_nowcast(uf, k, "f")
  f10 <- f_nowcast(uf, k, "f_q10")
  f90 <- f_nowcast(uf, k, "f_q90")
  d$casos_est     <- ifelse(is.na(f), NA_real_, d$casos / pmax(f, 0.05))
  d$casos_est_min <- ifelse(is.na(f), NA_real_, d$casos / pmax(f90, 0.05))
  d$casos_est_max <- ifelse(is.na(f), NA_real_, d$casos / pmax(f10, 0.05))
  d
}

# ----------------------------------------------------------------------- Rt --
# Nao ha Rt pronto para SRAG no banco (o InfoDengue so cobre arboviroses),
# entao o painel estima o seu, a partir da serie semanal do recorte+agente:
#
#   1. r = taxa de crescimento diaria entre duas janelas de 2 semanas:
#        r = ln(S_t / S_{t-2}) / 14,   S_t = casos(t) + casos(t-1)
#      (2 semanas suavizam o ruido de uma semana isolada).
#   2. Rt = (1 + r * theta)^k  (Wallinga & Lipsitch, 2007), com o intervalo
#      de geracao gama de media m e desvio s: k = (m/s)^2, theta = s^2/m.
#   3. IC 95%: erro de Poisson das contagens, ep(ln S_t/S_{t-2}) =
#      sqrt(1/S_t + 1/S_{t-2}), propagado por r.
#
# As semanas recentes usam os casos corrigidos pelo nowcast; o Rt delas e
# preliminar (linha tracejada). A semana corrente fica de fora: so uma fracao
# pequena dela esta digitada. Janela com menos de RT_MIN_CASOS casos no total
# fica sem estimativa.
#
# SRAG sao casos GRAVES: o Rt mede o crescimento das internacoes, um
# indicador atrasado e parcial da transmissao na comunidade.
RT_MIN_CASOS <- 20

# Intervalo de geracao (dias) por agente. Media/desvio da literatura:
# influenza 3,6/1,6 (Cowling et al., 2009); covid-19 Omicron ~3,5/2,4;
# VSR ~7,5/2,1 (Crowcroft et al., 2008). Mistura de agentes ou outros virus:
# valor generico de virus respiratorio, 4,5/2,5.
RT_INTERVALO_GERACAO <- list(
  influenza   = c(media = 3.6, dp = 1.6),
  influenza_a = c(media = 3.6, dp = 1.6),
  influenza_b = c(media = 3.6, dp = 1.6),
  covid       = c(media = 3.5, dp = 2.4),
  vsr         = c(media = 7.5, dp = 2.1),
  generico    = c(media = 4.5, dp = 2.5)
)

intervalo_geracao <- function(chave) RT_INTERVALO_GERACAO[[chave]] %||% RT_INTERVALO_GERACAO$generico

#' Rt semanal de uma serie (saida de serie_semanal). Acrescenta rt, rt_min,
#' rt_max, rt_casos (casos nas duas janelas) e rt_prelim (usa nowcast).
estimar_rt <- function(s, chave_agente) {
  ig    <- intervalo_geracao(chave_agente)
  k     <- (ig[["media"]] / ig[["dp"]])^2
  theta <- ig[["dp"]]^2 / ig[["media"]]

  n   <- nrow(s)
  inc <- dplyr::coalesce(s$casos_est, s$casos)
  inc[n] <- NA_real_                       # semana corrente: incompleta demais
  S   <- inc + dplyr::lag(inc)             # janela de 2 semanas terminando em t
  S0  <- dplyr::lag(S, 2)                  # as 2 semanas anteriores
  ok  <- !is.na(S) & !is.na(S0) & S > 0 & S0 > 0 & (S + S0) >= RT_MIN_CASOS

  para_rt <- function(lr) {
    base <- 1 + (lr / 14) * theta
    ifelse(base > 0, base^k, NA_real_)
  }
  lr <- ifelse(ok, log(S / S0), NA_real_)
  ep <- ifelse(ok, sqrt(1 / S + 1 / S0), NA_real_)

  s$rt        <- para_rt(lr)
  s$rt_min    <- para_rt(lr - 1.96 * ep)
  s$rt_max    <- para_rt(lr + 1.96 * ep)
  s$rt_casos  <- ifelse(ok, S + S0, NA_real_)
  # preliminar = a janela (t-1, t) inclui semana corrigida pelo nowcast
  com_nc      <- !is.na(s$casos_est)
  s$rt_prelim <- com_nc | dplyr::lag(com_nc, default = FALSE)
  s
}

# ----------------------------------------------------------------- helpers ---

#' Filtra a base pelo recorte geografico e pelo conjunto de agentes.
#' Brasil e UF vem da base por UF em memoria; municipio, do Parquet.
filtrar <- function(geo, classes = AGENTES$agente) {
  d <- switch(
    geo$nivel,
    base      = DADOS,
    uf        = DADOS[DADOS$uf_ibge == geo$uf_ibge, , drop = FALSE],
    municipio = dados_municipio(geo$geocodigo),
    stop("Nivel geografico desconhecido: ", geo$nivel, call. = FALSE)
  )
  d[d$agente %in% classes, , drop = FALSE]
}

#' UF do recorte para o nowcast (0 = Brasil).
uf_do_recorte <- function(geo) if (identical(geo$nivel, "base")) 0L else as.integer(geo$uf_ibge)

#' Populacao do recorte por ano.
pop_recorte <- function(geo) {
  switch(
    geo$nivel,
    base = POP_UF_ANO |> dplyr::group_by(ano) |>
      dplyr::summarise(pop = sum(pop, na.rm = TRUE), .groups = "drop"),
    uf = POP_UF_ANO |> dplyr::filter(uf_ibge == geo$uf_ibge) |> dplyr::select(ano, pop),
    municipio = POP_MUN_ANO |> dplyr::filter(municipio_geocodigo == geo$geocodigo) |>
      dplyr::select(ano, pop)
  )
}

#' SE (AAAASS) de um domingo de inicio de semana - calendario do MS.
se_de_domingo <- function(dd) {
  ano <- as.integer(format(dd + 3L, "%Y"))
  jan4 <- as.Date(sprintf("%d-01-04", ano))
  ano * 100L + as.integer((dd - (jan4 - as.integer(format(jan4, "%w")))) / 7) + 1L
}

#' Grade completa de semanas (sem buraco quando nao ha caso).
SEMANAS <- data.frame(data_ini_se = seq(DATA_MIN, DATA_MAX, by = 7))
SEMANAS$se <- se_de_domingo(SEMANAS$data_ini_se)

#' Serie semanal (total do recorte e classificacoes), com incidencia e nowcast.
serie_semanal <- function(geo, classes) {
  d <- filtrar(geo, classes) |>
    dplyr::group_by(data_ini_se) |>
    dplyr::summarise(casos = sum(casos), obitos = sum(obitos), uti = sum(uti),
                     .groups = "drop")
  SEMANAS |>
    dplyr::left_join(d, by = "data_ini_se") |>
    dplyr::mutate(dplyr::across(c(casos, obitos, uti), ~ dplyr::coalesce(.x, 0)),
                  ano = se %/% 100L, semana = se %% 100L) |>
    dplyr::left_join(pop_recorte(geo), by = "ano") |>
    dplyr::mutate(incidencia = casos / pop * 1e5) |>
    aplicar_nowcast(uf_do_recorte(geo))
}

# ----------------------------------------------- unidades do recorte --------
# Mapa, quadrantes e ranking comparam "unidades": as UFs na visao Brasil e os
# municipios da UF num estado ou municipio.

#' Tipo e conjunto de unidades do recorte.
unidades_do_recorte <- function(geo) {
  if (identical(geo$nivel, "base")) {
    list(tipo = "uf", uf = NA_integer_, rotulo = "UF", rotulo_plural = "UFs",
         dict = data.frame(id = UF_LOOKUP$uf_ibge, nome = UF_LOOKUP$uf_nome,
                           sigla = UF_LOOKUP$uf_sigla, uf_ibge = UF_LOOKUP$uf_ibge))
  } else {
    g <- GEO_DICT[GEO_DICT$uf_ibge == geo$uf_ibge, , drop = FALSE]
    list(tipo = "municipio", uf = as.integer(geo$uf_ibge), rotulo = "município",
         rotulo_plural = "municípios",
         dict = data.frame(id = g$municipio_geocodigo, nome = g$municipio_nome,
                           sigla = g$uf_sigla, uf_ibge = g$uf_ibge))
  }
}

#' Casos, MEDIDA_SECUNDARIA (`extra`) e obitos por unidade e semana (so semanas com caso).
casos_por_unidade <- function(un, classes) {
  if (identical(un$tipo, "uf")) {
    DADOS[DADOS$agente %in% classes, , drop = FALSE] |>
      dplyr::group_by(id = uf_ibge, data_ini_se) |>
      dplyr::summarise(casos = sum(casos), extra = sum(!!rlang::sym(MEDIDA_SECUNDARIA)),
                       obitos = sum(obitos), .groups = "drop")
  } else {
    dados_mun_uf(un$uf, classes) |> dplyr::rename(id = municipio_geocodigo)
  }
}

#' Populacao por unidade e ano.
pop_unidades <- function(un) {
  if (identical(un$tipo, "uf")) POP_UF_ANO |> dplyr::rename(id = uf_ibge)
  else POP_MUN_ANO |> dplyr::filter(uf_ibge == un$uf) |>
    dplyr::select(id = municipio_geocodigo, ano, pop)
}

#' Expressao JS que formata "AAAA-MM-DD" como "dd/mm/aaaa".
JS_DATA_BR <- "(function(s){var p=String(s).split('-');return p[2]+'/'+p[1]+'/'+p[0];})"

fmt_int <- function(x) {
  if (length(x) == 0 || all(is.na(x))) return("-")
  format(round(as.numeric(x)), big.mark = ".", decimal.mark = ",",
         scientific = FALSE, trim = TRUE)
}

fmt_num <- function(x, digits = 1) {
  if (length(x) == 0 || all(is.na(x))) return("-")
  format(round(as.numeric(x), digits), big.mark = ".", decimal.mark = ",",
         nsmall = digits, scientific = FALSE, trim = TRUE)
}

fmt_pct <- function(x, digits = 1) {
  if (length(x) == 0 || all(is.na(x)) || !is.finite(x)) return("-")
  paste0(fmt_num(100 * x, digits), "%")
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# --------------------------------------------------------- modulos do app ----
for (.f in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  source(.f, local = FALSE, encoding = "UTF-8")
}
rm(.f)

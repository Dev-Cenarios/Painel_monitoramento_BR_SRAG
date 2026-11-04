# ==============================================================================
# Gripe Brasil | etl_gripe_br.R
# Extrai do PostgreSQL ("Info_Gripe".srag, microdado SIVEP-Gripe/OpenDataSUS)
# os casos de SRAG de residentes em TODO o Brasil e grava Parquets JA
# AGREGADOS, unica fonte de dados lida pelo app:
#
#   data/srag_br_uf.parquet      semana x UF x agente x faixa etaria -> casos,
#                                obitos, UTI, vacinados, coinfeccao
#                                (fica inteiro em memoria: Brasil e estados)
#   data/srag_br_mun.parquet     o mesmo, por MUNICIPIO, ordenado por UF e
#                                municipio (lido do disco sob demanda)
#   data/srag_br_atraso.parquet  semana de sintomas x UF x atraso de digitacao
#                                (semanas) -> casos. Base do nowcast.
#   data/srag_br_pop.parquet     municipio x ano -> populacao (5.570)
#
# Nenhuma linha de microdado sai do banco: toda a agregacao roda no Postgres.
#
# Uso (working directory = raiz do projeto):  Rscript etl_gripe_br.R
#
# Mesmas regras do etl_gripe_ride.R (agente, coinfeccao, faixas, DF). Duas
# diferencas: sem filtro de municipio e sem a quebra por Regiao
# Administrativa do DF (no Brasil o DF e um municipio, como nos outros
# paineis nacionais). Todo 53xxxx continua indo para 5300108 - no SIVEP a
# residencia no DF vem por RA, e filtrar so 530010 perderia ~99% dos casos.
# ==============================================================================

library(DBI)
library(RPostgres)
library(arrow)
library(dplyr)

if (file.exists(".Renviron")) readRenviron(".Renviron")

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host     = Sys.getenv("PG_HOST"),
  port     = as.integer(Sys.getenv("PG_PORT", "5432")),
  dbname   = Sys.getenv("PG_DB"),
  user     = Sys.getenv("PG_USER"),
  password = Sys.getenv("PG_PASS"),  # credenciais no .Renviron (ver .Renviron.example)
  bigint   = "numeric"
)

DATA_MIN <- Sys.getenv("GRIPE_DATA_MIN", "2018-12-30")
t_ini <- Sys.time()
log_t <- function(...) message(sprintf("[ETL Gripe BR %5.0fs] ", as.numeric(difftime(Sys.time(), t_ini, units = "secs"))), ...)
fmt <- function(x) format(x, big.mark = ".", decimal.mark = ",")

# -------------------------------------------- populacao (InfoDengue) -------
log_t("Lendo populacao de Info_Dengue...")
pop <- DBI::dbGetQuery(con, "
SELECT municipio_geocodigo::int AS municipio_geocodigo,
       COALESCE(max(municipio_nome) FILTER (WHERE municipio_nome ~ '^[A-ZÀ-Ú]'), max(municipio_nome)) AS municipio_nome,
       max(uf_ibge)::int AS uf_ibge,
       EXTRACT(YEAR FROM data_ini_se)::int AS ano, max(pop) AS pop
FROM \"Info_Dengue\".dengue
GROUP BY 1, 4")

# ------------------------------------------------------------- base comum --
# Classificacao do agente: ver etl_gripe_ride.R.
sql_base <- sprintf("
WITH base AS (
  SELECT
    substr(dt_sin_pri, 1, 10)::date                       AS dt_sin,
    CASE WHEN dt_digita ~ '^[0-9]{4}-[0-9]{2}-[0-9]{2}'
         THEN substr(dt_digita, 1, 10)::date END          AS dt_dig,
    co_mun_res,
    split_part(classi_fin, '.', 1)                        AS cf,
    split_part(tp_flu_pcr, '.', 1)                        AS flu_pcr,
    split_part(tp_flu_an,  '.', 1)                        AS flu_an,
    split_part(pcr_vsr,    '.', 1)                        AS vsr_pcr,
    split_part(an_vsr,     '.', 1)                        AS vsr_an,
    (split_part(pcr_metap, '.', 1) = '1')::int            AS v_metap,
    (split_part(pcr_adeno, '.', 1) = '1' OR split_part(an_adeno, '.', 1) = '1')::int AS v_adeno,
    (split_part(pcr_para1, '.', 1) = '1' OR split_part(pcr_para2, '.', 1) = '1' OR
     split_part(pcr_para3, '.', 1) = '1' OR split_part(pcr_para4, '.', 1) = '1' OR
     split_part(an_para1,  '.', 1) = '1' OR split_part(an_para2,  '.', 1) = '1' OR
     split_part(an_para3,  '.', 1) = '1')::int            AS v_para,
    (split_part(pcr_rino,  '.', 1) = '1')::int            AS v_rino,
    (split_part(pcr_boca,  '.', 1) = '1' OR split_part(pcr_outro, '.', 1) = '1' OR
     split_part(an_outro,  '.', 1) = '1')::int            AS v_outro,
    split_part(evolucao,   '.', 1)                        AS evol,
    split_part(uti,        '.', 1)                        AS uti_,
    split_part(vacina,     '.', 1)                        AS vac,
    split_part(tp_idade,   '.', 1)                        AS tp_id,
    NULLIF(split_part(nu_idade_n, '.', 1), '')            AS idade_txt
  FROM \"Info_Gripe\".srag
  WHERE co_mun_res ~ '^[1-5][0-9]{5}'
    AND dt_sin_pri ~ '^[0-9]{4}-[0-9]{2}-[0-9]{2}'
    AND substr(dt_sin_pri, 1, 10) >= '%s'
    AND substr(dt_sin_pri, 1, 10) <= current_date::text
),
classif AS (
  SELECT
    dt_sin - EXTRACT(DOW FROM dt_sin)::int                AS data_ini_se,
    dt_dig,
    CASE WHEN co_mun_res LIKE '53%%' THEN '530010' ELSE substr(co_mun_res, 1, 6) END AS mun6,
    CASE
      WHEN cf = '1' AND (flu_pcr = '1' OR (flu_pcr NOT IN ('1','2') AND flu_an = '1')) THEN 'influenza_a'
      WHEN cf = '1' AND (flu_pcr = '2' OR (flu_pcr NOT IN ('1','2') AND flu_an = '2')) THEN 'influenza_b'
      WHEN cf = '1' THEN 'influenza_nd'
      WHEN cf = '5' THEN 'covid'
      WHEN cf = '2' AND (vsr_pcr = '1' OR vsr_an = '1') THEN 'vsr'
      WHEN cf = '2' AND v_metap = 1 THEN 'metapneumovirus'
      WHEN cf = '2' AND v_adeno = 1 THEN 'adenovirus'
      WHEN cf = '2' AND v_para  = 1 THEN 'parainfluenza'
      WHEN cf = '2' AND v_rino  = 1 THEN 'rinovirus'
      WHEN cf = '2' THEN 'outros_virus'
      WHEN cf = '3' THEN 'outro_agente'
      WHEN cf = '4' THEN 'nao_especificado'
      ELSE 'em_investigacao'
    END                                                   AS agente,
    CASE
      WHEN idade_txt IS NULL OR idade_txt !~ '^[0-9]+$' THEN 'ign'
      WHEN tp_id IN ('1','2') THEN '0-1'
      WHEN idade_txt::int < 2   THEN '0-1'
      WHEN idade_txt::int < 5   THEN '2-4'
      WHEN idade_txt::int < 15  THEN '5-14'
      WHEN idade_txt::int < 50  THEN '15-49'
      WHEN idade_txt::int < 65  THEN '50-64'
      ELSE '65+'
    END                                                   AS faixa_etaria,
    (evol = '2')::int                                     AS obito,
    (uti_ = '1')::int                                     AS uti,
    (vac  = '1')::int                                     AS vacinado_gripe,
    (cf = '2' AND ((vsr_pcr = '1' OR vsr_an = '1')::int + v_metap + v_adeno + v_para +
                   v_rino + v_outro) >= 2)::int           AS coinfeccao
  FROM base
)
", DATA_MIN)

# ------------------------------------------------------ 1. serie agregada --
log_t("Agregando SRAG do Brasil no Postgres (municipio)...")
agg <- DBI::dbGetQuery(con, paste0(sql_base, "
SELECT data_ini_se, mun6, agente, faixa_etaria,
       count(*)             AS casos,
       sum(obito)           AS obitos,
       sum(uti)             AS uti,
       sum(vacinado_gripe)  AS vacinado_gripe,
       sum(coinfeccao)      AS coinfeccao
FROM classif
GROUP BY data_ini_se, mun6, agente, faixa_etaria
"))
log_t(sprintf("%s linhas, %s casos.", fmt(nrow(agg)), fmt(sum(agg$casos))))

# ------------------------------------------------- 2. triangulo de atraso --
log_t("Montando triangulo de atraso por UF...")
atraso <- DBI::dbGetQuery(con, paste0(sql_base, "
SELECT data_ini_se, left(mun6, 2)::int AS uf_ibge,
       LEAST(GREATEST(floor((dt_dig - data_ini_se) / 7.0)::int, 0), 52) AS atraso_sem,
       count(*) AS casos
FROM classif
WHERE dt_dig IS NOT NULL AND dt_dig <= current_date
GROUP BY 1, 2, 3
"))

DBI::dbDisconnect(con)

# ----------------------------------------------- semana epidemiologica (MS) --
se_de_domingo <- function(domingo) {
  ano <- as.integer(format(domingo + 3L, "%Y"))
  jan4 <- as.Date(sprintf("%d-01-04", ano))
  inicio <- jan4 - as.integer(format(jan4, "%w"))
  ano * 100L + as.integer((domingo - inicio) / 7) + 1L
}

medidas <- c("casos", "obitos", "uti", "vacinado_gripe", "coinfeccao")

ano_ini <- min(pop$ano)
ano_fim <- max(as.integer(format(max(as.Date(agg$data_ini_se)) + 3L, "%Y")), max(pop$ano))
pop_full <- pop |>
  group_by(municipio_geocodigo) |>
  tidyr::complete(ano = seq(ano_ini, ano_fim)) |>
  tidyr::fill(pop, municipio_nome, uf_ibge, .direction = "downup") |>
  ungroup() |>
  mutate(pop = as.numeric(pop))

mun_dict <- pop_full |> distinct(municipio_geocodigo, municipio_nome, uf_ibge)
mun_dict$mun6 <- substr(mun_dict$municipio_geocodigo, 1, 6)

agg <- agg |>
  mutate(data_ini_se = as.Date(data_ini_se),
         se = se_de_domingo(data_ini_se),
         across(all_of(medidas), as.integer)) |>
  left_join(mun_dict |> select(mun6, municipio_geocodigo, uf_ibge), by = "mun6")

sem_mun <- agg |> filter(is.na(municipio_geocodigo))
if (nrow(sem_mun)) {
  log_t(sprintf("AVISO: %s casos com municipio de residencia sem codigo IBGE (descartados): %s",
                fmt(sum(sem_mun$casos)), paste(utils::head(unique(sem_mun$mun6), 15), collapse = ", ")))
}

mun <- agg |>
  filter(!is.na(municipio_geocodigo)) |>
  transmute(data_ini_se, se, uf_ibge = as.integer(uf_ibge),
            municipio_geocodigo = as.integer(municipio_geocodigo),
            agente, faixa_etaria, across(all_of(medidas))) |>
  arrange(uf_ibge, municipio_geocodigo, data_ini_se, agente, faixa_etaria)

uf <- mun |>
  group_by(data_ini_se, se, uf_ibge, agente, faixa_etaria) |>
  summarise(across(all_of(medidas), sum), .groups = "drop") |>
  arrange(uf_ibge, data_ini_se, agente, faixa_etaria)

atraso <- atraso |>
  mutate(data_ini_se = as.Date(data_ini_se), se = se_de_domingo(data_ini_se),
         uf_ibge = as.integer(uf_ibge), atraso_sem = as.integer(atraso_sem),
         casos = as.integer(casos)) |>
  filter(uf_ibge %in% mun_dict$uf_ibge) |>
  arrange(uf_ibge, data_ini_se, atraso_sem)

dir.create("data", showWarnings = FALSE, recursive = TRUE)
arrow::write_parquet(mun, "data/srag_br_mun.parquet", chunk_size = 50000)
arrow::write_parquet(uf, "data/srag_br_uf.parquet")
arrow::write_parquet(atraso, "data/srag_br_atraso.parquet")
arrow::write_parquet(pop_full, "data/srag_br_pop.parquet")

log_t(sprintf(
  "OK: %s casos de SRAG, %s a %s, %d municipios com caso; %s linhas municipais, %s por UF.",
  fmt(sum(mun$casos)), format(min(mun$data_ini_se), "%d/%m/%Y"), format(max(mun$data_ini_se), "%d/%m/%Y"),
  dplyr::n_distinct(mun$municipio_geocodigo), fmt(nrow(mun)), fmt(nrow(uf))
))

# ==============================================================================
# Gripe Brasil | Aba "Análises"
#
#   - ranking (tabela, respeita o filtro de agente e o recorte do Painel:
#     UFs na visao Brasil, municipios da UF num estado/municipio)
#   - agentes por unidade nas ultimas 52 semanas (no lugar do grafico por RA
#     do DF do gripe_ride: o painel nacional nao quebra o DF por RA)
#   - achados da analise exploratoria, CALCULADOS da base no start (nao ha
#     numero escrito a mao: um novo ETL atualiza o texto)
# ==============================================================================

# ------------------------------------------------------------------ achados --
calcular_achados <- function() {
  d <- DADOS
  encerr <- d[d$data_ini_se <= DATA_MAX - 8L * 7L, , drop = FALSE]  # desfecho conhecido
  pos <- encerr[encerr$ano >= 2022, , drop = FALSE]
  grupo <- function(x) AGENTES$grupo[match(x, AGENTES$agente)]
  mes <- c("jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", "out", "nov", "dez")
  mes_da_se <- function(se) mes[as.integer(format(as.Date("2023-01-01") + (se - 1) * 7 + 3, "%m"))]

  # 1. sazonalidade: SE de pico por ano (2022 em diante, anos completos)
  pico <- d |>
    dplyr::filter(ano >= 2022, ano < ANO_MAX + 1L) |>
    dplyr::mutate(g = grupo(agente)) |>
    dplyr::filter(g %in% c("vsr", "influenza")) |>
    dplyr::group_by(g, ano, semana) |>
    dplyr::summarise(n = sum(casos), .groups = "drop") |>
    dplyr::group_by(g, ano) |>
    dplyr::slice_max(n, n = 1, with_ties = FALSE) |>
    dplyr::ungroup()
  # A influenza de 2022 teve pico na SE 1 (cauda da H3N2 de 2021/22): fora
  # da mediana sazonal para nao puxar o pico "tipico" para janeiro.
  pk_vsr <- stats::median(pico$semana[pico$g == "vsr"])
  pk_flu <- stats::median(pico$semana[pico$g == "influenza" & pico$semana > 5])

  # 1b. pico do total de SRAG por regiao (2022+, anos completos)
  pico_reg <- d |>
    dplyr::filter(ano >= 2022, ano < ANO_MAX) |>
    dplyr::left_join(UF_LOOKUP[, c("uf_ibge", "regiao")], by = "uf_ibge") |>
    dplyr::group_by(regiao, ano, semana) |>
    dplyr::summarise(n = sum(casos), .groups = "drop") |>
    dplyr::group_by(regiao, ano) |>
    dplyr::slice_max(n, n = 1, with_ties = FALSE) |>
    dplyr::group_by(regiao) |>
    dplyr::summarise(se = stats::median(semana), .groups = "drop")
  pk_reg <- function(r) as.integer(pico_reg$se[pico_reg$regiao == r])

  # 2. letalidade por agente (pos-2022, casos encerrados)
  let <- pos |>
    dplyr::mutate(g = grupo(agente)) |>
    dplyr::group_by(g) |>
    dplyr::summarise(l = sum(obitos) / sum(casos), u = sum(uti) / sum(casos), .groups = "drop")
  lg <- function(x) let$l[let$g == x]
  flu65 <- pos[grupo(pos$agente) == "influenza" & pos$faixa_etaria == "65+", ]
  flu_let65 <- sum(flu65$obitos) / sum(flu65$casos)
  flu_vac65 <- sum(flu65$vacinado_gripe) / sum(flu65$casos)

  # 3. VSR em criancas
  vsr <- pos[pos$agente == "vsr", ]
  vsr_bebe <- sum(vsr$casos[vsr$faixa_etaria == "0-1"]) / sum(vsr$casos)
  vsr_5 <- sum(vsr$casos[vsr$faixa_etaria %in% c("0-1", "2-4")]) / sum(vsr$casos)

  # 4. covid: participacao entre casos com agente identificado
  ident <- d[!d$agente %in% c("nao_especificado", "em_investigacao"), ]
  pc <- function(a) sum(ident$casos[ident$ano == a & ident$agente == "covid"]) /
    sum(ident$casos[ident$ano == a])

  # 5. ano corrente vs anterior, mesmo periodo, por grupo
  sem_ult <- max(d$semana[d$ano == ANO_MAX])
  comp <- d |>
    dplyr::filter(ano %in% c(ANO_MAX - 1L, ANO_MAX), semana <= sem_ult) |>
    dplyr::mutate(g = dplyr::if_else(agente %in% c("influenza_a", "influenza_b"), agente, grupo(agente))) |>
    dplyr::group_by(g, ano) |>
    dplyr::summarise(n = sum(casos), .groups = "drop") |>
    tidyr::pivot_wider(names_from = ano, values_from = n, values_fill = 0)
  names(comp) <- c("g", "ant", "cor")
  cv <- function(g) { r <- comp[comp$g == g, ]; if (!nrow(r)) c(0, 0) else c(r$ant, r$cor) }
  fa <- cv("influenza_a"); fb <- cv("influenza_b"); cov <- cv("covid"); vs <- cv("vsr")
  tot_ant <- sum(comp$ant); tot_cor <- sum(comp$cor)

  # 6. etiologia nao especificada
  nesp <- d |>
    dplyr::group_by(ano) |>
    dplyr::summarise(p = sum(casos[agente == "nao_especificado"]) / sum(casos), .groups = "drop")

  # 7. DF x entorno
  ult52 <- d[d$data_ini_se > DATA_MAX - 52L * 7L, ]
  pop_ult <- POP_UF_ANO[POP_UF_ANO$ano == max(POP_UF_ANO$ano), ]
  inc_uf <- ult52 |>
    dplyr::group_by(uf_ibge) |>
    dplyr::summarise(n = sum(casos), .groups = "drop") |>
    dplyr::left_join(pop_ult, by = "uf_ibge") |>
    dplyr::mutate(inc = n / pop * 1e5) |>
    dplyr::arrange(dplyr::desc(inc))
  inc_br <- sum(ult52$casos) / sum(pop_ult$pop) * 1e5
  sig <- function(u) UF_LOOKUP$uf_sigla[match(u, UF_LOOKUP$uf_ibge)]

  # 8. outros virus: rinovirus/metapneumovirus, coinfeccao, perfil
  n_ano <- function(a, ano, ate = 53L) sum(d$casos[d$agente == a & d$ano == ano & d$semana <= ate])
  rino <- vapply(c(2023L, 2024L, 2025L), function(a) n_ano("rinovirus", a), numeric(1))
  metap_ant <- n_ano("metapneumovirus", ANO_MAX - 1L, sem_ult)
  metap_cor <- n_ano("metapneumovirus", ANO_MAX, sem_ult)
  ov <- pos[pos$agente %in% c("vsr", OUTROS_VIRUS), ]
  coinf <- sum(ov$coinfeccao) / sum(ov$casos)
  ri <- pos[pos$agente == "rinovirus", ]
  rino_5 <- sum(ri$casos[ri$faixa_etaria %in% c("0-1", "2-4")]) / sum(ri$casos)
  mp <- pos[pos$agente == "metapneumovirus", ]
  mp_let65 <- sum(mp$obitos[mp$faixa_etaria == "65+"]) / max(sum(mp$casos[mp$faixa_etaria == "65+"]), 1)

  pct <- function(x, dg = 0) fmt_pct(x, dg)
  var <- function(a, b) if (a > 0) sprintf("%s%s", if (b >= a) "+" else "−", pct(abs(b / a - 1))) else "n/a"

  list(
    list(titulo = "VSR chega antes da influenza",
         texto = sprintf(
           "Desde 2022 o pico de VSR cai, em mediana, na SE %d (%s) e o de influenza na SE %d (%s): cerca de %d semanas depois. O VSR é o sinal antecedente da temporada respiratória - e o momento de preparar leitos pediátricos, já que %s dos casos de VSR são em menores de 2 anos (%s em menores de 5).",
           as.integer(pk_vsr), mes_da_se(pk_vsr), as.integer(pk_flu), mes_da_se(pk_flu),
           as.integer(pk_flu - pk_vsr), pct(vsr_bebe), pct(vsr_5))),
    list(titulo = "A temporada cabe no ano civil - mas não chega junto a todo o país",
         texto = sprintf(
           "Desde 2022 o pico anual de SRAG cai, em mediana, na SE %d no Norte, %d no Nordeste, %d no Centro-Oeste, %d no Sudeste e %d no Sul. Ao contrário da dengue (temporada de verão, que atravessa a virada do ano), a SRAG cabe no ano calendário - por isso o canal endêmico usa SE 1 a 52. Compare estados de regiões diferentes com essa defasagem em mente.",
           pk_reg("Norte"), pk_reg("Nordeste"), pk_reg("Centro-Oeste"), pk_reg("Sudeste"), pk_reg("Sul"))),
    list(titulo = sprintf("%d frente a %d (até a SE %d)", ANO_MAX, ANO_MAX - 1L, sem_ult),
         texto = sprintf(
           "Total de SRAG %s (%s → %s). Influenza A %s (%s → %s) e influenza B %s (%s → %s): %s. VSR %s; covid-19 %s (%s → %s).",
           var(tot_ant, tot_cor), fmt_int(tot_ant), fmt_int(tot_cor),
           var(fa[1], fa[2]), fmt_int(fa[1]), fmt_int(fa[2]),
           var(fb[1], fb[2]), fmt_int(fb[1]), fmt_int(fb[2]),
           if (fb[2] > fa[2]) "a influenza B superou a A, o que é incomum"
           else "a influenza A segue predominante",
           var(vs[1], vs[2]), var(cov[1], cov[2]), fmt_int(cov[1]), fmt_int(cov[2]))),
    list(titulo = "Covid-19 virou coadjuvante, mas segue a mais letal",
         texto = sprintf(
           "A covid-19 respondia por %s dos casos com agente identificado em 2021 e por %s em %d. Ainda assim, entre os casos encerrados desde 2022 a letalidade é de %s na covid-19, contra %s na influenza e %s no VSR (este quase todo em crianças).",
           pct(pc(2021)), pct(pc(ANO_MAX)), ANO_MAX, pct(lg("covid"), 1), pct(lg("influenza"), 1),
           pct(lg("vsr"), 1))),
    list(titulo = "Idosos com influenza: alta letalidade, baixa vacinação registrada",
         texto = sprintf(
           "Entre os casos de SRAG por influenza em pessoas com 65 anos ou mais (desde 2022), %s evoluíram a óbito e só %s tinham vacina contra gripe registrada na ficha. O campo de vacina tem muito preenchimento em branco, então o número é um piso, mas o contraste é um argumento direto para a campanha de vacinação de abril/maio, antes do pico.",
           pct(flu_let65, 1), pct(flu_vac65))),
    list(titulo = "Rinovírus e metapneumovírus: o salto dos \"outros vírus\"",
         texto = sprintf(
           "O rinovírus passou de %s casos em 2023 para %s em 2024 e %s em 2025 - o salto coincide com a ampliação dos painéis multiplex de PCR, então provavelmente reflete mais detecção do que uma epidemia nova (a confirmar com a vigilância laboratorial). %s dos casos são em menores de 5 anos. O metapneumovírus cresce em %d: %s casos até a SE %d, contra %s no mesmo período de %d, com letalidade de %s nos idosos. %s dos casos por VSR e outros vírus têm 2+ vírus detectados; o painel atribui cada caso a um só (ordem: VSR, metapneumo, adeno, parainfluenza, rino).",
           fmt_int(rino[1]), fmt_int(rino[2]), fmt_int(rino[3]), pct(rino_5),
           ANO_MAX, fmt_int(metap_cor), sem_ult, fmt_int(metap_ant), ANO_MAX - 1L,
           pct(mp_let65, 1), pct(coinf))),
    list(titulo = "Os dados das últimas semanas são incompletos - daí o nowcast",
         texto = sprintf(
           "No Brasil, só %s dos casos de uma semana estão digitados no SIVEP ao fim da semana seguinte, %s após 4 semanas (o nowcast usa o atraso de cada UF). Sem correção, toda curva parece estar caindo no fim. O SIVEP traz a data de digitação de cada caso, o que permite estimar esse atraso (o InfoDengue do painel de dengue não oferece isso) e corrigir as últimas %d semanas.",
           pct(NOWCAST_F$f[2]), pct(NOWCAST_F$f[5]), NOWCAST_MAX_K)),
    list(titulo = "Um terço da SRAG fica sem agente identificado",
         texto = sprintf(
           "A proporção de SRAG não especificada variou de %s a %s entre os anos (%s em %d). Quando sobe, a vigilância enxerga menos influenza e VSR do que realmente circula - vale acompanhar junto da coleta de amostras.",
           pct(min(nesp$p)), pct(max(nesp$p)), pct(nesp$p[nesp$ano == ANO_MAX]), ANO_MAX)),
    list(titulo = "Onde a SRAG pesa mais",
         texto = sprintf(
           "Nas últimas 52 semanas o Brasil teve %s casos de SRAG por 100 mil habitantes. Maiores incidências: %s; menores: %s. Parte da diferença é acesso a hospital e à notificação, não só transmissão - SRAG é caso internado.",
           fmt_num(inc_br, 0),
           paste(sprintf("%s (%s)", sig(utils::head(inc_uf$uf_ibge, 4)), vapply(utils::head(inc_uf$inc, 4), fmt_num, "", 0)), collapse = ", "),
           paste(sprintf("%s (%s)", sig(utils::tail(inc_uf$uf_ibge, 3)), vapply(utils::tail(inc_uf$inc, 3), fmt_num, "", 0)), collapse = ", ")))
  )
}

ACHADOS <- tryCatch(calcular_achados(), error = function(e) {
  warning("[Gripe Brasil] Achados nao calculados: ", conditionMessage(e), call. = FALSE)
  list()
})

# ----------------------------------------------------------------------- UI --
mod_analises_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(class = "bg-primary text-white fw-semibold",
                           shiny::textOutput(ns("titulo_ranking"), inline = TRUE)),
        bslib::card_body(DT::DTOutput(ns("ranking")))
      ),
      cartao_grafico(
        shiny::textOutput(ns("titulo_agentes"), inline = TRUE),
        echarts4r::echarts4rOutput(ns("agentes"), height = "560px"),
        rodape = "Casos de SRAG nas últimas 52 semanas, por agente. No Brasil, as 27 UFs; num estado, os 20 municípios com mais casos. Ignora o filtro de agente."
      )
    ),
    bslib::card(
      bslib::card_header(class = "bg-primary text-white fw-semibold",
                         "Achados da análise exploratória — Brasil"),
      bslib::card_body(
        bslib::layout_column_wrap(
          width = "380px",
          !!!lapply(ACHADOS, function(a) shiny::div(
            class = "insight",
            shiny::tags$h6(a$titulo),
            shiny::tags$p(class = "small", a$texto)
          ))
        )
      )
    )
  )
}

mod_analises_server <- function(id, geo, agente) {
  shiny::moduleServer(id, function(input, output, session) {

    chave_un <- shiny::reactive({
      g <- geo()
      if (identical(g$nivel, "base")) "base" else paste0("uf_", g$uf_ibge)
    })
    un <- shiny::reactive(unidades_do_recorte(geo())) |> shiny::bindEvent(chave_un())
    nome_rec <- function(u) if (identical(u$tipo, "uf")) "Estados" else
      sprintf("Municípios de %s", UF_LOOKUP$uf_nome[UF_LOOKUP$uf_ibge == u$uf])

    output$titulo_ranking <- shiny::renderText(
      sprintf("%s — %s", nome_rec(un()), rotulo_filtro_agente(agente())))
    output$titulo_agentes <- shiny::renderText(
      sprintf("Agentes por %s (últimas 52 semanas)", un()$rotulo))

    output$ranking <- DT::renderDT({
      u <- un()
      d <- casos_por_unidade(u, agentes_do_filtro(agente()))
      pop <- pop_unidades(u)
      pop <- pop[pop$ano == max(pop$ano), c("id", "pop")]
      t <- d |>
        dplyr::group_by(id) |>
        dplyr::summarise(
          c52  = sum(casos[data_ini_se > DATA_MAX - 52L * 7L]),
          o52  = sum(extra[data_ini_se > DATA_MAX - 52L * 7L]),
          u4   = sum(casos[data_ini_se > DATA_MAX - 4L * 7L]),
          a4   = sum(casos[data_ini_se <= DATA_MAX - 4L * 7L & data_ini_se > DATA_MAX - 8L * 7L]),
          .groups = "drop")
      u$dict |>
        dplyr::select(id, nome, sigla) |>
        dplyr::left_join(t, by = "id") |>
        dplyr::left_join(pop, by = "id") |>
        dplyr::mutate(dplyr::across(c(c52, o52, u4, a4), ~ dplyr::coalesce(.x, 0))) |>
        dplyr::transmute(
          !!(if (identical(u$tipo, "uf")) "Estado" else "Município") := nome, UF = sigla,
          `Casos (52 sem.)` = c52,
          `Incid. /100 mil` = round(c52 / pop * 1e5, 1),
          `Óbitos (52 sem.)` = o52,
          `Últimas 4 sem.` = u4,
          `4 sem. anteriores` = a4
        ) |>
        dplyr::arrange(dplyr::desc(`Incid. /100 mil`)) |>
        DT::datatable(
          rownames = FALSE, options = list(pageLength = 12, dom = "ftp",
                                           language = list(search = "Buscar:",
                                                           paginate = list(previous = "Anterior", `next` = "Próxima"))),
          caption = "Últimas 4 semanas sem nowcast: compare com cautela (digitação incompleta)."
        ) |>
        DT::formatRound("Incid. /100 mil", digits = 1, mark = ".", dec.mark = ",") |>
        DT::formatStyle("Incid. /100 mil",
                        background = DT::styleColorBar(c(0, 400), TONS_GRIPE[["200"]]),
                        backgroundSize = "98% 70%", backgroundRepeat = "no-repeat",
                        backgroundPosition = "center")
    })

    output$agentes <- echarts4r::renderEcharts4r({
      u <- un()
      ini <- DATA_MAX - 51L * 7L
      d <- if (identical(u$tipo, "uf")) {
        DADOS[DADOS$data_ini_se >= ini, , drop = FALSE] |>
          dplyr::group_by(id = uf_ibge, agente) |>
          dplyr::summarise(casos = sum(casos), .groups = "drop")
      } else {
        uf <- u$uf
        DS_MUN |>
          dplyr::filter(uf_ibge == uf, data_ini_se >= ini) |>
          dplyr::group_by(id = municipio_geocodigo, agente) |>
          dplyr::summarise(casos = sum(casos)) |>
          dplyr::collect() |>
          dplyr::mutate(id = as.integer(id), casos = as.numeric(casos))
      }
      shiny::validate(shiny::need(nrow(d) > 0, "Sem casos nas últimas 52 semanas."))
      d$grupo <- AGENTES$grupo[match(d$agente, AGENTES$agente)]
      d$grupo[d$grupo %in% c("nao_especificado", "em_investigacao")] <- "sem_agente"
      tot <- d |> dplyr::group_by(id) |> dplyr::summarise(t = sum(casos), .groups = "drop") |>
        dplyr::arrange(dplyr::desc(t)) |> utils::head(if (identical(u$tipo, "uf")) 27L else 20L)
      larga <- d[d$id %in% tot$id, ] |>
        dplyr::group_by(id, grupo) |>
        dplyr::summarise(casos = sum(casos), .groups = "drop") |>
        tidyr::pivot_wider(names_from = grupo, values_from = casos, values_fill = 0) |>
        dplyr::left_join(u$dict[, c("id", "nome", "sigla")], by = "id") |>
        dplyr::left_join(tot, by = "id") |>
        dplyr::arrange(t)
      larga$x <- if (identical(u$tipo, "uf")) larga$sigla else larga$nome
      grupos <- c("influenza", "vsr", "covid", "rinovirus", "metapneumovirus", "adenovirus",
                  "parainfluenza", "outros_virus", "outro", "sem_agente")
      grupos <- grupos[grupos %in% names(larga)]
      rot <- c(ROTULO_GRUPO, sem_agente = "Sem agente / em investigação")
      cor <- c(influenza = COR_AGENTE[["influenza_a"]], vsr = COR_AGENTE[["vsr"]],
               covid = COR_AGENTE[["covid"]], rinovirus = COR_AGENTE[["rinovirus"]],
               metapneumovirus = COR_AGENTE[["metapneumovirus"]], adenovirus = COR_AGENTE[["adenovirus"]],
               parainfluenza = COR_AGENTE[["parainfluenza"]], outros_virus = COR_AGENTE[["outros_virus"]],
               outro = COR_AGENTE[["outro_agente"]], sem_agente = COR_AGENTE[["nao_especificado"]])
      e <- larga |> echarts4r::e_charts(x)
      for (g in grupos) e <- e |> echarts4r::e_bar_(g, name = rot[[g]], stack = "a")
      e |>
        echarts4r::e_flip_coords() |>
        echarts4r::e_color(unname(cor[grupos])) |>
        echarts4r::e_tooltip(trigger = "axis", axisPointer = list(type = "shadow"), confine = TRUE) |>
        echarts4r::e_legend(type = "scroll", bottom = 0) |>
        echarts4r::e_grid(left = if (identical(u$tipo, "uf")) 40 else 150, right = 20, top = 10, bottom = 50) |>
        echarts4r::e_y_axis(axisLabel = list(fontSize = 10)) |>
        e_padrao()
    })
  })
}


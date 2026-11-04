# ==============================================================================
# Gripe Brasil | Modulo: KPIs
#
#   1. casos no ano corrente x mesmo periodo do ano anterior
#   2. ultimas 4 semanas ENCERRADAS, ja com nowcast, x as 4 anteriores
#      (a semana corrente fica de fora: com ~20% dos casos digitados, a
#      correcao dela e instavel demais para dar tendencia)
#   3. obitos e letalidade no ano
#   4. proporcao de casos que foram para UTI no ano
#   5. agente predominante entre os casos COM agente identificado nas
#      ultimas 8 semanas - ignora o filtro de agente de proposito: responde
#      "o que esta circulando agora" no recorte.
# ==============================================================================

mod_kpi_ui <- function(id) shiny::uiOutput(shiny::NS(id)("boxes"))

mod_kpi_server <- function(id, geo, agente, serie) {
  shiny::moduleServer(id, function(input, output, session) {

    tom <- function(bg, claro = FALSE) {
      bslib::value_box_theme(bg = bg, fg = if (claro) PALETA[["tinta"]] else "#FFFFFF")
    }

    output$boxes <- shiny::renderUI({
      s <- serie()
      shiny::req(nrow(s) > 0)

      ult_se   <- max(s$se)
      ano_cor  <- ult_se %/% 100L
      sem_cor  <- ult_se %% 100L
      no_ano   <- s$ano == ano_cor
      ant_per  <- s$ano == ano_cor - 1L & s$semana <= sem_cor
      casos_ano <- sum(s$casos[no_ano])
      casos_ant <- sum(s$casos[ant_per])
      var_ano   <- if (casos_ant > 0) casos_ano / casos_ant - 1 else NA_real_

      est   <- dplyr::coalesce(s$casos_est, s$casos)
      n     <- nrow(s)
      ult4  <- sum(est[(n - 4):(n - 1)])
      ant4  <- sum(est[(n - 8):(n - 5)])
      var4  <- if (ant4 > 0) ult4 / ant4 - 1 else NA_real_

      obitos_ano <- sum(s$obitos[no_ano])
      letal      <- if (casos_ano > 0) obitos_ano / casos_ano else NA_real_
      uti_ano    <- if (casos_ano > 0) sum(s$uti[no_ano]) / casos_ano else NA_real_

      ident <- filtrar(geo(), setdiff(AGENTES$agente, c("nao_especificado", "em_investigacao")))
      ident <- ident[ident$data_ini_se > DATA_MAX - 8L * 7L, , drop = FALSE]
      pred <- if (nrow(ident)) {
        g <- tapply(ident$casos, AGENTES$grupo[match(ident$agente, AGENTES$agente)], sum)
        g <- sort(g, decreasing = TRUE)
        list(nome = ROTULO_GRUPO[[names(g)[1]]], pct = g[[1]] / sum(g), n = sum(g))
      } else NULL

      seta <- function(v) {
        if (is.na(v)) return("")
        sprintf("%s %s", if (v >= 0) "▲" else "▼", fmt_pct(abs(v), 0))
      }

      bslib::layout_column_wrap(
        width = "190px", fill = FALSE, class = "mb-3",
        bslib::value_box(
          title = sprintf("Casos de SRAG em %d", ano_cor),
          value = fmt_int(casos_ano),
          shiny::tags$small(sprintf("%s vs. mesmo período de %d (%s)",
                                    seta(var_ano), ano_cor - 1L, fmt_int(casos_ant))),
          theme = tom(TONS_GRIPE[["600"]])
        ),
        bslib::value_box(
          title = "Últimas 4 semanas (nowcast)",
          value = fmt_int(ult4),
          shiny::tags$small(sprintf("%s vs. 4 semanas anteriores", seta(var4))),
          theme = if (!is.na(var4) && var4 > 0.15) tom(TONS_GRIPE[["800"]])
                  else tom(TONS_GRIPE[["100"]], claro = TRUE)
        ),
        bslib::value_box(
          title = sprintf("Óbitos em %d", ano_cor),
          value = fmt_int(obitos_ano),
          shiny::tags$small(sprintf("letalidade %s (casos recentes ainda sem desfecho)",
                                    fmt_pct(letal))),
          theme = tom(PALETA[["critico"]])
        ),
        bslib::value_box(
          title = sprintf("Internados em UTI em %d", ano_cor),
          value = fmt_pct(uti_ano),
          shiny::tags$small(sprintf("%s casos", fmt_int(sum(s$uti[no_ano])))),
          theme = tom(TONS_AZUL[["700"]])
        ),
        bslib::value_box(
          title = "Agente predominante (8 sem.)",
          value = if (is.null(pred)) "—" else pred$nome,
          shiny::tags$small(if (is.null(pred)) "sem casos com agente identificado"
                            else sprintf("%s dos %s casos com agente identificado",
                                         fmt_pct(pred$pct, 0), fmt_int(pred$n))),
          theme = tom(TONS_AZUL[["100"]], claro = TRUE)
        )
      )
    })
  })
}

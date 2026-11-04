# ==============================================================================
# Gripe Brasil | Modulo: perfil etario e gravidade
#
#   "Faixa etária": casos por faixa, empilhados por agente. Mostra quem cada
#     virus atinge: VSR concentrado em < 2 anos, covid em 65+, influenza
#     espalhada por todas as idades.
#   "Gravidade": letalidade (obitos / casos) e % UTI por faixa, para o agente
#     filtrado. Casos recentes ainda sem desfecho puxam a letalidade para
#     baixo - por isso o periodo padrao exclui as ultimas 8 semanas.
# ==============================================================================

mod_perfil_controles <- function(id) {
  ns <- shiny::NS(id)
  shiny::radioButtons(ns("periodo"), "Período",
                      c("Últimas 52 semanas" = "52", "Desde 2022 (pós-pandemia)" = "2022",
                        "Série completa" = "tudo"), selected = "52")
}

mod_perfil_saida <- function(id, qual = c("faixa", "gravidade"), height = "330px") {
  qual <- match.arg(qual)
  echarts4r::echarts4rOutput(shiny::NS(id)(qual), height = height)
}

mod_perfil_server <- function(id, geo, agente) {
  shiny::moduleServer(id, function(input, output, session) {

    base <- shiny::reactive({
      ini <- switch(input$periodo %||% "52",
                    "52" = DATA_MAX - 51L * 7L, "2022" = as.Date("2022-01-02"), DATA_MIN)
      d <- filtrar(geo(), agentes_do_filtro(agente()))
      d[d$data_ini_se >= ini & d$faixa_etaria %in% FAIXAS, , drop = FALSE]
    })

    output$faixa <- echarts4r::renderEcharts4r({
      d <- base()
      shiny::validate(shiny::need(nrow(d) > 0, "Sem casos no período."))
      larga <- d |>
        dplyr::group_by(faixa_etaria, agente) |>
        dplyr::summarise(casos = sum(casos), .groups = "drop") |>
        tidyr::pivot_wider(names_from = agente, values_from = casos, values_fill = 0)
      larga <- data.frame(faixa_etaria = FAIXAS) |> dplyr::left_join(larga, by = "faixa_etaria")
      ags <- AGENTES$agente[AGENTES$agente %in% names(larga)]
      larga[ags] <- lapply(larga[ags], function(x) dplyr::coalesce(x, 0))
      larga$x <- unname(ROTULO_FAIXA[larga$faixa_etaria])

      e <- larga |> echarts4r::e_charts(x)
      for (a in ags) e <- e |> echarts4r::e_bar_(a, name = ROTULO_AGENTE[[a]], stack = "a")
      e |>
        echarts4r::e_color(unname(COR_AGENTE[ags])) |>
        echarts4r::e_tooltip(trigger = "axis", axisPointer = list(type = "shadow"), confine = TRUE) |>
        echarts4r::e_legend(type = "scroll", bottom = 0) |>
        echarts4r::e_y_axis(name = "Casos") |>
        echarts4r::e_grid(left = 55, right = 15, top = 30, bottom = 60) |>
        e_padrao()
    })

    output$gravidade <- echarts4r::renderEcharts4r({
      d <- base()
      # Exclui as 8 semanas mais recentes: desfecho ainda em aberto.
      d <- d[d$data_ini_se <= DATA_MAX - 8L * 7L, , drop = FALSE]
      shiny::validate(shiny::need(nrow(d) > 0, "Sem casos encerrados no período."))
      g <- d |>
        dplyr::group_by(faixa_etaria) |>
        dplyr::summarise(casos = sum(casos), letal = round(100 * sum(obitos) / casos, 1),
                         uti = round(100 * sum(uti) / casos, 1), .groups = "drop")
      g <- data.frame(faixa_etaria = FAIXAS) |> dplyr::left_join(g, by = "faixa_etaria")
      g$x <- unname(ROTULO_FAIXA[g$faixa_etaria])

      g |>
        echarts4r::e_charts(x) |>
        echarts4r::e_bar(uti, name = "% em UTI") |>
        echarts4r::e_bar(letal, name = "Letalidade (%)") |>
        echarts4r::e_color(c(TONS_AZUL[["400"]], PALETA[["critico"]])) |>
        echarts4r::e_tooltip(trigger = "axis", axisPointer = list(type = "shadow"), confine = TRUE,
                             valueFormatter = htmlwidgets::JS("function(v){return v + '%';}")) |>
        echarts4r::e_legend(bottom = 0) |>
        echarts4r::e_y_axis(name = "%") |>
        echarts4r::e_grid(left = 45, right = 15, top = 30, bottom = 50) |>
        e_padrao()
    })
  })
}

# ==============================================================================
# Gripe Brasil | Modulo: evolucao semanal por agente
#
# Barras empilhadas por agente etiologico (casos ou incidencia por semana de
# inicio dos sintomas) + linha do nowcast sobre o total das ultimas semanas,
# com faixa de incerteza. Na visao "Proporção" as barras viram 100% e mostram
# a composicao etiologica - util para ver a troca de agentes (VSR no outono,
# influenza no inverno, covid em ondas) sem o volume total mascarar.
# ==============================================================================

mod_serie_controles <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::radioButtons(ns("visao"), "Visão",
                        c("Casos" = "casos", "Incidência /100 mil" = "inc",
                          "Proporção por agente (%)" = "prop"),
                        selected = "casos"),
    shiny::radioButtons(ns("periodo"), "Período",
                        c("Últimas 52 semanas" = "52", "Últimas 104 semanas" = "104",
                          "Série completa" = "tudo"),
                        selected = "104"),
    shiny::checkboxInput(ns("nowcast"), "Mostrar nowcast (atraso de digitação)", TRUE)
  )
}

mod_serie_saida <- function(id, height = "380px") {
  echarts4r::echarts4rOutput(shiny::NS(id)("grafico"), height = height)
}

mod_serie_server <- function(id, geo, agente, serie) {
  shiny::moduleServer(id, function(input, output, session) {

    output$grafico <- echarts4r::renderEcharts4r({
      ags <- agentes_do_filtro(agente())
      d <- filtrar(geo(), ags) |>
        dplyr::group_by(data_ini_se, agente) |>
        dplyr::summarise(casos = sum(casos), .groups = "drop")

      ini <- switch(input$periodo %||% "104",
                    "52" = DATA_MAX - 51L * 7L, "104" = DATA_MAX - 103L * 7L, DATA_MIN)

      tot <- serie() |> dplyr::filter(data_ini_se >= ini)
      larga <- tot |>
        dplyr::select(data_ini_se, ano, pop) |>
        dplyr::left_join(tidyr::pivot_wider(d, names_from = agente, values_from = casos,
                                            values_fill = 0),
                         by = "data_ini_se")
      ags <- AGENTES$agente[AGENTES$agente %in% names(larga)]
      shiny::validate(shiny::need(length(ags) > 0, "Sem casos para este recorte e agente."))
      larga[ags] <- lapply(larga[ags], function(x) dplyr::coalesce(x, 0))

      visao <- input$visao %||% "casos"
      fator <- switch(visao, inc = 1e5 / larga$pop, prop = 100 / pmax(rowSums(larga[ags]), 1), 1)
      larga[ags] <- lapply(larga[ags], function(x) round(x * fator, 2))
      larga$x <- as.character(larga$data_ini_se)

      # Nowcast sobre o TOTAL do recorte+agente (as barras sao o observado).
      com_nc <- isTRUE(input$nowcast) && visao != "prop" && any(!is.na(tot$casos_est))
      if (com_nc) {
        f <- if (visao == "inc") 1e5 / tot$pop else 1
        larga$nc_est <- round(tot$casos_est * f, 2)
        larga$nc_lo  <- round(tot$casos_est_min * f, 2)
        larga$nc_dif <- round((tot$casos_est_max - tot$casos_est_min) * f, 2)
        # O nowcast so existe nas ultimas semanas; a linha parte do ultimo
        # ponto observado anterior para nao "nascer" do nada.
        ancora <- min(which(!is.na(larga$nc_est))) - 1L
        if (ancora >= 1L) {
          larga$nc_est[ancora] <- round(tot$casos[ancora] * f[min(ancora, length(f))], 2)
        }
      }

      e <- larga |> echarts4r::e_charts(x)
      for (a in ags) {
        e <- e |> echarts4r::e_bar_(a, name = ROTULO_AGENTE[[a]], stack = "agentes",
                                    barCategoryGap = "15%")
      }
      cores <- unname(COR_AGENTE[ags])

      if (com_nc) {
        e <- e |>
          echarts4r::e_line_("nc_lo", name = "Faixa nowcast", stack = "ic", symbol = "none",
                             lineStyle = list(opacity = 0), areaStyle = list(opacity = 0),
                             tooltip = list(show = FALSE)) |>
          echarts4r::e_line_("nc_dif", name = "Faixa nowcast", stack = "ic", symbol = "none",
                             lineStyle = list(opacity = 0),
                             areaStyle = list(color = cor_alpha(PALETA[["nowcast"]], 0.15)),
                             tooltip = list(show = FALSE)) |>
          echarts4r::e_line_("nc_est", name = "Estimado (nowcast)", symbol = "circle",
                             symbolSize = 5, lineStyle = list(type = "dashed", width = 2))
        cores <- c(cores, rep(PALETA[["nowcast"]], 3))
      }

      eixo_y <- switch(visao, casos = "Casos", inc = "Incidência /100 mil", prop = "%")
      e <- e |>
        echarts4r::e_color(cores) |>
        echarts4r::e_tooltip(trigger = "axis", axisPointer = list(type = "shadow"),
                             confine = TRUE) |>
        echarts4r::e_legend(type = "scroll", bottom = 0) |>
        echarts4r::e_x_axis(axisLabel = list(formatter = htmlwidgets::JS(
          sprintf("function(v){return %s(v);}", JS_DATA_BR)))) |>
        echarts4r::e_y_axis(name = eixo_y, max = if (visao == "prop") 100 else NULL) |>
        echarts4r::e_grid(left = 55, right = 20, top = 30, bottom = 70) |>
        echarts4r::e_datazoom(type = "inside") |>
        e_padrao()
      # A faixa do nowcast sao duas series auxiliares (base invisivel +
      # largura): fora da legenda. e_legend() refaz `data` a partir das
      # series, entao a lista e imposta depois.
      e$x$opts$legend$data <- as.list(c(unname(ROTULO_AGENTE[ags]),
                                        if (com_nc) "Estimado (nowcast)"))
      e
    })
  })
}

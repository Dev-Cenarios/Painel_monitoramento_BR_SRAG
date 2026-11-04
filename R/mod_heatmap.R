# ==============================================================================
# Gripe Brasil | Modulo: sazonalidade (ano x semana epidemiologica)
#
# Incidencia semanal do recorte+agente. E onde aparece a regularidade
# sazonal (faixa escura entre as SE 10 e 25 todo ano) e as excecoes: as ondas
# de covid de 2020-2022 e a influenza A(H3N2) fora de epoca na virada
# 2021/2022.
# ==============================================================================

mod_heatmap_controles <- function(id) {
  ns <- shiny::NS(id)
  shiny::radioButtons(ns("metrica"), "Métrica",
                      c("Incidência /100 mil" = "inc", "Casos" = "casos"), selected = "inc")
}

mod_heatmap_saida <- function(id, height = "330px") {
  echarts4r::echarts4rOutput(shiny::NS(id)("grafico"), height = height)
}

mod_heatmap_server <- function(id, serie) {
  shiny::moduleServer(id, function(input, output, session) {
    output$grafico <- echarts4r::renderEcharts4r({
      s <- serie()
      met <- input$metrica %||% "inc"
      s$valor <- round(if (met == "inc") s$incidencia else s$casos, 2)
      s$semana <- pmin(s$semana, 52L)
      d <- s |>
        dplyr::group_by(ano, semana) |>
        dplyr::summarise(valor = sum(valor), .groups = "drop") |>
        dplyr::mutate(x = sprintf("%02d", semana), y = as.character(ano), valor = round(valor, 2)) |>
        dplyr::arrange(semana, ano)

      # Escala pelos anos pos-pandemia: com 2020-2021 no maximo, a covid
      # (10-20x a SRAG usual) apagaria todo o resto em tons claros. Acima do
      # teto a cor satura (o tooltip mostra o valor real).
      teto <- max(d$valor[d$ano >= 2022], na.rm = TRUE)
      if (!is.finite(teto) || teto <= 0) teto <- max(d$valor, na.rm = TRUE)

      e <- d |>
        echarts4r::e_charts(x) |>
        echarts4r::e_heatmap(y, valor, name = if (met == "inc") "Incidência /100 mil" else "Casos",
                             itemStyle = list(borderColor = "#FFFFFF", borderWidth = 1)) |>
        echarts4r::e_visual_map(valor, inRange = list(color = ESCALA_MAPA),
                                orient = "horizontal", left = "center", bottom = 0,
                                calculable = FALSE, itemHeight = 160,
                                text = list("máx. 2022+ (acima satura)", "0")) |>
        echarts4r::e_tooltip(formatter = htmlwidgets::JS(
          "function(p){return 'SE ' + p.value[0] + '/' + p.value[1] + '<br/>' +
             p.seriesName + ': ' + Number(p.value[2]).toLocaleString('pt-BR',{maximumFractionDigits:1});}")) |>
        echarts4r::e_x_axis(name = "SE", nameLocation = "middle", nameGap = 22) |>
        echarts4r::e_grid(left = 50, right = 15, top = 10, bottom = 80) |>
        e_padrao()
      # e_visual_map() recalcula min/max a partir da serie; o teto e imposto
      # depois. No visualMap continuo, valor acima do max recebe a cor maxima.
      e$x$opts$visualMap[[1]]$min <- 0
      e$x$opts$visualMap[[1]]$max <- round(teto, 1)
      e
    })
  })
}

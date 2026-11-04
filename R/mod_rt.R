# ==============================================================================
# Gripe Brasil | Modulo: Numero de Reproducao Efetivo (Rt)
#
# Mesmo grafico do painel de dengue (linha de Rt com limiar em 1,0), mas o Rt
# e estimado pelo proprio painel - nao ha Rt de SRAG no banco. Metodo em
# estimar_rt() no global.R: taxa de crescimento entre janelas de 2 semanas
# convertida em Rt pelo intervalo de geracao do agente selecionado.
#
# Diferente do Rt do InfoDengue (um por municipio, media ponderada acima
# dele), este e calculado direto da serie agregada do recorte, entao vale
# igual para municipio, estado e Brasil - e segue o filtro de agente.
#
# Faixa = IC 95% (erro de Poisson). Linha tracejada = semanas corrigidas pelo
# nowcast (preliminares).
# ==============================================================================

RT_TETO_VISUAL <- 3

mod_rt_controles <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::radioButtons(ns("periodo"), "Período",
                        c("Últimas 52 semanas" = "52", "Últimas 104 semanas" = "104",
                          "Série completa" = "tudo"),
                        selected = "104"),
    shiny::checkboxInput(ns("limitar_escala"),
                         sprintf("Limitar eixo a Rt = %d", RT_TETO_VISUAL), value = TRUE),
    shiny::tags$hr(),
    shiny::uiOutput(ns("nota_metodo"))
  )
}

mod_rt_saida <- function(id, height = "320px") {
  echarts4r::echarts4rOutput(shiny::NS(id)("grafico"), height = height)
}

mod_rt_server <- function(id, agente, serie) {
  shiny::moduleServer(id, function(input, output, session) {

    dados <- shiny::reactive({
      d <- estimar_rt(serie(), agente())
      ini <- switch(input$periodo %||% "104",
                    "52" = DATA_MAX - 51L * 7L, "104" = DATA_MAX - 103L * 7L, DATA_MIN)
      d[d$data_ini_se >= ini, , drop = FALSE]
    })

    output$nota_metodo <- shiny::renderUI({
      ig <- intervalo_geracao(agente())
      shiny::tags$div(
        class = "small text-muted",
        shiny::tags$strong("Rt estimado pelo painel."), shiny::tags$br(),
        sprintf("Crescimento entre janelas de 2 semanas, intervalo de geração de %s dias (desvio %s). Janelas com menos de %d casos ficam sem estimativa.",
                fmt_num(ig[["media"]]), fmt_num(ig[["dp"]]), RT_MIN_CASOS)
      )
    })

    output$grafico <- echarts4r::renderEcharts4r({
      d <- dados()
      shiny::req(nrow(d) > 0)
      shiny::validate(shiny::need(any(!is.na(d$rt)),
        sprintf("Casos insuficientes para estimar o Rt (mínimo de %d casos em 4 semanas).", RT_MIN_CASOS)))

      d$x <- as.character(d$data_ini_se)
      d$limiar  <- 1
      d$rt_cons <- ifelse(d$rt_prelim, NA_real_, round(d$rt, 2))
      d$rt_prel <- ifelse(d$rt_prelim, round(d$rt, 2), NA_real_)
      # a linha preliminar parte do ultimo ponto consolidado
      i_ini <- which(d$rt_prelim)[1]
      if (!is.na(i_ini) && i_ini > 1L) d$rt_prel[i_ini - 1L] <- d$rt_cons[i_ini - 1L]
      teto <- if (isTRUE(input$limitar_escala)) RT_TETO_VISUAL else max(c(d$rt_max, 1.5), na.rm = TRUE)
      d$ic_lo  <- round(pmin(d$rt_min, teto), 2)
      d$ic_dif <- round(pmin(d$rt_max, teto) - pmin(d$rt_min, teto), 2)

      tabela_js <- jsonlite::toJSON(
        data.frame(rt = round(d$rt, 2), lo = round(d$rt_min, 2), hi = round(d$rt_max, 2),
                   n = d$rt_casos, p = d$rt_prelim),
        dataframe = "rows", na = "null", auto_unbox = TRUE, digits = 2
      )
      js_tooltip <- htmlwidgets::JS(sprintf(
        "function(params){
           if (!params || !params.length) return '';
           var tab = %s, r = tab[params[0].dataIndex];
           if (!r) return '';
           function f(x, d){ return (x === null || x === undefined) ? '—' :
             x.toLocaleString('pt-BR', {maximumFractionDigits: d}); }
           var cor = (r.rt !== null && r.rt > 1) ? '%s' : '%s';
           var txt = '<b>' + %s(params[0].axisValue) + '</b><br/>' +
                     'Rt: <b style=\"color:' + cor + '\">' + f(r.rt, 2) + '</b>' +
                     (r.rt === null ? '' : ' (IC 95%%: ' + f(r.lo, 2) + '–' + f(r.hi, 2) + ')') +
                     '<br/>Casos nas 4 semanas: ' + f(r.n, 0);
           if (r.p) txt += '<br/><span style=\"opacity:.7\">Preliminar (nowcast)</span>';
           return txt;
         }",
        tabela_js, PALETA[["critico"]], PALETA[["azul_titulo"]], JS_DATA_BR
      ))

      e <- d |>
        echarts4r::e_charts(x) |>
        echarts4r::e_line(ic_lo, name = "IC 95%", stack = "ic", symbol = "none",
                          lineStyle = list(opacity = 0), areaStyle = list(opacity = 0),
                          tooltip = list(show = FALSE)) |>
        echarts4r::e_line(ic_dif, name = "IC 95%", stack = "ic", symbol = "none",
                          lineStyle = list(opacity = 0),
                          areaStyle = list(color = cor_alpha(PALETA[["gripe"]], 0.18)),
                          tooltip = list(show = FALSE)) |>
        echarts4r::e_line(rt_cons, name = "Rt", symbol = "none",
                          lineStyle = list(color = PALETA[["gripe"]], width = 2),
                          itemStyle = list(color = PALETA[["gripe"]])) |>
        echarts4r::e_line(rt_prel, name = "Rt preliminar (nowcast)", symbol = "circle", symbolSize = 4,
                          lineStyle = list(color = PALETA[["gripe"]], width = 2, type = "dashed"),
                          itemStyle = list(color = PALETA[["gripe"]])) |>
        echarts4r::e_line(limiar, name = "Limiar (Rt = 1)", symbol = "none", silent = TRUE,
                          lineStyle = list(color = PALETA[["tinta"]], width = 1.5, type = "dashed"),
                          itemStyle = list(color = PALETA[["tinta"]]),
                          tooltip = list(show = FALSE)) |>
        echarts4r::e_x_axis(axisLabel = list(formatter = htmlwidgets::JS(
          sprintf("function(v){return %s(v);}", JS_DATA_BR)))) |>
        echarts4r::e_y_axis(name = "Rt", nameLocation = "end", nameGap = 12, min = 0, max = teto) |>
        echarts4r::e_datazoom(x_index = 0, type = "slider", bottom = 4) |>
        echarts4r::e_legend(top = 0, type = "scroll") |>
        echarts4r::e_grid(left = 50, right = 24, top = 40, bottom = 78) |>
        echarts4r::e_tooltip(trigger = "axis", formatter = js_tooltip) |>
        e_padrao()
      # A faixa sao duas series auxiliares (base invisivel + largura): uma so
      # entrada na legenda.
      e$x$opts$legend$data <- list("Rt", "Rt preliminar (nowcast)", "IC 95%", "Limiar (Rt = 1)")
      e
    })
  })
}

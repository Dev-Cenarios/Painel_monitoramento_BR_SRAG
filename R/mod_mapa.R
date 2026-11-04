# ==============================================================================
# Modulo: mapa de incidencia (UFs no Brasil, municipios num estado)
#
# Visao Brasil: as 27 UFs (BR_UF). Clique numa UF -> recorte no estado.
# Estado ou municipio: os municipios da UF (MUN_<uf>), com o municipio em
# foco destacado. Clique num municipio -> recorte no municipio. "Visao
# geral" volta ao Brasil. Respeita o filtro de CLASSIFICACAO.
#
# Incidencia acumulada no periodo = casos / pop * 100 mil, com a populacao do
# ano mais recente do periodo.
#
# Bordas: municipios em cinza claro e, por cima, o contorno da UF (anexado a
# malha por malha_municipios(), codarea "CONTORNO") - sem isso, municipio na
# cor mais clara da escala some no fundo branco do card.
# ==============================================================================

COR_BORDA_MUN  <- "#CBD5DB"   # entre municipios
COR_BORDA_UF   <- "#94A3AC"   # divisa entre estados / contorno da UF

#' Desenha o contorno da UF por cima do mapa municipal: item de dado sem
#' valor (o visualMap ignora), sem preenchimento e `silent` (clique, hover e
#' tooltip atravessam para o municipio de baixo) - so a borda.
com_contorno <- function(e) {
  e$x$opts$series[[1]]$data <- c(e$x$opts$series[[1]]$data, list(list(
    name = "CONTORNO", value = NULL,
    itemStyle = list(areaColor = "none", borderColor = COR_BORDA_UF, borderWidth = 1.2),
    emphasis = list(disabled = TRUE), select = list(disabled = TRUE),
    tooltip = list(show = FALSE), silent = TRUE
  )))
  e
}

mod_mapa_controles <- function(id, metricas = stats::setNames(c("inc", "casos", "hosp"),
                                   c("Incidência /100 mil", "Casos", ROTULO_SECUNDARIA)),
                               periodo = "52") {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::radioButtons(ns("metrica"), "Métrica", metricas, selected = metricas[[1]]),
    shiny::radioButtons(ns("periodo"), "Período",
                        c("Últimas 12 semanas" = "12", "Últimas 52 semanas" = "52",
                          "Ano corrente" = "ano", "Série completa" = "tudo"), selected = periodo),
    shiny::helpText(shiny::tags$small(
      "No Brasil, clique em um estado para abrir os municípios dele; no estado, clique em um município para focar o painel nele."))
  )
}

mod_mapa_saida <- function(id, height = "460px") {
  ns <- shiny::NS(id)
  shiny::div(
    class = "position-relative",
    shiny::actionButton(ns("limpar"), "Visão geral", icon = shiny::icon("rotate-left"),
                        class = "btn btn-sm btn-light shadow-sm",
                        style = "position:absolute; top:8px; left:8px; z-index:5; padding:.15rem .5rem; font-size:.8rem;"),
    echarts4r::echarts4rOutput(ns("mapa"), height = height)
  )
}

#' @param classes reactive com o vetor de classes (ou agentes) do filtro.
#' @param rotulo_hosp rotulo da segunda medida no tooltip/metrica "hosp".
#' @param casos_unidade funcao(un, classes) -> id, data_ini_se, casos,
#   extra (padrao: casos_por_unidade do global.R).
mod_mapa_server <- function(id, geo, classes, rotulo_hosp = ROTULO_SECUNDARIA,
                            casos_unidade = casos_por_unidade) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # So o que define as unidades (Brasil x UF): trocar de municipio dentro
    # da mesma UF nao recalcula o mapa.
    chave_un <- shiny::reactive({
      g <- geo()
      if (identical(g$nivel, "base")) "base" else paste0("uf_", g$uf_ibge)
    })
    un <- shiny::reactive(unidades_do_recorte(geo())) |> shiny::bindEvent(chave_un())

    dados <- shiny::reactive({
      u <- un()
      ini <- switch(input$periodo %||% "52",
                    "12" = DATA_MAX - 11L * 7L, "52" = DATA_MAX - 51L * 7L, "tudo" = DATA_MIN,
                    as.Date(sprintf("%d-01-01", ANO_MAX)) - 3L)
      d <- casos_unidade(u, classes())
      agg <- d[d$data_ini_se >= ini, , drop = FALSE] |>
        dplyr::group_by(id) |>
        dplyr::summarise(casos = sum(casos), hosp = sum(extra), .groups = "drop")
      pop <- pop_unidades(u)
      pop <- pop[pop$ano == max(pop$ano), c("id", "pop")]
      u$dict |>
        dplyr::select(id, nome, sigla) |>
        dplyr::left_join(agg, by = "id") |>
        dplyr::left_join(pop, by = "id") |>
        dplyr::mutate(casos = dplyr::coalesce(casos, 0), hosp = dplyr::coalesce(hosp, 0),
                      inc = round(casos / pop * 1e5, 1),
                      chave = as.character(id))
    })

    output$mapa <- echarts4r::renderEcharts4r({
      u <- un()
      d <- dados()
      met <- input$metrica %||% "inc"
      d$valor <- d[[met]]
      rot <- c(inc = "Incidência /100 mil", casos = "Casos", hosp = rotulo_hosp)[[met]]
      eh_uf <- identical(u$tipo, "uf")

      info <- jsonlite::toJSON(
        stats::setNames(lapply(seq_len(nrow(d)), function(i) list(
          n = if (eh_uf) d$nome[i] else sprintf("%s (%s)", d$nome[i], d$sigla[i]),
          c = d$casos[i], o = d$hosp[i], inc = d$inc[i]
        )), d$chave), auto_unbox = TRUE)
      tt <- htmlwidgets::JS(sprintf("function(p){
        var i = (%s)[p.name]; if (!i) return p.name;
        var f = function(x){ return (x === null || x === undefined) ? '-' :
                 Number(x).toLocaleString('pt-BR', {maximumFractionDigits: 1}); };
        return '<b>' + i.n + '</b><br/>Casos: ' + f(i.c) + '<br/>Incidência: ' + f(i.inc) +
               ' /100 mil<br/>%s: ' + f(i.o)%s;
      }", info, rotulo_hosp,
      if (eh_uf) " + '<br/><span style=\"opacity:.7\">Clique para abrir o estado</span>'" else ""))

      clique <- htmlwidgets::JS(sprintf(
        "function(p){ if (p && p.name && p.name !== 'CONTORNO') { var a = p.name; setTimeout(function(){
           Shiny.setInputValue('%s', a, {priority: 'event'}); }, 60); } }", ns("clique")))

      g <- geo()
      sel <- if (identical(g$nivel, "municipio")) as.character(g$geocodigo) else ""
      nome_mapa <- if (eh_uf) "BR_UF" else paste0("MUN_", u$uf)
      malha <- if (eh_uf) MALHA_UF else malha_municipios(u$uf)

      e <- d |>
        echarts4r::e_charts(chave) |>
        echarts4r::e_map_register(nome_mapa, malha) |>
        echarts4r::e_map(valor, map = nome_mapa, name = rot, nameProperty = "codarea",
                         roam = TRUE, label = list(show = FALSE),
                         itemStyle = list(borderColor = if (eh_uf) COR_BORDA_UF else COR_BORDA_MUN,
                                          borderWidth = if (eh_uf) 0.8 else 0.4),
                         emphasis = list(itemStyle = list(areaColor = TONS_AZUL[["300"]]),
                                         label = list(show = FALSE)),
                         selectedMode = "single",
                         select = list(itemStyle = list(areaColor = PALETA[["azul_acao"]]),
                                       label = list(show = FALSE))) |>
        echarts4r::e_visual_map(valor, inRange = list(color = ESCALA_MAPA),
                                orient = "vertical", left = "left", bottom = "6%",
                                text = list("alto", "baixo"), calculable = TRUE) |>
        echarts4r::e_tooltip(trigger = "item", formatter = tt) |>
        echarts4r::e_on(query = NULL, handler = clique, event = "click") |>
        echarts4r::e_on(query = NULL, event = "finished", handler = htmlwidgets::JS(sprintf(
          "function(){ if (this.__selFeito) return; this.__selFeito = true;
             if ('%s') this.dispatchAction({type: 'select', seriesIndex: 0, name: '%s'}); }",
          sel, sel))) |>
        e_padrao()
      # Escala: teto no percentil 98 (uma capital ou um municipio pequeno com
      # surto apagaria o resto do mapa em tons claros).
      pos <- d$valor[is.finite(d$valor) & d$valor > 0]
      if (length(pos)) e$x$opts$visualMap[[1]]$max <- max(round(stats::quantile(pos, 0.98, names = FALSE), 1), 0.1)
      e$x$opts$visualMap[[1]]$min <- 0
      if (!eh_uf) e <- com_contorno(e)
      e
    })

    pedido <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$clique, {
      cod <- suppressWarnings(as.integer(input$clique))
      shiny::req(!is.na(cod))
      pedido(if (cod < 100L) list(nivel = "uf", uf_ibge = cod, .ts = Sys.time())
             else list(nivel = "municipio", geocodigo = cod, .ts = Sys.time()))
    })
    shiny::observeEvent(input$limpar, {
      pedido(list(nivel = "base", .ts = Sys.time()))
    })
    pedido
  })
}

# ==============================================================================
# Gripe Brasil | server.R
# Orquestra os modulos. Brasil e UFs vem da base por UF em memoria; municipio
# (e os municipios de uma UF, no mapa/quadrantes/ranking) do Parquet
# municipal, sob demanda - ver global.R.
# ==============================================================================

function(input, output, session) {

  foco_mapa <- shiny::reactiveVal(NULL)

  # O seletor de grupo do modulo de filtros aqui e o agente etiologico.
  filtros <- mod_filtros_server("filtros", foco_externo = foco_mapa, padrao = "todos",
                                rotulo_filtro = rotulo_filtro_agente)
  geo    <- filtros$geo
  agente <- filtros$classe
  agentes <- shiny::reactive(agentes_do_filtro(agente()))

  # Serie semanal do recorte + agente: compartilhada por KPIs, serie, Rt,
  # canal e heatmap.
  serie <- shiny::reactive(serie_semanal(geo(), agentes())) |>
    shiny::bindCache(geo()$nivel, geo()$uf_ibge, geo()$geocodigo, agente())

  mod_kpi_server("kpi", geo, agente, serie)

  pedido <- mod_mapa_server("mapa", geo, agentes)
  shiny::observeEvent(pedido(), foco_mapa(pedido()), ignoreInit = TRUE)

  mod_serie_server("serie", geo, agente, serie)
  mod_rt_server("rt", agente, serie)
  pedido_tendencia <- mod_mapa_rt_server("tendencia", geo, agentes)
  shiny::observeEvent(pedido_tendencia(), foco_mapa(pedido_tendencia()), ignoreInit = TRUE)
  mod_canal_server("canal", serie)
  mod_heatmap_server("heatmap", serie)
  mod_perfil_server("perfil", geo, agente)
  mod_analises_server("analises", geo, agente)
}

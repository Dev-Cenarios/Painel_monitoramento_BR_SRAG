# ==============================================================================
# Gripe Brasil | Modulo: diagrama de controle (canal endemico)
#
# Diferente da dengue, o canal aqui usa o ANO CALENDARIO (SE 1 a 52): na
# Brasil (com defasagem entre regioes) a sazonalidade respiratoria e de outono-inverno (VSR com pico por
# volta da SE 12-16, influenza por volta da SE 18-24), entao o ano civil ja
# contem a temporada inteira e nao ha o que "costurar" na virada do ano.
#
# Anos de referencia: por padrao 2022 em diante, ate o ano anterior ao
# analisado. 2020-2021 (pandemia) tem 10-20x mais SRAG que o normal e
# inflariam o canal; 2019 tem notificacao bem menor (SIVEP antes da
# pandemia). O usuario pode incluir qualquer ano.
#
# Canal = quartis da incidencia semanal dos anos de referencia:
#   abaixo de Q1 (sucesso) | Q1-Q3 (seguranca/alerta, mediana marcada) |
#   acima de Q3 (epidemia). Com poucos anos de referencia os quartis sao
#   instaveis - o rodape do grafico avisa quantos anos entraram.
# ==============================================================================

mod_canal_controles <- function(id) {
  ns <- shiny::NS(id)
  anos <- seq(ANO_MIN, ANO_MAX)
  shiny::tagList(
    shiny::selectInput(ns("ano"), "Ano analisado", choices = rev(anos), selected = ANO_MAX),
    shiny::checkboxGroupInput(ns("ref"), "Anos de referência",
                              choices = anos, inline = TRUE,
                              selected = anos[anos >= 2022 & anos < ANO_MAX]),
    shiny::helpText(shiny::tags$small(
      "2020–2021 ficam fora por padrão: a covid-19 multiplicou a SRAG e distorce o canal."
    )),
    shiny::checkboxInput(ns("nowcast"), "Mostrar nowcast", TRUE)
  )
}

mod_canal_saida <- function(id, height = "400px") {
  shiny::tagList(
    echarts4r::echarts4rOutput(shiny::NS(id)("grafico"), height = height),
    shiny::div(class = "small text-muted px-3 pb-2", shiny::textOutput(shiny::NS(id)("nota")))
  )
}

mod_canal_server <- function(id, serie) {
  shiny::moduleServer(id, function(input, output, session) {

    dados <- shiny::reactive({
      s <- serie()
      s$semana <- pmin(s$semana, 52L)  # SE 53 entra como 52
      s <- s |>
        dplyr::group_by(ano, semana) |>
        dplyr::summarise(inc = sum(casos) / max(pop) * 1e5,
                         inc_est = if (all(is.na(casos_est))) NA_real_
                                   else sum(dplyr::coalesce(casos_est, casos)) / max(pop) * 1e5,
                         .groups = "drop")

      ano <- as.integer(input$ano %||% ANO_MAX)
      ref <- setdiff(as.integer(input$ref), ano)

      canal <- s |>
        dplyr::filter(ano %in% ref) |>
        dplyr::group_by(semana) |>
        dplyr::summarise(q1 = stats::quantile(inc, 0.25), q2 = stats::median(inc),
                         q3 = stats::quantile(inc, 0.75), .groups = "drop")

      atual <- s |> dplyr::filter(ano == !!ano) |> dplyr::select(semana, inc, inc_est)

      out <- data.frame(semana = 1:52) |>
        dplyr::left_join(canal, by = "semana") |>
        dplyr::left_join(atual, by = "semana")
      list(d = out, ano = ano, ref = ref)
    })

    output$nota <- shiny::renderText({
      r <- dados()
      sprintf("Canal: quartis da incidência semanal de %s (%d ano%s). Linha: %d, por semana de início dos sintomas.",
              if (length(r$ref)) paste(sort(r$ref), collapse = ", ") else "nenhum ano",
              length(r$ref), if (length(r$ref) == 1) "" else "s", r$ano)
    })

    output$grafico <- echarts4r::renderEcharts4r({
      r <- dados()
      d <- r$d
      shiny::validate(shiny::need(length(r$ref) > 0, "Escolha ao menos um ano de referência."))

      d <- d |>
        dplyr::mutate(
          x      = sprintf("SE %02d", semana),
          base   = round(q1, 2),
          faixa  = round(q3 - q1, 2),
          q2     = round(q2, 2),
          inc    = round(inc, 2),
          inc_est = round(inc_est, 2)
        )
      com_nc <- isTRUE(input$nowcast) && any(!is.na(d$inc_est))
      if (com_nc) {
        # A linha do nowcast parte do ultimo ponto observado antes dela.
        k0 <- min(which(!is.na(d$inc_est))) - 1L
        if (k0 >= 1L) d$inc_est[k0] <- d$inc[k0]
      }

      e <- d |>
        echarts4r::e_charts(x) |>
        echarts4r::e_line(base, name = "Abaixo de Q1", stack = "canal", symbol = "none",
                          lineStyle = list(width = 0),
                          areaStyle = list(color = TONS_AZUL[["50"]])) |>
        echarts4r::e_line(faixa, name = "Q1 – Q3", stack = "canal", symbol = "none",
                          lineStyle = list(width = 0),
                          areaStyle = list(color = TONS_AZUL[["200"]])) |>
        echarts4r::e_line(q2, name = "Mediana", symbol = "none",
                          lineStyle = list(width = 2, color = TONS_AZUL[["800"]])) |>
        echarts4r::e_line(inc, name = as.character(r$ano), symbol = "circle", symbolSize = 5,
                          lineStyle = list(width = 3, color = TONS_GRIPE[["700"]]),
                          itemStyle = list(color = TONS_GRIPE[["700"]]))
      cores <- c(TONS_AZUL[["50"]], TONS_AZUL[["200"]], TONS_AZUL[["800"]], TONS_GRIPE[["700"]])
      if (com_nc) {
        e <- e |> echarts4r::e_line(inc_est, name = "Nowcast", symbol = "none",
                                    lineStyle = list(type = "dashed", width = 2,
                                                     color = PALETA[["nowcast"]]))
        cores <- c(cores, PALETA[["nowcast"]])
      }

      # Tooltip: a serie "Q1 – Q3" guarda a LARGURA da faixa (empilhada sobre
      # Q1); o tooltip mostra Q3 = Q1 + largura.
      tt <- htmlwidgets::JS("function(ps){
        var out = ps[0].axisValue, q1 = null;
        ps.forEach(function(p){
          var v = Array.isArray(p.value) ? p.value[1] : p.value;
          if (v === null || v === undefined || v === '-' || isNaN(v)) return;
          if (p.seriesName === 'Abaixo de Q1') { q1 = +v; out += '<br/>Q1: ' + (+v).toFixed(1); return; }
          if (p.seriesName === 'Q1 – Q3') { out += '<br/>Q3: ' + (q1 + +v).toFixed(1); return; }
          out += '<br/>' + p.marker + p.seriesName + ': ' + (+v).toFixed(1);
        });
        return out;
      }")

      e |>
        echarts4r::e_color(unname(cores)) |>
        echarts4r::e_tooltip(trigger = "axis", formatter = tt, confine = TRUE) |>
        echarts4r::e_legend(bottom = 0) |>
        echarts4r::e_y_axis(name = "Incidência /100 mil") |>
        echarts4r::e_grid(left = 55, right = 20, top = 30, bottom = 60) |>
        e_padrao()
    })
  })
}

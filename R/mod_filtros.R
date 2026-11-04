# ==============================================================================
# Modulo: filtros globais (recorte geografico + classificacao)
#
# Contrato de saida: lista de dois reactives
#   $geo     list(nivel = "base"|"uf"|"municipio", uf_ibge, geocodigo, rotulo)
#            ("base" = Brasil)
#   $classe  chave de FILTROS_CLASSE ("provaveis", "confirmados", ...)
#
# Seletor de municipio em cascata: so os municipios da UF escolhida (5.570 de
# uma vez deixariam o selectize lento). Trocar a UF esvazia o municipio;
# escolher um municipio leva a abrangencia para "Municipio".
#
# Entrada `foco_externo`: reactive com o pedido do mapa -
#   list(nivel = "uf", uf_ibge)           clique numa UF (visao Brasil)
#   list(nivel = "municipio", geocodigo)  clique num municipio
#   list(nivel = "base")                  botao "Visao geral"
# ==============================================================================

mod_filtros_ui <- function(id, rotulo_classe = "Casos", filtros = FILTROS_CLASSE,
                           selecionado = filtros[[1]]) {
  ns <- shiny::NS(id)

  bslib::layout_column_wrap(
    width = 1/4,
    gap   = "1rem",
    shiny::radioButtons(
      ns("nivel"), "Abrangência",
      choices  = c(stats::setNames("base", ROTULO_BASE), "Estado" = "uf", "Município" = "municipio"),
      selected = "base", inline = TRUE
    ),
    shiny::selectizeInput(
      ns("uf"), "Estado",
      choices  = stats::setNames(as.character(UF_DISPONIVEIS$uf_ibge),
                                 sprintf("%s (%s)", UF_DISPONIVEIS$uf_nome, UF_DISPONIVEIS$uf_sigla)),
      selected = "53", width = "100%",
      options  = list(dropdownParent = "body")
    ),
    shiny::selectizeInput(
      ns("municipio"), "Município",
      choices  = NULL, width = "100%",
      options  = list(placeholder = "Digite para buscar...", dropdownParent = "body")
    ),
    shiny::selectizeInput(
      ns("classe"), rotulo_classe,
      choices = filtros, selected = selecionado, width = "100%",
      options = list(dropdownParent = "body")
    )
  )
}

mod_filtros_resumo <- function(id) {
  shiny::textOutput(shiny::NS(id)("resumo"), inline = TRUE)
}

#' Opcoes do seletor de municipio de uma UF.
.choices_municipio <- function(uf) {
  g <- GEO_DICT[GEO_DICT$uf_ibge == as.integer(uf), , drop = FALSE]
  c(stats::setNames("", ""), stats::setNames(as.character(g$municipio_geocodigo), g$municipio_nome))
}

mod_filtros_server <- function(id, foco_externo = shiny::reactive(NULL),
                               padrao = FILTROS_CLASSE[[1]], rotulo_filtro = rotulo_filtro_classe) {
  shiny::moduleServer(id, function(input, output, session) {

    # Municipio que deve ficar selecionado depois que a lista da UF for
    # recarregada (clique no mapa ja traz o municipio).
    mun_pendente <- shiny::reactiveVal("")

    # Trocar a UF recarrega a lista de municipios e leva a abrangencia para
    # "Estado" (exceto quando a troca veio de um clique num municipio de
    # outra UF). A selecao inicial nao tira o painel da visao Brasil.
    uf_inicial <- TRUE
    shiny::observeEvent(input$uf, {
      pend <- mun_pendente()
      shiny::updateSelectizeInput(session, "municipio", choices = .choices_municipio(input$uf),
                                  selected = pend, server = TRUE)
      mun_pendente("")
      if (uf_inicial) { uf_inicial <<- FALSE; return() }
      if (!nzchar(pend) && !identical(input$nivel, "uf")) {
        shiny::updateRadioButtons(session, "nivel", selected = "uf")
      }
    })

    # Escolher um municipio leva a abrangencia para "Municipio".
    shiny::observeEvent(input$municipio, {
      if (nzchar(input$municipio %||% "") && !identical(input$nivel, "municipio")) {
        shiny::updateRadioButtons(session, "nivel", selected = "municipio")
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(foco_externo(), {
      f <- foco_externo()
      if (identical(f$nivel, "base")) {
        shiny::updateRadioButtons(session, "nivel", selected = "base")
      } else if (identical(f$nivel, "uf")) {
        shiny::updateSelectizeInput(session, "uf", selected = as.character(f$uf_ibge))
        shiny::updateRadioButtons(session, "nivel", selected = "uf")
      } else if (identical(f$nivel, "municipio")) {
        uf <- as.integer(f$geocodigo) %/% 100000L
        if (!identical(as.character(uf), input$uf)) {
          mun_pendente(as.character(f$geocodigo))
          shiny::updateSelectizeInput(session, "uf", selected = as.character(uf))
        } else {
          shiny::updateSelectizeInput(session, "municipio", selected = as.character(f$geocodigo))
        }
        shiny::updateRadioButtons(session, "nivel", selected = "municipio")
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    geo_uf <- function() {
      uf <- as.integer(input$uf)
      list(nivel = "uf", uf_ibge = uf, geocodigo = NA_integer_,
           rotulo = UF_LOOKUP$uf_nome[UF_LOOKUP$uf_ibge == uf])
    }

    geo_bruto <- shiny::reactive({
      shiny::req(input$nivel, input$uf)
      switch(
        input$nivel,
        base = list(nivel = "base", uf_ibge = NA_integer_, geocodigo = NA_integer_,
                    rotulo = ROTULO_BASE),
        uf = geo_uf(),
        municipio = {
          cod <- suppressWarnings(as.integer(input$municipio))
          g <- GEO_DICT[GEO_DICT$municipio_geocodigo %in% cod, ]
          # Municipio ainda nao escolhido (ou de outra UF durante a troca):
          # o painel fica no estado ate o seletor se acertar.
          if (!nrow(g) || g$uf_ibge != as.integer(input$uf)) return(geo_uf())
          list(nivel = "municipio", uf_ibge = g$uf_ibge, geocodigo = cod,
               rotulo = sprintf("%s (%s)", g$municipio_nome, g$uf_sigla))
        }
      )
    })

    # So propaga quando o recorte MUDA. Um clique no mapa troca a UF e o nivel
    # em duas mensagens do navegador; sem isso, o painel inteiro recalculava
    # duas vezes (a segunda com o mesmo recorte).
    geo_rv <- shiny::reactiveVal(NULL)
    shiny::observe({
      g <- geo_bruto()
      if (!identical(g, shiny::isolate(geo_rv()))) geo_rv(g)
    }, priority = 10)
    geo <- shiny::reactive({
      shiny::req(geo_rv())
      geo_rv()
    })

    classe <- shiny::reactive(input$classe %||% padrao)

    output$resumo <- shiny::renderText({
      sprintf("%s · %s", geo()$rotulo, rotulo_filtro(classe()))
    })

    list(geo = geo, classe = classe)
  })
}

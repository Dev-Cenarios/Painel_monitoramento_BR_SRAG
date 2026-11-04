# ==============================================================================
# Gripe Brasil | ui.R
#
# Aba "Painel":
#   0. cabecalho
#   1. filtros (recorte geografico + agente etiologico)
#   2. KPIs
#   3. mapa (5) | evolucao semanal por agente (7)
#   4. Rt estimado pelo painel (7) | tendencia por municipio - quadrantes (5)
#   5. canal endemico (7) | sazonalidade (5)
#   6. perfil etario e gravidade
# Aba "Análises": ranking (UFs ou municipios da UF), agentes por unidade, achados da exploratoria.
# Aba "Sobre os dados": fonte, definicoes e limitacoes.
# ==============================================================================

bslib::page_navbar(
  title        = "Gripe Brasil",
  theme        = TEMA_GRIPE,
  window_title = "Gripe Brasil — Monitoramento de SRAG e vírus respiratórios",
  padding      = "1rem",
  fillable     = FALSE,

  bslib::nav_panel(
    "Painel",
    gripe_ui_assets(),

    shiny::tags$div(
      class = "d-flex justify-content-between align-items-end mb-3",
      shiny::tags$div(
        shiny::tags$h4(class = "mb-0 fw-semibold", "Gripe Brasil"),
        shiny::tags$span(class = "text-muted small",
                         "Painel de monitoramento de SRAG e vírus respiratórios · fonte SIVEP-Gripe")
      ),
      shiny::tags$span(
        class = "text-muted small text-end",
        sprintf("27 UFs · %s municípios · %d–%d · dados até a semana de %s",
                fmt_int(nrow(GEO_DICT)), ANO_MIN, ANO_MAX, format(DATA_MAX, "%d/%m/%Y"))
      )
    ),

    bslib::card(
      class = "mb-3",
      bslib::card_header(
        class = "bg-primary text-white d-flex justify-content-between align-items-center",
        shiny::tags$span(class = "fw-semibold", "Recorte"),
        shiny::tags$span(class = "small", mod_filtros_resumo("filtros"))
      ),
      bslib::card_body(mod_filtros_ui("filtros", rotulo_classe = "Agente etiológico", filtros = FILTROS_AGENTE, selecionado = "todos"))
    ),

    mod_kpi_ui("kpi"),

    bslib::layout_columns(
      col_widths = c(5, 7),
      cartao_grafico("Distribuição espacial", mod_mapa_saida("mapa", "460px"),
                     mod_mapa_controles("mapa")),
      cartao_grafico("Casos semanais por agente etiológico", mod_serie_saida("serie", "460px"),
                     mod_serie_controles("serie"),
                     rodape = "Semana de início dos sintomas. Linha tracejada: estimativa corrigida pelo atraso de digitação (nowcast) e faixa de incerteza.")
    ),

    bslib::layout_columns(
      col_widths = c(7, 5),
      cartao_grafico("Rt — número de reprodução efetivo (estimado pelo painel)", mod_rt_saida("rt", "440px"),
                     mod_rt_controles("rt"),
                     rodape = "Acima de 1 (linha tracejada preta), os casos de SRAG estão crescendo. Faixa: IC 95%. Trecho tracejado: semanas corrigidas pelo nowcast, preliminares. Segue o recorte e o agente selecionados."),
      # Nao e o Rt real: nome mantido igual ao do dengue_ride (ver R/mod_mapa_rt.R).
      cartao_grafico("Rt — Tendência por UF / município (quadrantes)",
                     mod_mapa_rt_saida("tendencia", "440px"), mod_mapa_rt_controles("tendencia"),
                     rodape = "Não é o Rt: compara cada unidade com ela mesma em anos anteriores (X) e com a janela anterior (Y). No Brasil, um ponto por UF; num estado, os municípios dele. Segue o agente. Semana corrente fora; recentes corrigidas pelo nowcast da UF.")
    ),

    bslib::layout_columns(
      col_widths = c(7, 5),
      cartao_grafico("Diagrama de controle (canal endêmico)", mod_canal_saida("canal", "380px"),
                     mod_canal_controles("canal")),
      cartao_grafico("Sazonalidade — ano × semana epidemiológica",
                     mod_heatmap_saida("heatmap", "420px"), mod_heatmap_controles("heatmap"))
    ),

    bslib::navset_card_tab(
      full_screen = TRUE,
      title = shiny::tags$span(class = "fw-semibold", "Perfil etário e gravidade"),
      bslib::nav_panel("Faixa etária × agente",
                       bslib::card_body(class = "p-0", mod_perfil_saida("perfil", "faixa"))),
      bslib::nav_panel("Gravidade por faixa",
                       bslib::card_body(class = "p-0", mod_perfil_saida("perfil", "gravidade"),
                                        shiny::tags$p(class = "small text-muted px-3",
                                                      "Exclui as 8 semanas mais recentes (desfecho ainda em aberto)."))),
      bslib::nav_spacer(),
      bslib::nav_item(bslib::popover(gatilho_opcoes(), mod_perfil_controles("perfil"),
                                     title = "Opções", placement = "left"))
    )
  ),

  bslib::nav_panel("Análises", mod_analises_ui("analises")),

  bslib::nav_panel(
    "Sobre os dados",
    bslib::card(
      bslib::card_body(
        class = "small",
        shiny::tags$h5("Fonte"),
        shiny::tags$p(
          "Microdados de SRAG do SIVEP-Gripe (OpenDataSUS), tabela ", shiny::tags$code("\"Info_Gripe\".srag"),
          sprintf(" no banco do projeto. Casos com residência em todo o Brasil (5.570 municípios, 27 UFs), de %s a %s. ",
                  format(DATA_MIN, "%d/%m/%Y"), format(DATA_MAX, "%d/%m/%Y")),
          "O ETL (", shiny::tags$code("etl_gripe_br.R"), ") agrega tudo no Postgres; o painel não lê microdado."
        ),
        shiny::tags$h5("Definições"),
        shiny::tags$ul(
          shiny::tags$li(shiny::tags$b("Semana:"), " semana epidemiológica (domingo a sábado) do início dos sintomas."),
          shiny::tags$li(shiny::tags$b("Agente:"), " a partir de CLASSI_FIN; influenza A/B pelo RT-PCR (ou antígeno); os demais vírus respiratórios (CLASSI_FIN = 2) quebrados pelo painel de PCR/antígeno. Com 2+ vírus detectados, vale o primeiro nesta ordem: VSR, metapneumovírus, adenovírus, parainfluenza, rinovírus, outros (bocavírus, enterovírus, coronavírus sazonais). \"Em investigação\" = caso ainda sem classificação final."),
          shiny::tags$li(shiny::tags$b("Incidência:"), " casos por 100 mil habitantes, com a estimativa populacional anual do IBGE usada pelo InfoDengue."),
          shiny::tags$li(shiny::tags$b("Óbito:"), " EVOLUCAO = 2 (óbito por SRAG). Letalidade = óbitos / casos."),
          shiny::tags$li(shiny::tags$b("DF:"), " o SIVEP registra a residência no DF por Região Administrativa; o painel soma todas as RAs no município Brasília (5300108). O detalhe por RA fica no painel da RIDE.")
        ),
        shiny::tags$h5("Nowcast"),
        shiny::tags$p(sprintf(
          "Proporção acumulada de casos digitados em até k semanas, F(k), medida nas semanas já maduras (≥ 26 semanas) dos últimos dois anos; o valor das últimas %d semanas é dividido por F(k). No Brasil, F(0) = %s, F(1) = %s, F(4) = %s. Faixa: quantis 10%%–90%% de F(k) entre as semanas de treino. F(k) estimado por UF (o atraso de digitação varia muito entre estados); o Brasil usa o atraso nacional e UFs com pouca base também. Vale para qualquer agente.",
          NOWCAST_MAX_K, fmt_pct(NOWCAST_F$f[1]), fmt_pct(NOWCAST_F$f[2]), fmt_pct(NOWCAST_F$f[5]))),
        shiny::tags$h5("Rt"),
        shiny::tags$p(sprintf(
          "Não há Rt de SRAG pronto no banco (o InfoDengue cobre só arboviroses), então o painel estima o seu a partir da série semanal do recorte e do agente: a taxa de crescimento diária r = ln(S_t / S_t−2) / 14, com S_t = casos das semanas t e t−1, vira Rt = (1 + r·θ)^k (Wallinga & Lipsitch, 2007) pelo intervalo de geração gama do agente - influenza 3,6 dias (desvio 1,6), covid-19 3,5 (2,4), VSR 7,5 (2,1) e 4,5 (2,5) para mistura ou outros vírus. IC 95%% pelo erro de Poisson das contagens. As semanas recentes usam os casos corrigidos pelo nowcast e são preliminares; a semana corrente fica de fora; janelas com menos de %d casos ficam sem estimativa. Como SRAG é caso grave, o Rt mede o crescimento das internações, um sinal atrasado da transmissão na comunidade.",
          RT_MIN_CASOS)),
        shiny::tags$h5("Limitações"),
        shiny::tags$ul(
          shiny::tags$li("SRAG é um evento hospitalar: mede a forma grave, não toda a circulação viral (síndrome gripal leve não entra)."),
          shiny::tags$li("2019 tem notificação bem menor que os anos seguintes; 2020–2021 são dominados pela covid-19. Por isso o canal endêmico usa 2022 em diante por padrão."),
          shiny::tags$li("Desfecho e classificação final levam semanas para fechar: letalidade e proporção de agentes das últimas semanas ainda vão mudar."),
          shiny::tags$li("Vacinação contra gripe tem muito campo em branco; os percentuais são um piso.")
        )
      )
    )
  )
)

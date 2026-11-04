# ==============================================================================
# Gripe Brasil | utils_ui.R
# Tema e helpers de composicao de cards (mesmo desenho do painel de dengue).
# ==============================================================================

TEMA_GRIPE <- bslib::bs_theme(
  version = 5,
  preset  = "zephyr",

  bg = PALETA[["fundo"]],
  fg = PALETA[["texto"]],

  primary   = PALETA[["azul_acao"]],
  secondary = PALETA[["sub"]],
  success   = PALETA[["ok"]],
  warning   = PALETA[["atencao"]],
  danger    = PALETA[["critico"]],
  info      = PALETA[["azul"]],

  base_font = bslib::font_collection(
    bslib::font_google("DM Sans", wght = c(400, 500, 700), local = FALSE),
    "system-ui", "-apple-system", "Segoe UI", "Roboto", "sans-serif"
  ),
  heading_font = bslib::font_collection(
    bslib::font_google("Poppins", wght = c(600, 700), local = FALSE),
    "system-ui", "sans-serif"
  ),

  "headings-color"     = PALETA[["azul_titulo"]],
  "body-color"         = PALETA[["texto"]],
  "link-color"         = PALETA[["azul_acao"]],
  "border-radius"      = "0.65rem",
  "border-radius-sm"   = "0.45rem",
  "card-border-radius" = "0.65rem",
  "card-cap-padding-y" = "0.6rem",
  "card-border-color"  = PALETA[["linha"]],
  "card-bg"            = PALETA[["superficie"]],
  "enable-shadows"     = TRUE
) |>
  bslib::bs_add_rules(sprintf("
    .card-header.bg-primary { background-color: %s !important; }
    .card-header, .card-header .fw-semibold, .navbar-brand {
      font-family: Poppins, system-ui, sans-serif;
    }
    .value-box-value, .bslib-value-box .value-box-value, table td {
      font-variant-numeric: tabular-nums;
    }
    .value-box-value { font-weight: 700; }
    .insight { border-left: 4px solid %s; padding: .35rem .9rem; margin-bottom: .9rem; }
    .insight h6 { margin-bottom: .2rem; }
    .insight p { margin-bottom: 0; }
  ", PALETA[["azul_titulo"]], TONS_GRIPE[["500"]]))


#' Gatilho de popover para os controles de um card.
gatilho_opcoes <- function(rotulo = "Opções") {
  shiny::tags$span(
    class = "badge rounded-pill bg-light text-primary fw-normal",
    style = "cursor: pointer;",
    rotulo
  )
}

#' Card padrao de grafico: titulo, popover de opcoes e saida sem padding.
cartao_grafico <- function(titulo, saida, controles = NULL, rodape = NULL) {
  bslib::card(
    full_screen = TRUE,
    bslib::card_header(
      class = "bg-primary text-white d-flex justify-content-between align-items-center",
      shiny::tags$span(class = "fw-semibold", titulo),
      if (!is.null(controles)) {
        bslib::popover(gatilho_opcoes(), controles, title = titulo, placement = "left")
      }
    ),
    bslib::card_body(class = "p-0", saida),
    if (!is.null(rodape)) bslib::card_footer(class = "small text-muted", rodape)
  )
}

#' Opcoes comuns a todos os graficos echarts (fonte, toolbox).
e_padrao <- function(e) {
  e |>
    echarts4r::e_text_style(fontFamily = FONTE_GRAFICO) |>
    echarts4r::e_toolbox_feature("saveAsImage", title = "Salvar imagem")
}

#' Ajustes globais de UI: menu dos selectize por cima de tudo, popover que
#' fecha ao clicar fora (ou Esc) e resize dos graficos que esticam com o card.
gripe_ui_assets <- function() {
  shiny::tagList(
    shiny::singleton(shiny::tags$style(shiny::HTML(
      ".selectize-dropdown { z-index: 1080 !important; }"
    ))),
    shiny::singleton(shiny::tags$script(shiny::HTML(
"(function() {
  function insidePicker(t) {
    return t && t.closest && t.closest('.selectize-dropdown, .selectize-control');
  }
  function triggerOf(tip) {
    return tip.id ? document.querySelector('[aria-describedby=\"' + tip.id + '\"]') : null;
  }
  function hide(tip) {
    var trg  = triggerOf(tip);
    var inst = (trg && window.bootstrap && bootstrap.Popover)
      ? bootstrap.Popover.getInstance(trg) : null;
    var host = (trg && trg.closest) ? trg.closest('bslib-popover') : null;
    if (inst) inst.hide();
    else if (host && typeof host.hide === 'function') host.hide();
    else if (trg) trg.click();
  }
  document.addEventListener('click', function(e) {
    var open = document.querySelectorAll('.popover.show');
    if (!open.length || insidePicker(e.target)) return;
    open.forEach(function(tip) {
      var trg = triggerOf(tip);
      if (tip.contains(e.target)) return;
      if (trg && (trg === e.target || trg.contains(e.target))) return;
      hide(tip);
    });
  });
  document.addEventListener('keydown', function(e) {
    if (e.key !== 'Escape') return;
    document.querySelectorAll('.popover.show').forEach(hide);
  });
})();"
    )))
  )
}

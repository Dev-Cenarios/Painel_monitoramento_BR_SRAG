# Gripe Brasil — Painel de monitoramento de SRAG

Versão nacional do **gripe_ride**: os mesmos gráficos (agentes etiológicos,
nowcast, Rt estimado pelo painel, canal endêmico em ano civil), para os 5.570
municípios, com recorte **Brasil → Estado → Município**. Fonte:
`"Info_Gripe".srag` (SIVEP-Gripe) e população do InfoDengue.

Estrutura idêntica à do `chik_br` (ver o README dele); aqui a coluna de
agrupamento é `agente`, não `classe`.

## Atualizar os dados

```bash
cd /srv/shiny-server/gripe_br && Rscript etl_gripe_br.R && touch restart.txt
```

~2 min (4,7 mi de linhas no banco), ~21 MB de Parquets (não versionados):
`srag_br_uf`, `srag_br_mun`, `srag_br_atraso`, `srag_br_pop`.

## Diferenças para o gripe_ride

| Tema | Decisão |
|---|---|
| RA do DF | Sem quebra por Região Administrativa: no Brasil o DF é um município (todo 53xxxx continua indo para 5300108). O detalhe por RA fica no painel da RIDE. |
| Análises | O gráfico "DF por RA" virou "Agentes por UF / município" (últimas 52 semanas) no recorte do Painel. O achado do DF virou "Onde a SRAG pesa mais" (incidência por UF). |
| Sazonalidade | Achado calculado por região (pico mediano no Norte, Nordeste, Centro-Oeste, Sudeste e Sul). |
| Nowcast | F(k) por UF, com os parâmetros do gripe_ride (2 anos, semanas com ≥ 20 casos, corrige também a semana corrente). |

Rodar local: `shiny::runApp()` na pasta (preview na porta 8792).

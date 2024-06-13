
library(tidyverse)
library(shinypanels)
library(shiny)
library(DT)
library(hgmagic)
# library(lfltmagic)
library(sibdata)
library(shinyinvoer)
library(dsmods)
library(dbplyr)



dbdir <- sys_file_sibdata("db/sibdata.duckdb")
con <- DBI::dbConnect(RSQLite::SQLite(), sys_file_sibdata("db/sibdata.sqlite"),
                      read_only = TRUE)


av_grupos_bio <- sib_available_grupos(tipo = "biologico", con = con)
opts_grupo_biologico <- c("Todos" = "todos", av_grupos_bio)
av_grupos_int <- sib_available_grupos(tipo = "interes", con = con)
opts_grupo_interes <-  c("Todos" = "todos", av_grupos_int)

pais <- sib_available_regions(subtipo = "País", con = con)
departamentos <- sib_available_regions(subtipo = "Departamento", con = con)

opts_tematicas <- c("Todas" = "todas", sib_available_tematicas())




input <- list(
  region = "colombia",
  tipo = "especies",
  grupo_tipo = "biologico",
  grupo = NULL,
  tematica = NULL,
  subregiones = FALSE,
  with_parent = FALSE
)
inp <- input
region <- inp$region
d <- sibdata(inp$region,
             grupo = inp$grupo,
             tipo = inp$tipo,
             cobertura = inp$cobertura,
             tematica = inp$tematica,
             subregiones = inp$subregiones,
             with_parent = inp$with_parent,
             con = con)
d
hgmagic::hg_pie_CatNum(d)




input <- list(
  region = "colombia",
  tipo = "especies",
  grupo_tipo = "biologico",
  grupo = NULL,
  tematica = NULL,
  subregiones = FALSE,
  with_parent = FALSE
)
inp <- input
region <- inp$region
d <- sibdata(inp$region,
             grupo = inp$grupo,
             tipo = inp$tipo,
             cobertura = inp$cobertura,
             tematica = inp$tematica,
             subregiones = inp$subregiones,
             with_parent = inp$with_parent,
             con = con)
d
hgmagic::hg_bar_CatNum(iris |> select(5,4), color_palette_categorical = NULL, color_by = NULL)
hgmagic::hg_pie_CatNum(d)






input <- list(
  region = "boyaca",
  grupo = "animales",
  tipo = "especies",
  tematica = "amenazadas_nacional",
  subregiones = FALSE,
  with_parent = FALSE
)
inp <- input
region <- inp$region
d <- sibdata(inp$region,
             grupo = inp$grupo,
             tipo = inp$tipo,
             #cobertura = inp$cobertura,
             tematica = inp$tematica,
             subregiones = inp$subregiones,
             with_parent = inp$with_parent,
             con = con)
d <- d |> sib_merge_ind_label(con = con)
palette <- c("#FF0000", "#FFA500", "#FFFF00")
hgmagic::hg_pie_CatNum(d, opts = list(color_palette_categorical = palette))

input <- list(
  region = "boyaca",
  grupo = "animales",
  tipo = "especies",
  tematica = "cites",
  subregiones = FALSE,
  with_parent = FALSE
)
inp <- input
region <- inp$region
d <- sibdata(inp$region,
             grupo = inp$grupo,
             tipo = inp$tipo,
             #cobertura = inp$cobertura,
             tematica = inp$tematica,
             subregiones = inp$subregiones,
             with_parent = inp$with_parent,
             con = con)
d <- d |> sib_merge_ind_label(con = con)
palette <- c("#00AFFF", "#000000", "#FFD150", "#4DD3AC")
hgmagic::hg_pie_CatNum(d, opts = list(color_palette_categorical = palette))


## Exoticas total

input <- list(
  region = "boyaca",
  grupo = "animales",
  tematica = "exoticas-total",
  subregiones = FALSE,
  with_parent = FALSE
)
inp <- input
region <- inp$region
d <- sibdata(inp$region,
             grupo = inp$grupo,
             tipo = inp$tipo,
             cobertura = inp$cobertura,
             tematica = inp$tematica,
             subregiones = inp$subregiones,
             with_parent = inp$with_parent,
             con = con)


esp <- list_species(region = inp$region,
                    grupo = inp$grupo,
                    tematica = inp$tematica,
                    con = con)




##
esp <- list_species(region = input$region,
             grupo = input$grupo,
             tematica = input$tematica,
             con = con)
esp <- esp |> collect()
  vars <- c("label", "registros", "url_gbif", "url_cbc", "kingdom",
            "phylum", "class", "order", "family", "genus")

esp |>
  select(any_of(vars)) |>
  rename(
    "Especie" = "label",
    "Registros" = "registros",
    "Reino" = "kingdom",
    "Filo" = "phylum",
    "Clase" = "class",
    "Orden" = "order",
    "Familia" = "family",
    "Género" = "genus"
  )
esp



### Map






### Collapsible tree

#install.packages("collapsibleTree")
library(collapsibleTree)
library(tidyverse)


library(data.tree)

gru <- sibdata_grupo(con) |> collect() |> filter(tipo == "biologico") |>
  arrange()

gru_tree <- data.tree::FromDataFrameNetwork(gru)
gru_df <- data.tree::ToDataFrameNetwork(gru_tree,
                                        direction = "descend",
                                        "label", "level", "path")
paste_dash <- function(str, times = 1){
  paste(" ", paste0(rep("-",times-1), collapse = ""),str)
}
opt_gru <- gru_df |>
  rowwise() |>
  mutate(label = paste_dash(label, level)) |>
  arrange(path)
opts_grupo_biologico <- opt_gru$from
names(opts_grupo_biologico) <- opt_gru$label
opts_grupo_biologico <- c("Todos" = "todos", opts_grupo_biologico)
str(names(opts_grupo_biologico))

collapsibleTree(gru_tree,
                attribute = "label",
                tooltipHTML = "label",
                collapsed = TRUE)

tem <- sibdata_tematica(con) |> collect()
collapsibleTree(tem, c("parent", "slug"))


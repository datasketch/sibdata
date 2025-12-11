
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



# dbdir <- sys_file_sibdata("db/sibdata.duckdb")
con <- DBI::dbConnect(RSQLite::SQLite(), sys_file_sibdata("db/sibdata.sqlite"),
                      read_only = TRUE)

conmap <- gt_con()
# Do not forget to close the connection when done


## VISUALIZATION3 MODULE

### WHEN r$is_special_region is TRUE
### show buttonImageInput only with the table
### WHEN r$is_special_region is FALSE
### show regular map of the regions
### When r$has_subtematica is TRUE
### Show also the buttonImageInput with the possibility to select chartypes
### map, pie, donut, treemap, bars



r <- list(
  sel_region = "colombia",
  sel_tipo = "registros",
  sel_grupo_tipo = "biologico",
  sel_grupo = NULL,
  sel_tematica = "amenazadas_nacional",
  sel_indicador = "amenazadas_nacional_total",
  subregiones = FALSE,
  with_parent = FALSE
)
# Convert dashes to underscores for API compatibility
tematica_api <- if(!is.null(r$sel_tematica)) gsub("-", "_", r$sel_tematica) else r$sel_tematica

d <- sibdata(
  region = r$sel_region,
  grupo = r$sel_grupo,
  tipo = r$sel_tipo,
  tematica = tematica_api,
  indicador = r$indicador,
  subregiones = TRUE, # Always TRUE for maps
  with_parent = FALSE,
  con = con
)
d
data <- d

result <- choropleth_map(
  data = data,
  region = r$sel_region,
  tipo = r$sel_tipo,
  tematica = r$sel_tematica,
  indicador = r$indicador,
  grupo = r$sel_grupo,
  subregiones = TRUE,  # Always TRUE for maps
  with_parent = FALSE,
  con = con,
  conmap = conmap
)
result



## MAP example

r <- list(
  sel_region = "colombia",
  sel_tipo = "especies",
  sel_grupo_tipo = "biologico",
  sel_grupo = NULL,
  sel_tematica = "endemicas",
  indicador = "registros_region_total",
  subregiones = FALSE,
  with_parent = FALSE
)
# Convert dashes to underscores for API compatibility
tematica_api <- if(!is.null(r$sel_tematica)) gsub("-", "_", r$sel_tematica) else r$sel_tematica

d <- sibdata(
  region = r$sel_region,
  grupo = r$sel_grupo,
  tipo = r$sel_tipo,
  tematica = tematica_api,
  indicador = r$indicador,
  subregiones = TRUE, # Always TRUE for maps
  with_parent = FALSE,
  con = con
)

data <- d

result <- choropleth_map(
  data = data,
  region = r$sel_region,
  tipo = r$sel_tipo,
  tematica = r$sel_tematica,
  indicador = r$indicador,
  grupo = r$sel_grupo,
  subregiones = TRUE,  # Always TRUE for maps
  with_parent = FALSE,
  con = con,
  conmap = conmap
)
result

# Map Deptos region - Amazonas
# region: amazonas
# grupo: NULL
# tipo: registros
# tematica: NULL
# indicador: registros_region_total
# subregiones: TRUE
# with_parent: FALSE

r <- list(
  sel_region = "amazonas",
  sel_tipo = "registros",
  sel_grupo_tipo = "biologico",
  sel_grupo = NULL,
  sel_tematica = NULL,
  sel_indicador = "especies_region_total",
  subregiones = TRUE,
  with_parent = FALSE
)
# Convert dashes to underscores for API compatibility
tematica_api <- if(!is.null(r$sel_tematica)) gsub("-", "_", r$sel_tematica) else r$sel_tematica

d <- sibdata(
  region = r$sel_region,
  grupo = r$sel_grupo,
  tipo = r$sel_tipo,
  tematica = tematica_api,
  indicador = r$indicador,
  subregiones = r$subregiones,
  con = con
)

data <- d

result <- choropleth_map(
  data = data,
  region = r$sel_region,
  tipo = r$sel_tipo,
  tematica = r$sel_tematica,
  indicador = r$indicador,
  grupo = r$sel_grupo,
  con = con,
  conmap = conmap
)
result




# Map Special region
## The map has no values

r <- list(
  sel_region = "region-amazonia",
  sel_tipo = "especies",
  sel_grupo_tipo = "biologico",
  sel_grupo = NULL,
  sel_tematica = "endemicas",
  subregiones = FALSE,
  with_parent = FALSE
)
# Convert dashes to underscores for API compatibility
tematica_api <- if(!is.null(r$sel_tematica)) gsub("-", "_", r$sel_tematica) else r$sel_tematica

d <- sibdata(
  region = r$sel_region,
  grupo = r$sel_grupo,
  tipo = r$sel_tipo,
  tematica = tematica_api,
  indicador = r$indicador,
  con = con
)

data <- d

result <- choropleth_map(
  data = data,
  region = r$sel_region,
  tipo = r$sel_tipo,
  tematica = r$sel_tematica,
  indicador = r$indicador,
  grupo = r$sel_grupo,
  con = con,
  conmap = conmap
)
result





## GENERAL INPUT COMBINATIONS

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
hg_pie_CatNum(d)

list_species(region = "colombia",
             grupo = "",
             tematica = "",
             con = con)


list_species(region = "colombia",
             grupo = "",
             tematica = "cites",
             con = con)



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
hg_bar_CatNum(iris |> select(5,4), color_palette_categorical = NULL, color_by = NULL)
hg_pie_CatNum(d)



input <- list(
  region = "colombia",
  tipo = "especies",
  grupo_tipo = "biologico",
  grupo = NULL,
  # tematica = "exoticas_exoticas_riesgo_invasion",
  tematica = "exoticas_riesgo_invasion",
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
hg_pie_CatNum(d)








input <- list(
  region = "boyaca",
  grupo = "animales",
  tipo = "especies",
  tematica = "amenazadas_nacional",
  indicador <- NULL,
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
hg_pie_CatNum(d, opts = list(color_palette_categorical = palette))


## Cites

input <- list(
  region = "boyaca",
  grupo = "animales",
  tipo = "especies",
  tematica = "cites",
  indicador = NULL,
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
opts <- list(
  color_palette_categorical = palette,
  legend_align="right",
  legend_vertical_align = "middle",
  axis_text_wrap = 100
  )

hg_pie_CatNum(d, opts = opts)

esp <- list_species(region = inp$region,
                    grupo = inp$grupo,
                    tematica = inp$tematica,
                    con = con) |> collect()



## Exoticas total

input <- list(
  region = "boyaca",
  grupo = "animales",
  tematica = "exoticas",
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

## Invasoras

input <- list(
  region = "boyaca",
  grupo = NULL,
  tematica = "exoticas",
  #indicador = NULL,
  indicador = "registros_invasoras",
  subregiones = TRUE,
  with_parent = FALSE
)
inp <- input
region <- inp$region
d <- sibdata(inp$region,
             grupo = inp$grupo,
             tipo = inp$tipo,
             cobertura = inp$cobertura,
             tematica = inp$tematica,
             indicador = inp$indicador,
             subregiones = inp$subregiones,
             with_parent = inp$with_parent,
             con = con)
d




##


input <- list(
  region = "colombia",
  grupo = NULL,
  tipo = "registros",
  tematica = NULL,
  indicador = NULL,
  subregiones = TRUE,
  with_parent = FALSE
)
inp <- input
region <- inp$region
d <- sibdata(region = inp$region,
             grupo = inp$grupo,
             tipo = inp$tipo,
             cobertura = inp$cobertura,
             tematica = inp$tematica,
             indicador = inp$indicador,
             subregiones = inp$subregiones,
             with_parent = inp$with_parent,
             con = con)
d



input <- list(
  region = "colombia",
  grupo = "aracnidos",
  tipo = "registros",
  tematica = NULL,
  indicador = NULL,
  subregiones = TRUE,
  with_parent = FALSE
)
inp <- input
region <- inp$region
d <- sibdata(region = inp$region,
             grupo = inp$grupo,
             tipo = inp$tipo,
             cobertura = inp$cobertura,
             tematica = inp$tematica,
             indicador = inp$indicador,
             subregiones = inp$subregiones,
             with_parent = inp$with_parent,
             con = con)
d





###


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



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
  region = "boyaca",
  grupo = "animales",
  tematica = "amenazadas_nacional",
  subregiones = FALSE,
  with_parent = FALSE
)
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










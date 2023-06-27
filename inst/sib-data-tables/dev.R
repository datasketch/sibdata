
library(tidyverse)
library(shinypanels)
library(shiny)
library(DT)
library(hgchmagic)
library(lfltmagic)
library(sibdata)
library(shinyinvoer)
library(dsmodules)
library(dbplyr)


con <- DBI::dbConnect(RSQLite::SQLite(), sys_file("db/sibdata.sqlite"),
                      read_only = TRUE)


opts_grupo_biologico <- c("Todos" = "todos",
                          sib_available_grupos(tipo = "biologico", con))
opts_grupo_interes <-  c("Todos" = "todos",
                         sib_available_grupos(tipo = "interes", con))

opts_region <- c(sib_available_regions(subtipo = "País"),
                 sib_available_regions(subtipo = "Departamento", con))
# opts_region <- c("colombia", "narino", "boyaca", "santander", "tolima",
#                  "resguardo-indigena-pialapi-pueblo-viejo",
#                  "reserva-natural-la-planada")

opts_tematicas <- c("Todas" = "todas", sib_available_tematicas())


input <- list(
  region = "boyaca",
  tipo = "registros",
  grupo_tipo = "biologico",
  grupo = "animales",
  tematica = "amenazadas_nacional"
  # subregiones = subregiones,
  # with_parent = with_parent
)

input <- list(
  region = "boyaca",
  grupo = "animales",
  tematica = "amenazadas-nacional"
  # subregiones = subregiones,
  # with_parent = with_parent
)



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






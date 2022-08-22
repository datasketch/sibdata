
library(tidyverse)
library(ggmagic)
library(hgchmagic)


av_tems <- sib_available_tematicas()


## Inputs

input_gr_bio_o_int <- "biologico" # grupo biológico o de interés

input <- list(
  sel_region = "tolima",
  sel_tipo = "especies",
  sel_grupo_type = "biologico",
  sel_grupo = "todos",
  sel_cobertura = "continental"
)


# input <- list(
#   sel_grupo_type = "interes",
#   sel_region = "tolima",
#   sel_grupo_biologico = "algas",
#   sel_grupo_interes = "frailejones",
#   sel_modo = "continental",
#   region_type = "region",
#   ##
#   tematica = "amenazadas_nacioal",
#   registro_especie = "especie",
#   modo = "continental"
# )


# input <- list(
#   sel_region = "narino",
#   sel_grupo_type = "interes",
#   sel_grupo_biologico = "todos",
#   sel_grupo_interes = "todos",
#   sel_modo = "todos"
# )


## Region

region <- function(){
  input$sel_region
}
region()

## Grupo Biológico 0


## Select Grupo
sel_grupo <- function(){
  if(input$sel_grupo_type == "biologico"){
    return(input$sel_grupo_biologico)
  } else {
    return(input$sel_grupo_interes)
  }
}

sel_grupo()



data <- function(){
  region <- input$sel_region
  grupo <- input$sel_grupo
  tipo <- input$sel_tipo
  cobertura <- input$sel_cobertura
  tematica <- input$sel_tematica
  d <- sibdata(region, grupo = grupo, tipo = tipo,
               cobertura = cobertura,
               tematica = tematica,
               subregiones = subregiones,
               with_parent = with_parent)
  d
}




# Select Grupo Data
d_gr <- function(){




  if(input$sel_grupo_type == "biologico"){
    d <- ds$region_grupo_biologico
    if(sel_grupo() != "todos")
      d <- d |> filter(slug_grupo_biologico == sel_grupo())
  } else {
    d <- ds$region_grupo_interes_conservacion
    if(sel_grupo() != "todos")
      d <- d |> filter(slug_grupo_interes_conservacion == sel_grupo())
  }
  d
}
d_gr()

# Select Grupo Reg

d_gr_reg <- function(){
  d <- d_gr()
  ind_reg <- d |>
    filter(slug_region == input$sel_region)
  ind_reg
}
d_gr_reg()

# Select subreg
d_subreg <- function(){
  #d <- d_gr()
  subregion_tematica(region())
}

d_subreg()


# Select columns



vars_meta <- function(){
  inds <- ds$ind_meta
  if(input$tematica != "todas"){
    inds <- inds |>
      filter(grepl(input$tematica,tematica))
  }
  if(input$cobertura != "todos"){
    inds <- inds |>
      filter(grepl(input$modo,modo))
  }
  if(input$registro_especie != "todos"){
    inds <- inds |>
      filter(grepl(input$registro_especie,tipo))
  }
  inds |> pull(indicador)
}


data <- function(){
  #req(d_gr_reg())
  d <- d_gr_reg()
  if(input$region_type == "subregion"){
    d <- d_gr_subreg()
  }
  vars <- c("slug", vars_meta())
  d <- d |>
    select(contains(vars))

  d2 <- d |>
    pivot_longer(-starts_with("slug"),
                 names_to = c("indicador"),
                 values_to = "count")

  inds <- ds$ind_meta |>
    filter(indicador %in% names(d))
  d3 <- left_join(d2, inds)
  d4 <- d3 |>
    select_if(~length(unique(.))!= 1) |>
    select(-indicador)
  d5 <- d4 %>% relocate(count, .after = last_col())
  d5
}

d <- data()


chart_vars <- function(){
  d <- data()
  d <- d |> select(-count)
  names(d)
}



especies_amenazadas <- amenazadas |>
  filter(tipo == "especies") |>
  select(-tipo)

h <- hgchmagic::hgch_bar_CatNum(especies_amenazadas,
                                title = "Especies amenazadas nacionales")
h








######









########

indicadores_sel_region <- ds$region |>
  filter(slug == sel_region)

top_tematicas <- tematica |> filter(parent == "0") |> pull(slug)

especie_tematica <- read_delim("db-cifras-sib/especie_tematica.tsv")
esp_tematica_sel_region <- especie_tematica |>
  filter(slug_region == sel_region) |>
  select(-slug_region)


# for a given temática level, make temáticas intermediate tables
level <- top_tematicas[1]
level <- "amenazadas_nacional"


amenazadas_nacional <- indicadores_sel_region |> select(contains(level))

a <- amenazadas_nacional |>
  pivot_longer(everything(),
               names_to = c("tipo", "clasificacion"),
               names_pattern = "(.*)amenazadas_nacional(.*)",
               values_to = "count"
  )
a <- a |> mutate(tipo = gsub("^_|_$","", tipo),
                 clasificacion = gsub("_","", clasificacion))

totales <- a |> filter(nchar(clasificacion) == 0)
amenazadas <- a |> filter(!nchar(clasificacion) == 0)

especies_amenazadas <- amenazadas |>
  filter(tipo == "especies") |>
  select(-tipo)

h <- hgchmagic::hgch_bar_CatNum(especies_amenazadas,
                                title = "Especies amenazadas nacionales")
h
htmlwidgets::saveWidget(h, "narino/especies_amenazadas_nacionales.html")


h <- hgchmagic::hgch_donut_CatNum(especies_amenazadas,
                                  title = "Especies amenazadas nacionales",
                                  color_by = "clasificacion")
htmlwidgets::saveWidget(h, "narino/especies_amenazadas_nacionales-donut.html")


level <- "endemica"
endemica <- indicadores_sel_region |> select(contains(level))

a <- endemica |>
  pivot_longer(everything(),
               names_to = c("tipo"),
               names_pattern = "(.*)endemicas",
               values_to = "count"
  )
a <- a |> mutate(tipo = gsub("^_|_$","", tipo))

especies_endemicas <- a |>
  filter(grepl("especies",tipo)) |>
  filter(tipo == "especies")

h <- hgchmagic::hgch_bar_CatNum(especies_endemicas,
                                title = "Especies endémicas nacionales")
htmlwidgets::saveWidget(h, "narino/especies_endemicas.html")


esp_endemicas <- esp_tematica_sel_region |>
  filter(slug_tematica ==  "endemicas") |>
  select(slug = slug_especie) |>
  left_join(especies)

t <- DT::datatable(esp_endemicas)
htmlwidgets::saveWidget(t, "narino/esp_endemicas_list.html")


# Publicadores
pubs_sel_region <- publicadores |> filter(slug_region == "narino")
pubs_sel_region <- pubs_region |>
  filter(slug_region == "narino") |>
  left_join(pubs_sel_region,
            c("slug_publicador" = "slug"))






library(tidyverse)
library(shinypanels)
library(shiny)
library(DT)
library(hgchmagic)
library(lfltmagic)
library(sibdata)
library(shinyinvoer)
library(dsmodules)


con <- DBI::dbConnect(RSQLite::SQLite(),
                      sys_file_sibdata("db/sibdata.sqlite"),
                      read_only = TRUE)

opts_grupo_biologico <- c("Todos" = "todos",
                          sib_available_grupos(tipo = "biologico", con))
opts_grupo_interes <-  c("Todos" = "todos",
                         sib_available_grupos(tipo = "interes", con))

# opts_region <- c(sib_available_regions(subtipo = "País", con = con),
#                  sib_available_regions(subtipo = "Departamento", con = con))

opts_region <- c(
  "Colombia" = "colombia",
  "Boyacá" = "boyaca",
  "Nariño" = "narino",
  "Santander" = "santander",
  "Tolima" = "tolima"
)

opts_tematicas <- c("Todas" = "todas", sib_available_tematicas())


ui <- panelsPage(
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href="custom.css")
  ),
  shinypanels::panel(title = "Opciones", width = 300,
                     collapsable = FALSE,
                     body = div(
                       #verbatimTextOutput("debug"),
                       uiOutput("sel_region_"),
                       hr(),
                       #radioButtons("sel_tipo", "Tipo", c("Observaciones" = "registros","Especies"="especies")),
                       # hr(),
                       uiOutput("sel_grupo_tipo"),
                       uiOutput("sel_grupo_opts"),
                       hr(),
                       uiOutput("sel_tematica_")
                       ,
                       br()
                     ),
                     footer = ""),
  shinypanels::panel(title = "Especies",
                     #width = 600,
                     collapsable = FALSE,
                     head = downloadTableUI("species_table",
                                            dropdownLabel = "Descargar especies",
                                            formats = c("csv", "xlsx", "json"),
                                            display = "dropdown",
                                            dropdownWidth = 200),
                     body = list(
                       verbatimTextOutput("debug"),
                       dataTableOutput("list_species"),
                       br()
                       )
  )
)

server <-  function(input, output, session) {


  par <- list(region = NULL, tematica = NULL, grupo = NULL)
  url_par <- reactive({
    url_params(par, session)$inputs
  })

  output$debug <- renderPrint({
    #capture.output(str(data()))
    #glimpse(data())
    #input$sel_grupo
    #glimpse(inputs())
    #summary(list(a=1, b= "x"))
    # what <- c(input$sel_grupo_type, inputs())
    # what <- data()
    # what <- inputs()
    # what <- input$viz_selection
    # paste0(capture.output(what),collapse = "\n")
    #url_par()
    str(inputs())
    str(data_especies())
    names(data_especies())
  })


  output$sel_region_ <- renderUI({
    req(opts_region)
    default_select <- NULL
    if (!is.null(url_par()$region)) default_select <- tolower(url_par()$region)
    selectizeInput("sel_region","Seleccione Región",
                   opts_region,
                   selected = default_select
    )
  })


  output$sel_grupo_tipo <- renderUI({

    radioButtons("sel_grupo_type", "Tipo de grupo",
                 c("Biológico" = "biologico", "Interés de Conservación" = "interes"))
  })


  output$sel_grupo_opts <- renderUI({
    req(input$sel_grupo_type)
    default_select <- NULL
    if (!is.null(url_par()$grupo)) default_select <- tolower(url_par()$grupo)

    opts <- opts_grupo_interes
    id <- "sel_grupo_int"
    if (input$sel_grupo_type == "biologico") {
      opts <- opts_grupo_biologico
      id <- "sel_grupo_bio"
    }

    selectizeInput(id ,
                   "Seleccione grupo",
                   opts,
                   default_select)

  })



  output$sel_tematica_ <- renderUI({
    req(opts_tematicas)
    default_select <- NULL
    if (!is.null(url_par()$tematica)) default_select <- tolower(url_par()$tematica)
    radioButtons("sel_tematica", "Temática", opts_tematicas, selected = default_select)
  })

  inputs <- reactive({

    req(input$sel_grupo_type)
    req(input$sel_tematica)

    grupo <- ifelse(input$sel_grupo_type == "biologico",
                    input$sel_grupo_bio, input$sel_grupo_int)
    if(grupo == "todos") grupo <- NULL
    tematica <- input$sel_tematica
    if (tematica == "todas") tematica <- NULL

    list(
      region = input$sel_region,
      grupo = grupo,
      tematica = tematica
    )
  })


  data_especies <- reactive({
    # req(input$sel_grupo_type)
    # req(input$sel_tematica)

    esp <- list_species(region = inputs()$region,
                        grupo = inputs()$grupo,
                        tematica = inputs()$tematica,
                        #with_labels = TRUE
                        con = con) |> collect()
    vars <- c("label", "registros", "url_gbif", "url_cbc", "kingdom",
              "phylum", "class", "order", "family", "genus")

    esp <- esp |>
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
  })

  output$list_species <- renderDataTable({

    req(data_especies())
    l_s <- data_especies()
    l_s$GBIF <- paste0("<a href='",l_s$url_gbif,"'  target='_blank'>","GBIF","</a>")
    l_s$CBC <- paste0("<a href='",l_s$url_cbc,"'  target='_blank'>","CBC","</a>")
    l_s$CBC[l_s$CBC == "<a href='NA'  target='_blank'>NA</a>"] <- ""
    l_s$GBIF[l_s$GBIF == "<a href='NA'  target='_blank'>NA</a>"] <- ""
    l_s$url_gbif <- NULL
    l_s$url_cbc <- NULL
    l_s <- l_s |> relocate(GBIF, .after = "Registros")
    l_s <- l_s |> relocate(CBC, .after = "GBIF")
    DT::datatable(l_s,
                  rownames = F,
                  selection = 'none',
                  escape = FALSE,
                  #extensions = 'Buttons',
                  options = list(
                    dom = 'Bftsp',
                    buttons = c('copy', 'csv', 'xlsx'),
                    language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                    scrollX = T,
                    #fixedColumns = TRUE,
                    #fixedHeader = TRUE,
                    searching = FALSE,
                    info = FALSE,
                    #scrollY = "700px",
                    initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).css({'background-color': '#4ad3ac', 'color': '#ffffff'});",
                      "}")
                  ))

  })


  sel_grupo <- reactive({
    if(input$sel_grupo_type == "biologico"){
      return(input$sel_grupo_biologico)
    } else {
      return(input$sel_grupo_interes)
    }
  })






  output$descargas <- renderUI({
    downloadTableUI("species_table", dropdownLabel = "Descargar", formats = c("csv", "xlsx", "json"), display = "dropdown")
  })

  downloadTableServer("species_table", element = reactive(data_especies()), formats = c("csv", "xlsx", "json"))



}

shinyApp(ui, server)

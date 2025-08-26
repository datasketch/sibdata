# Minimal Cards Demo App
# Tests the cards visualization independently

library(shiny)
library(sibdata)
library(shinyjs)

# Connection
con <- DBI::dbConnect(RSQLite::SQLite(), sys_file_sibdata("db/sibdata.sqlite"), read_only = TRUE)


regiones <- c("colombia", "region-amazonia", "amazonas")
grupos <- c("","animales", "algas")

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Cards demo"),
  sidebarLayout(
    sidebarPanel(
      selectInput("region", "Región",
                  choices = regiones),
      selectInput("grupo", "Grupo",
                  choices = grupos)
    ),
    mainPanel(
      uiOutput("cards")
    )
  )
)

server <- function(input, output, session){

  fetch_indicator_value <- function(region, grupo, ind_key){
    d <- tryCatch({
      sibdata(
        region = region,
        grupo = if (nzchar(grupo)) grupo else NULL,
        tipo = if (grepl("^especies", ind_key)) "especies" else "registros",
        tematica = NULL,
        indicador = ind_key,
        subregiones = FALSE,
        with_parent = FALSE,
        con = con
      )
    }, error = function(e){ NULL })
    if (is.null(d) || !ind_key %in% names(d)) return(NA_real_)
    val <- suppressWarnings(as.numeric(d[[ind_key]][1]))
    if (is.na(val)) 0 else val
  }

  output$cards <- renderUI({
    req(input$region)
    ind_regs <- "registros_region_total"
    ind_esps <- "especies_region_total"

    val_regs <- fetch_indicator_value(input$region, input$grupo, ind_regs)
    val_esps <- fetch_indicator_value(input$region, input$grupo, ind_esps)

    labels <- sib_merge_ind_label(c(ind_regs, ind_esps), con = con)
    label_regs <- if (!is.null(names(labels))) labels[[ind_regs]] else labels[1]
    label_esps <- if (!is.null(names(labels))) labels[[ind_esps]] else labels[2]

    card_css <- "display: flex; gap: 12px; justify-content: space-between; flex-wrap: wrap;"
    box_css <- "flex: 1; min-width: 180px; border: 1px solid #e6e6e6; border-radius: 8px; padding: 16px; background: #ffffff; box-shadow: 0 1px 2px rgba(0,0,0,0.05);"
    value_css <- "font-size: 28px; font-weight: 700; color: #09A274; margin: 0;"
    label_css <- "font-size: 13px; color: #666666; margin: 0; margin-top: 6px;"

    div(
      style = card_css,
      div(style = box_css,
          p(style = value_css, format(val_regs, big.mark = ",", scientific = FALSE)),
          p(style = label_css, label_regs)
      ),
      div(style = box_css,
          p(style = value_css, format(val_esps, big.mark = ",", scientific = FALSE)),
          p(style = label_css, label_esps)
      )
    )
  })

  session$onSessionEnded(function(){
    DBI::dbDisconnect(con)
  })
}

shinyApp(ui, server)

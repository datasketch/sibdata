# Test app for exp_inputs_grupo module
# Uses real database connection

library(shiny)
library(shinyjs)
library(sibdata)

# Get database connection
con <- DBI::dbConnect(RSQLite::SQLite(), sys_file_sibdata("db/sibdata.sqlite"),
                      read_only = TRUE)

# Get app options (same as in main app)
app_options <- get_app_options(con, debug = TRUE)

###### APP
# Simple test UI
ui <- fluidPage(
  titlePanel("Test Grupo Module"),
  sidebarLayout(
    sidebarPanel(
      exp_inputs_grupo_ui("test_grupo")
    ),
    mainPanel(
      verbatimTextOutput("selected_grupo"),
      verbatimTextOutput("grupo_options")
    )
  )
)

# Test server
server <- function(input, output, session) {
  selected_grupo <- exp_inputs_grupo_server("test_grupo", app_options, session, debug = TRUE)

  output$selected_grupo <- renderPrint({
    grupo_result <- selected_grupo()
    if (!is.null(grupo_result)) {
      cat("Selected grupo type:", grupo_result$type, "\n")
      cat("Selected grupo value:", grupo_result$value, "\n")
    } else {
      cat("No grupo selected\n")
    }
  })

  output$grupo_options <- renderPrint({
    cat("Available options:\n")
    cat("Biological groups:", length(app_options$grupo_biologico), "\n")
    cat("Interest groups:", length(app_options$grupo_interes), "\n")
    cat("\nFirst few biological groups:\n")
    cat(paste(head(app_options$grupo_biologico, 5), collapse = ", "), "\n")
    cat("\nFirst few interest groups:\n")
    cat(paste(head(app_options$grupo_interes, 5), collapse = ", "), "\n")
  })
}

# Run the app
shinyApp(ui = ui, server = server) 
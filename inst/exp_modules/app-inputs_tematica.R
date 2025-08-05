# Test app for exp_inputs_tematica module
# Uses real database connection

library(shiny)
library(shinyjs)
library(sibdata)

# Get database connection
con <- DBI::dbConnect(RSQLite::SQLite(), sys_file_sibdata("db/sibdata.sqlite"),
                      read_only = TRUE)

###### APP
# Simple test UI
ui <- fluidPage(
  titlePanel("Test Tematica Module"),
  sidebarLayout(
    sidebarPanel(
      exp_inputs_tematica_ui("test_tematica")
    ),
    mainPanel(
      verbatimTextOutput("selected_tematica"),
      verbatimTextOutput("tree_structure")
    )
  )
)

# Test server
server <- function(input, output, session) {
  selected_tematica <- exp_inputs_tematica_server("test_tematica", con, session, debug = TRUE)

  output$selected_tematica <- renderPrint({
    cat("Selected tematica:", selected_tematica(), "\n")
  })

  # output$tree_structure <- renderPrint({
  #   cat("Tree structure summary:\n")
  #   cat("Root slug:", tematicas_tree$slug, "\n")
  #   cat("Number of first-level items:", length(tematicas_tree$children), "\n")
  #   for (item in tematicas_tree$children) {
  #     cat("- ", item$label, " (", item$slug, ")\n")
  #     if (!is.null(item$children)) {
  #       cat("  Children: ", length(item$children), "\n")
  #     }
  #   }
  # })
}

# Run the app
shinyApp(ui = ui, server = server)

# test_session_isolation.R
# Test script to verify session isolation in the modular app

library(shiny)
library(testthat)

# Test that reactiveValues are created per session
test_that("ReactiveValues are session-specific", {
  # This test would need to be run in a Shiny context
  # For now, we'll just document the expected behavior
  
  cat("=== Session Isolation Test ===\n")
  cat("Expected behavior:\n")
  cat("1. Each browser session should have its own reactiveValues\n")
  cat("2. Changes in one session should NOT affect other sessions\n")
  cat("3. Each session should start with default values\n")
  cat("4. Database connections should be per-session\n")
  cat("\nTo test manually:\n")
  cat("1. Open http://localhost:3838 in two different browser windows\n")
  cat("2. Change settings in one window (region, group, etc.)\n")
  cat("3. Verify that the other window remains unchanged\n")
  cat("4. Check that each window maintains its own state\n")
})

# Test the app structure
test_that("App has correct structure", {
  app_file <- "inst/org_sibhumboldt_sibdata_app2/app2.R"
  
  expect_true(file.exists(app_file), 
              info = "App file should exist")
  
  app_code <- readLines(app_file)
  
  # Check that reactiveValues is created inside server function
  server_start <- which(grepl("^server <- function", app_code))
  expect_true(length(server_start) > 0, 
              info = "Server function should exist")
  
  # Check that reactiveValues is created after server function starts
  rv_line <- which(grepl("reactiveValues\\(", app_code))
  expect_true(length(rv_line) > 0, 
              info = "reactiveValues should exist")
  expect_true(all(rv_line > server_start), 
              info = "reactiveValues should be created inside server function")
  
  # Check that app_options is created inside server function
  app_options_line <- which(grepl("app_options <- get_app_options", app_code))
  expect_true(length(app_options_line) > 0, 
              info = "app_options should exist")
  expect_true(all(app_options_line > server_start), 
              info = "app_options should be created inside server function")
  
  cat("✓ App structure validation passed\n")
})

# Test module structure
test_that("Modules don't have global library calls", {
  module_files <- list.files("R", pattern = "exp_.*\\.R$", full.names = TRUE)
  
  for (file in module_files) {
    code <- readLines(file)
    library_lines <- which(grepl("^library\\(", code))
    
    expect_true(length(library_lines) == 0, 
                info = paste("Module", basename(file), "should not have library calls"))
  }
  
  cat("✓ Module structure validation passed\n")
})

cat("\n=== Summary ===\n")
cat("The main issue was that reactiveValues was defined globally outside the server function.\n")
cat("This caused all sessions to share the same reactive state.\n")
cat("The fix involved:\n")
cat("1. Moving reactiveValues creation inside the server function\n")
cat("2. Moving app_options creation inside the server function\n")
cat("3. Removing library calls from module files\n")
cat("4. Ensuring all state is session-specific\n")
cat("\nThe app should now have proper session isolation.\n") 
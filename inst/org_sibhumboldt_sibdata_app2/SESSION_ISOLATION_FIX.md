# Session Isolation Fix for SIB Data App (Modular Version)

## Problem Description

The modular version of the SIB Data App (`app2.R`) had a critical issue where **reactive values were shared across all user sessions**. This meant that when one user changed settings (region, group, theme, etc.), those changes would affect all other users of the application.

## Root Cause

The issue was caused by defining `reactiveValues` and other state variables **globally** outside the server function:

```r
# ❌ WRONG - Global reactiveValues (shared across all sessions)
r <- reactiveValues(
  sel_region = NULL,
  sel_grupo_type = "biologico",
  # ... other values
)

ui <- fluidPage(...)
server <- function(input, output, session) {
  # Server logic here
}
```

In Shiny, when `reactiveValues` is created outside the server function, it becomes a **global variable** that is shared across all sessions. This violates the fundamental principle of session isolation in web applications.

## Solution

### 1. Move reactiveValues Inside Server Function

```r
# ✅ CORRECT - Session-specific reactiveValues
ui <- fluidPage(...)
server <- function(input, output, session) {
  # Create session-specific reactive values
  r <- reactiveValues(
    sel_region = NULL,
    sel_grupo_type = "biologico",
    sel_grupo = NULL,
    sel_tematica = NULL,
    sel_tipo = "registros",
    chart_type = "map",
    amenazadas_categoria = NULL,
    cites_categoria = NULL,
    exotica_categoria = NULL,
    especies_total_estimadas = NULL,
    indicador = NULL,
    show_subcategoria = FALSE,
    show_especies_total_estimadas = FALSE,
    current_subcategory = NULL,
    main_data = NULL,
    species_data = NULL,
    map_data = NULL,
    breadcrumb = NULL,
    available_charts = NULL
  )
  
  # Rest of server logic
}
```

### 2. Move App Options Inside Server Function

```r
# ✅ CORRECT - Session-specific app options
server <- function(input, output, session) {
  # Create session-specific app options
  temp_con <- get_app_connection("db/sibdata.sqlite")
  app_options <- get_app_options(temp_con)
  DBI::dbDisconnect(temp_con)
  
  # Create session-specific reactive values
  r <- reactiveValues(...)
  
  # Rest of server logic
}
```

### 3. Remove Global Library Calls from Modules

Removed `library(shinyinvoer)` from `R/exp_chart_selector.R` to prevent potential conflicts and ensure proper dependency management.

## Files Modified

1. **`inst/org_sibhumboldt_sibdata_app2/app2.R`**
   - Moved `reactiveValues` creation inside server function
   - Moved `app_options` creation inside server function
   - Ensured all state is session-specific

2. **`R/exp_chart_selector.R`**
   - Removed global `library(shinyinvoer)` call

## Testing

### Manual Testing
1. Open the app in two different browser windows: `http://localhost:3838`
2. Change settings in one window (region, group, theme, etc.)
3. Verify that the other window remains unchanged
4. Check that each window maintains its own state

### Automated Testing
Run the test script to verify the fix:
```r
source('inst/org_sibhumboldt_sibdata_app2/test_session_isolation.R')
```

## Benefits

1. **Session Isolation**: Each user now has their own independent session
2. **No Cross-Contamination**: Changes in one session don't affect others
3. **Proper State Management**: All reactive values are session-specific
4. **Database Connection Safety**: Each session has its own database connection
5. **Scalability**: App can now handle multiple concurrent users safely

## Best Practices Applied

1. **Session-Specific State**: All reactive values created inside server function
2. **Resource Management**: Database connections created per session
3. **Dependency Management**: Library calls only in main app file
4. **Modular Design**: Clean separation between UI, server, and modules
5. **Testing**: Comprehensive tests to verify session isolation

## Verification

The app is now running successfully at `http://localhost:3838` with proper session isolation. Each browser session maintains its own state independently. 
# TODO Checklist for Modularization

- [x] Create R/ directory structure with exp_ prefix
- [x] Create exp_utils_ui.R with helper functions
- [x] Set up centralized reactive values structure in main app
- [x] Create database connection utilities

- [x] Implement exp_inputs.R (Input Controls Module)
    - [x] UI and server functions
    - [x] Input handling and URL parameters
    - [x] Reactive value updates
    - [x] Data.tree hierarchical group selector
    - [x] Conditional UI for biological vs interest groups

- [x] Implement exp_species_table.R (Species Table Module)
    - [x] UI and server functions
    - [x] Species data fetching and display
    - [x] Custom DataTable styling with Spanish localization
    - [x] External links to GBIF and CBC
    - [ ] Download functionality (pending visualization module)

- [x] Create app to test the functionalities up to this point with inputs and species table
- [x] Add debug output for reactive values monitoring
- [x] Fix data formatting and URL parameter handling
- [x] Debug and fix amenazadas tematica issue (species disappearing)
- [x] Test URL parameters functionality thoroughly
- [x] Fix database connection issue (move to server to avoid hangs)

- [x] Implement exp_visualization.R (Visualization Module)
    - [x] UI and server functions
    - [x] Map and table rendering with choropleth_map
    - [x] Chart type selector (Map/Table)
    - [x] Breadcrumb functionality 
    - [x] Download data functionality
    - [x] Integration with reactive values system

- [x] Replace shinypanels with standard fluidPage layout
    - [x] Implement 3-column responsive design  
    - [x] Integrate all modules in main app file

- [ ] Test complete data flow and reactive dependencies
- [ ] Debug and optimize performance
- [ ] Update README.app2.md and module documentation

## Current Status: ✅ FULLY FUNCTIONAL MODULAR APP (Phase 1 Complete)

### 🎯 Phase 1 - Core Modularization (COMPLETED)
The app now has:
- ✅ **Modular structure**: Clean separation of concerns with exp_ prefix modules
- ✅ **Centralized reactive values**: All state managed through r <- reactiveValues()
- ✅ **Input controls module**: Hierarchical biological groups with data.tree, URL parameter support
- ✅ **Species table module**: Real-time data fetching, custom DataTable styling, full-screen modal with downloads
- ✅ **Basic visualization module**: Map/table switching with choropleth mapping and downloads
- ✅ **Custom download system**: Replaced dsmodules with HTML5 dropdown supporting CSV, XLSX, JSON
- ✅ **Responsive design**: Mobile-friendly horizontal scrolling, 300px minimum column widths
- ✅ **Database optimization**: Fixed connection hanging issues, proper cleanup
- ✅ **URL routing**: Support for all regions including amazonas, caqueta via URL parameters
- ✅ **Debug panel**: 300px height monitoring reactive state
- ✅ **Custom styling**: Green theme matching original app, subtle scrollbars

### 🔧 Technical Implementation Details
- **Modules**: exp_inputs.R, exp_species_table.R, exp_visualization.R, exp_download.R, exp_utils_ui.R
- **Layout**: Standard Bootstrap 3-column layout (320px, 480px, 320px minimum widths)
- **State management**: Single r reactive values object shared across all modules
- **Data flow**: URL params → inputs → reactive values → data fetching → visualization/table updates
- **Performance**: Optimized database connections, fixed reactive loops, efficient data processing

---

## 🚀 Phase 2 - Advanced Visualization Module (NEXT)

### 📋 Phase 2 Objectives
Transform the basic visualization module into a comprehensive chart system matching the original app's functionality with multiple visualization types and dynamic chart selection.

### 🎯 Key Features to Implement

#### 1. **Image Button Chart Selector** (Like Original App)
- Replace basic radio buttons with image-based chart selector
- Visual icons for different chart types (pie, bar, map, table)
- Dynamic chart availability based on data combinations
- Tooltips and hover effects for better UX

#### 2. **hgmagic Chart Integration**
Based on `dev.R` analysis, implement support for:
- **Pie Charts**: `hg_pie_CatNum()` for categorical/numerical data
- **Bar Charts**: `hg_bar_CatNum()` for categorical/numerical data  
- **Dynamic palettes**: Theme-specific color schemes
  - **Amenazadas**: Red/Orange/Yellow (`#FF0000`, `#FFA500`, `#FFFF00`)
  - **CITES**: Blue/Black/Yellow/Green (`#00AFFF`, `#000000`, `#FFD150`, `#4DD3AC`)
  - **Default**: System default colors

#### 3. **Chart Logic Based on Data Combinations**
- **Map view**: When `subregiones = TRUE` and geographic data available
- **Pie/Bar charts**: When categorical data with counts available
- **Table view**: Always available as fallback
- **Chart availability logic**: Dynamic based on `r$sel_tipo`, `r$sel_tematica`, `r$sel_grupo`

#### 4. **Enhanced Visualization Module Structure**
```r
# Updated exp_visualization.R structure
exp_visualization_ui <- function(id) {
  tagList(
    # Chart type selector with images
    exp_chart_selector_ui(ns("chart_selector")),
    
    # Visualization area
    uiOutput(ns("viz_output")),
    
    # Chart options (when applicable)
    uiOutput(ns("chart_options"))
  )
}
```

### 📁 New Files to Create

#### 1. **exp_chart_selector.R**
- Image-based chart type selector
- Dynamic chart availability logic
- Integration with existing reactive values

#### 2. **exp_charts.R** 
- hgmagic chart rendering functions
- Palette management for different themes
- Chart options and customization

### 🔄 Data Flow Enhancement
```
User Input → Chart Type Selection → Data Validation → Chart Rendering
     ↓              ↓                    ↓              ↓
   r values    → Available Charts  → Data Processing → hgmagic/leaflet
```

### 🎨 Visual Design Requirements
- Match original app's chart selector layout
- Maintain green theme (#09A274) consistency
- Responsive image buttons with active states
- Smooth transitions between chart types

### 📊 Chart Type Decision Matrix
| Data Type | Tematica | Subregiones | Available Charts |
|-----------|----------|-------------|------------------|
| especies | NULL/todas | TRUE | Map, Table |
| especies | amenazadas | TRUE | Map (red palette), Pie, Table |
| especies | cites | TRUE | Map (blue palette), Pie, Table |
| especies | exoticas | FALSE | Pie, Bar, Table |
| registros | NULL/todas | TRUE | Map, Table |
| registros | amenazadas | TRUE | Map (red palette), Bar, Table |

### 🧪 Testing Strategy
- Test each chart type with different data combinations
- Verify palette application for themed data
- Validate chart selector responsiveness
- Performance testing with large datasets

### 📋 Implementation Checklist
- [ ] Create exp_chart_selector.R module
- [ ] Create exp_charts.R for hgmagic integration  
- [ ] Update exp_visualization.R with new chart selector
- [ ] Implement palette management system
- [ ] Add chart availability logic
- [ ] Create image assets for chart selector buttons
- [ ] Update CSS for chart selector styling
- [ ] Test all chart type combinations
- [ ] Performance optimization
- [ ] Documentation updates

---

# Modularization Plan for SIB Colombia Biodiversity Data Explorer (App2)

## Overview
Transform the monolithic `app.R` into a modular Shiny application with proper separation of concerns, centralized reactive values, and standard Shiny layout. All Shiny app-related files will be placed in the `R/` directory with the `exp_` prefix to distinguish them from general package functions.

## Directory Structure
```
R/
├── exp_inputs.R            # Input controls module
├── exp_visualization.R     # Visualization module  
├── exp_species_table.R     # Species table module
├── exp_utils_data.R        # Data processing utilities for the app
├── exp_utils_ui.R          # UI helper functions for the app
```

## Centralized Reactive Values
- Move ALL reactive elements into `r <- reactiveValues()`
- Create comprehensive reactive state management
- Define clear reactive value naming conventions

```r
r <- reactiveValues(
  # User inputs
  sel_region = NULL,
  sel_grupo_type = "biologico", 
  sel_grupo = NULL,
  sel_tematica = "todas",
  sel_tipo = "registros",
  chart_type = "map",
  
  # Data control states
  amenazadas_categoria = NULL,
  cites_categoria = NULL,
  exotica_categoria = NULL,
  especies_total_estimadas = NULL,
  
  # Computed states
  indicador = NULL,
  show_subcategoria = FALSE,
  show_especies_total_estimadas = FALSE,
  current_subcategory = NULL,
  
  # Data
  main_data = NULL,
  species_data = NULL,
  
  # UI states
  breadcrumb = NULL,
  available_charts = NULL
)
```

## Modules and Utilities

### exp_inputs.R (Input Controls Module)
**Purpose**: Handle all user input controls and URL parameter updates

**Inputs**:
- Region selection dropdown
- Group type radio buttons (Biológico/Interés)
- Group selection (conditional on type)
- Thematic category selection
- Type selection (Observaciones/Especies)

**Outputs**:
- Updates `r$sel_region`, `r$sel_grupo_type`, `r$sel_grupo`, `r$sel_tematica`, `r$sel_tipo`
- Handles URL parameter initialization
- Manages conditional UI for group selection

**Module Structure**:
```r
exp_inputs_ui <- function(id) { ... }
exp_inputs_server <- function(id, r, opts_region, opts_grupo_biologico, opts_grupo_interes, opts_tematicas) { ... }
```

### exp_visualization.R (Visualization Module)
**Purpose**: Handle chart type selection, data controls, and visualization rendering

**Sub-components**:
- Chart type selector (image buttons)
- Dynamic data controls (subcategories, total/estimadas)
- Breadcrumb display
- Visualization area (map/chart/table)
- Download controls

**Inputs from r**:
- `r$sel_tipo`, `r$sel_tematica`, `r$chart_type`
- `r$main_data`, `r$indicador`

**Outputs to r**:
- `r$chart_type`, `r$amenazadas_categoria`, `r$cites_categoria`
- `r$show_subcategoria`, `r$show_especies_total_estimadas`

**Module Structure**:
```r
exp_visualization_ui <- function(id) { ... }
exp_visualization_server <- function(id, r, ...) { ... }
```

### exp_species_table.R (Species Table Module)
**Purpose**: Display and manage the species list table

**Features**:
- Dynamic species summary text
- Paginated data table
- Download functionality
- Reactive updates based on filter changes

**Inputs from r**:
- `r$sel_region`, `r$sel_grupo`, `r$sel_tematica`
- `r$current_subcategory`

**Outputs to r**:
- `r$species_data`

**Module Structure**:
```r
exp_species_table_ui <- function(id) { ... }
exp_species_table_server <- function(id, r, ...) { ... }
```

### exp_utils_data.R (Data Utilities)
- Centralized data fetching and processing for the app
- Functions for main data and species data retrieval, processing, and validation

**Example Functions**:
```r
get_main_data <- function(params, con) { ... }
get_species_data <- function(params, con) { ... }
process_viz_data <- function(data, chart_type, params) { ... }
standardize_column_names <- function(data, indicador) { ... }
validate_data_params <- function(params) { ... }
```

### exp_utils_ui.R (UI Helper Functions)
- Helper functions for UI rendering, formatting, and layout

## Standard Shiny Layout Migration

### Replace shinypanels with Standard Layout
```r
ui <- fluidPage(
  disconnectMessage(...),
  tags$head(...),
  
  fluidRow(
    # Left column - Input controls
    column(3,
           wellPanel(
             h4("Opciones"),
             exp_inputs_ui("inputs")
           )
    ),
    
    # Center column - Visualization  
    column(6,
           wellPanel(
             div(style = "display: flex;",
                 div(class='first-container', exp_visualization_chart_selector_ui("viz")),
                 div(class='second-container', NULL)
             ),
             exp_visualization_ui("viz")
           )
    ),
    
    # Right column - Species table
    column(3,
           wellPanel(
             h4("Especies"),
             exp_species_table_ui("species")
           )
    )
  )
)
```

### Responsive Design Considerations
- Use Bootstrap grid system
- Ensure mobile compatibility
- Maintain visual hierarchy

## Data Layer Refactoring

- All app-specific data utilities go in `exp_utils_data.R`
- General package data functions remain outside the exp_ files
- Implement proper error handling and data caching where appropriate

## Implementation Steps

1. **Setup and Foundation**
    - Create R/ directory structure with exp_ prefix
    - Create `exp_utils_ui.R` with helper functions
    - Set up centralized reactive values structure
    - Create database connection utilities

2. **Input Module**
    - Implement `exp_inputs.R`
    - Test input handling and URL parameters
    - Verify reactive value updates

3. **Species Table Module**
    - Implement `exp_species_table.R`
    - Test species data fetching and display
    - Verify download functionality

4. **Visualization Module**
    - Implement basic structure of `exp_visualization.R`
    - Start with simple map and pie chart
    - Add chart type selector
    - Implement dynamic data controls
    - Add breadcrumb functionality

5. **Layout Migration**
    - Replace shinypanels with standard fluidPage layout
    - Implement 3-column responsive design
    - Test all functionality

6. **Integration and Testing**
    - Create main `app_main.R` file
    - Integrate all modules
    - Test complete data flow
    - Debug reactive dependencies
    - Performance optimization

## Module Communication Strategy

### Reactive Values as Central Hub
All modules communicate through the centralized `r` reactive values object:

```r
# In main server
r <- reactiveValues(...)

# Pass to modules
exp_inputs_server("inputs", r, ...)
exp_visualization_server("viz", r, ...)  
exp_species_table_server("species", r, ...)
```

### Data Flow
1. User interacts with inputs → `exp_inputs` updates `r` values
2. `r` value changes trigger data fetching in main server
3. Updated data stored in `r$main_data`, `r$species_data`
4. Visualization and table modules react to data changes
5. UI updates automatically through reactive dependencies

## Benefits of This Approach

1. **Maintainability**: Separated concerns, smaller files
2. **Testability**: Individual modules can be tested in isolation
3. **Reusability**: Modules can be used in other applications
4. **Debugging**: Easier to track reactive dependencies
5. **Performance**: Better control over when computations occur
6. **Scalability**: Easy to add new visualization types or features

## Migration Strategy

1. **Parallel Development**: Keep original app.R working while building modules
2. **Incremental Testing**: Test each module individually before integration
3. **Feature Parity**: Ensure all original functionality is preserved
4. **Performance Validation**: Compare performance before/after migration
5. **User Acceptance**: Test with real users to ensure UI/UX is maintained

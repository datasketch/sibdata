
# SIB Colombia Biodiversity Data Explorer

## Overview
This Shiny app is a data exploration and visualization tool for biodiversity records in Colombia, built for SIB Colombia (Sistema de Información sobre Biodiversidad de Colombia). The app provides interactive tools to explore species observations and distribution data across different regions and thematic categories.

### Core Functionality
Users can:
- Select a region (country or department)
- Filter by biological or conservation interest groups
- Choose thematic filters (e.g., threatened, endemic, migratory, CITES, exotics)
- Visualize data as maps, pie charts, bar charts, treemaps, or tables
- Download data and species lists
- See a paginated, filterable table of species with links to GBIF and CBC

## Current Application Structure

### UI Layout (shinypanels)
The application uses `shinypanels` with a 3-panel layout:

1. **Left Panel (280px)**: "Opciones" - Input controls
2. **Center Panel**: "Gráficos" - Main visualization area
3. **Right Panel (400px)**: "Especies" - Species list table

### Key Components

#### 1. Input Controls (Left Panel)
- **Region Selection**: Dropdown with countries and departments
- **Group Type**: Radio buttons for "Biológico" vs "Interés de Conservación"
- **Group Selection**: Conditional dropdowns based on group type
- **Thematic Categories**: Radio buttons for various conservation themes

#### 2. Visualization Panel (Center)
- **Type Selector**: "Observaciones" vs "Especies"
- **Chart Type Selector**: Image buttons for Map, Pie, Donut, Treemap, Bar, Table
- **Dynamic Controls**: Subcategory selectors (CITES, Amenazadas categories)
- **Breadcrumb**: Shows current filter selection
- **Download Controls**: Data export options
- **Visualization Area**: Renders charts based on selection

#### 3. Species Table (Right Panel)
- **Summary Text**: Dynamic description of current selection
- **Data Table**: Paginated species list with taxonomic information
- **Download Options**: Export species data in multiple formats

### Data Flow

#### Reactive Values (`r`)
```r
r <- reactiveValues(
  amenazadas_categoria = NULL,
  cites_categoria = NULL,
  exotica_categoria = NULL,
  especies_total_estimadas = NULL,
  indicador = NULL,
  show_subcategoria = FALSE,
  show_especies_total_estimadas = FALSE,
  breadcrumb = NULL,
  current_subcategory = NULL
)
```

#### Key Reactive Functions
1. **`inputs()`**: Consolidates all user inputs
2. **`data_params()`**: Processes inputs into database query parameters
3. **`data()`**: Fetches main visualization data using `sibdata()`
4. **`data_especies()`**: Fetches species list using `list_species()`
5. **`vizOps()`**: Prepares visualization options
6. **`l_viz()`**: Creates visualization objects

### Database Connections
- **Main DB**: SQLite connection for species data (`sibdata.sqlite`)
- **Map DB**: Geotable connection for geographic data

### Visualization Types
- **Map**: Choropleth maps using Leaflet
- **Charts**: Pie, Donut, Treemap, Bar charts using Highcharts
- **Table**: DataTable with Spanish localization

### URL Parameters
The app supports URL parameters for:
- `region`: Geographic region
- `grupo`: Biological or conservation group
- `tematica`: Thematic category

### Key Features
1. **Dynamic UI**: Controls change based on selections
2. **Subcategories**: Additional filtering for CITES and threatened species
3. **Breadcrumb Navigation**: Shows current selection path
4. **Data Downloads**: Multiple export formats
5. **Bilingual Support**: Spanish interface with English data links
6. **Real-time Updates**: Reactive data flow updates all components

### Dependencies
- `shiny`, `shinypanels`, `DT`, `leaflet`
- `hgmagic`, `dsmods`, `dsmodules`, `geotable`
- `sibdata`, `duckdbits`
- `tidyverse`, `shinyinvoer`, `shinyjs`, `shinydisconnect`

### Current Architecture Issues
1. **Monolithic Structure**: All logic in single file
2. **Complex Reactive Dependencies**: Hard to track reactive chains
3. **Mixed Concerns**: UI, server logic, and data processing intertwined
4. **Large File**: 1200+ lines difficult to maintain
5. **Reactive Values Scattered**: Not centralized in `r`
6. **Panel Layout**: Uses shinypanels instead of standard Shiny layout


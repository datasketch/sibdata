# Reactive Conditions Analysis - SIB Data App

## Chart Availability Logic (from original app lines 340-355)

```r
# ALL chart types available
charts <- c("Mapa" = "map", "Torta"= "pie", "Dona" = "donut", "Treemap" = "treemap","Barras" = "bar", "Tabla"="table")

# Restricted chart types for regular themes
map_table <- c("Mapa" = "map", "Tabla" = "table")
map_table_bar <- c("Mapa" = "map", "Tabla" = "table", "Barras" = "bar")

# Chart availability conditions:
if(!is_amenazadas_or_cites_or_exoticas()){
  if(tipo == "registros"){
    return(map_table)  # Map, Table only
  }
  if(tipo == "especies"){
    return(map_table_bar)  # Map, Table, Bar
  }
}
# For amenazadas/cites/exoticas themes: return ALL charts
```

## Control Visibility Logic (from original app observe block)

```r
# Original logic was CORRECT - subcategory should be visible ONLY for MAP charts in amenazadas themes
# CORRECT: r$show_subcategoria <- is_amenazadas_or_cites_or_exoticas() && current_chart() == "map"
# REASON: Non-map charts (bar, pie, donut, treemap) compare subcategories, so no filter needed

# Species total/estimadas selector
show_especies_total_estimadas <- current_chart() == "map" && 
  inputs()$tipo == "especies" && 
  !is_amenazadas_or_cites_or_exoticas() && 
  is.null(inputs()$tematica)
```

## Central Reactive Conditions (Best Location: app2.R main observe block)

```r
# This should be in a single centralized observe block in app2.R
observe({
  # 1. Chart availability logic
  if(!is_amenazadas_or_cites_or_exoticas()) {
    if(r$sel_tipo == "registros") {
      r$available_charts <- c("Mapa" = "map", "Tabla" = "table")
    } else if(r$sel_tipo == "especies") {
      r$available_charts <- c("Mapa" = "map", "Tabla" = "table", "Barras" = "bar")
    }
  } else {
    # For amenazadas/cites/exoticas: ALL charts available
    r$available_charts <- c("Mapa" = "map", "Torta" = "pie", "Dona" = "donut", 
                           "Treemap" = "treemap", "Barras" = "bar", "Tabla" = "table")
  }
  
  # 2. Control visibility logic
  # CORRECTED: Show subcategory controls ONLY for MAP charts in amenazadas themes
  # Non-map charts (bar, pie, donut, treemap) compare subcategories, so no filter needed
  r$show_subcategoria <- is_amenazadas_or_cites_or_exoticas() && r$chart_type == "map"
  
  # Species total/estimadas only for map + especies + regular theme
  r$show_especies_total_estimadas <- r$chart_type == "map" && 
    r$sel_tipo == "especies" && 
    !is_amenazadas_or_cites_or_exoticas() && 
    is.null(r$sel_tematica)
})

# Helper function
is_amenazadas_or_cites_or_exoticas <- function() {
  if(is.null(r$sel_tematica)) return(FALSE)
  (grepl("cites", r$sel_tematica) || grepl("amenazadas", r$sel_tematica) || grepl("exoticas", r$sel_tematica))
}
```

## Summary of Issues Found:

1. **CORRECT**: Original app correctly hides subcategory selector for non-map charts
2. **REASON**: Non-map charts (bar, pie, donut, treemap) compare subcategories, so filtering doesn't make sense
3. **FIXED**: Centralized all reactive conditions in single observe block in app2.R
4. **FIXED**: Removed chart-specific logic from individual modules, use centralized r$show_subcategoria

## Chart Type Behavior:
- **Map charts**: Show subcategory selector for filtering (e.g., show only EN species on map)
- **Comparison charts** (bar, pie, donut, treemap): Hide subcategory selector, show all subcategories for comparison
- **Table charts**: Hide subcategory selector (shows detailed data)

## Chart Data Processing Fix:
**Problem**: Charts were showing NA values instead of proper subcategory breakdown
**Root Cause**: 
1. `sib_merge_ind_label()` was called twice (once in app2.R, once in prepare_chart_data)
2. Extra columns were being added that interfered with hgmagic processing

**Solution**: 
1. Removed duplicate `sib_merge_ind_label()` call from prepare_chart_data
2. Removed unnecessary column creation (categoria)
3. Pass data to hgmagic in original format: `indicador` + `count` columns
4. Added detailed debugging to identify data structure issues

**Expected Result**: Charts now show proper subcategory breakdown:
- Registros CR categoría nacional
- Registros EN categoría nacional  
- Registros VU categoría nacional
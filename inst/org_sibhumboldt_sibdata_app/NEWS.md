# NEWS

## Version 1.2.0 (2024-04-15)

### Major Improvements
- **Map Rendering Stability**: Fixed critical issues with map rendering when switching between subcategories
- **Data Consistency**: Improved data visualization across different views and filters
- **Enhanced Error Handling**: Added better error management and user feedback

### Map and Visualization Fixes
- **#80**: Fixed missing tooltips - Map tooltips now display correctly for all municipalities
- **#51**: Corrected geographical boundaries and map rendering issues in department maps
- **#62**: Improved visualization for endemic and migratory species categories
- **#83**: Fixed invasive species filtering - The filter now works correctly without errors
- **#60**: Enhanced thematic filtering - Species tables now correctly reflect subcategory filtering
- **#69**: Added support for proper exotic species categorization - Now correctly shows the sum of all subcategories

### Navigation and URL Handling
- **#64**: Fixed URL parameter loading - Application now correctly loads group and region parameters from URLs
- **#84**: Resolved errors when filtering Bogotá data and switching between interest and biological groups

### Data Export and Display
- **#72**: Fixed CSV download inconsistencies - Empty fields now display correctly instead of showing "NA"
- **#65**: Added comprehensive data download buttons for different formats
- **#50**: Improved styling in the Endemic species table section

### Error Management
- **#73**: Improved error handling when changing filters - Application now responds gracefully to filter changes
- **#68**: Fixed disconnection errors during complex operations
- Added detailed logging for better troubleshooting

### User Interface Improvements
- **#70**: Translated all interface elements to Spanish, removing English text artifacts
- **#77**: Fixed accent marks in the exotic species section
- Improved overall application responsiveness

### Technical Enhancements
- Implemented better data column standardization for consistent visualization
- Enhanced reactivity dependencies for more reliable UI updates
- Optimized data transformation pipeline between services

### Implementation Details
The most critical fix involved standardizing data column structures while preserving original indicator columns:

```r
# Find and standardize indicator columns while preserving original structure
dd$value <- dd[[indicator_col]]
if(!"count" %in% names(dd)) {
  dd$count <- dd[[indicator_col]]
}
```

This approach ensures visualization components receive consistent data structures while maintaining compatibility with existing functions. 
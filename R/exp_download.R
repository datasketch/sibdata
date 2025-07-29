# exp_download.R
# Custom download table functionality for SIB Data App (modular version)

#' Download Table UI
#' @param id Module ID
#' @param text Download button text
#' @param formats File formats to support
#' @param display Display type: "buttons" or "dropdown"
#' @param dropdownLabel Label for dropdown
#' @param dropdownWidth Width of dropdown
#' @export
downloadTableUI <- function(id, text = "Download", formats = NULL, 
                            display = c("buttons", "dropdown"),
                            dropdownLabel = "Download", dropdownWidth = 150) {
  
  ns <- shiny::NS(id)
  
  if (is.null(formats)) formats <- "csv"
  
  formats_id <- ns(paste0("DownloadTbl", formats))
  formats_lb <- paste0(text, " ", toupper(formats))
  names(formats_id) <- formats_lb
  
  if (display == "dropdown") {
    # Create a simple dropdown using details/summary
    tags$div(
      class = "dropdown-container",
      style = "position: relative; display: inline-block;",
      tags$details(
        class = "dropdown-details",
        tags$summary(
          class = "btn btn-secondary dropdown-action-trigger btn-sm",
          style = "cursor: pointer; list-style: none; outline: none;",
          dropdownLabel,
          tags$span(
            class = "dropdown-arrow",
            style = "margin-left: 5px;",
            HTML("&#9660;")
          )
        ),
        tags$div(
          class = "dropdown-content",
          style = "position: absolute; top: 100%; left: 0; background: white; border: 1px solid #ccc; border-radius: 4px; box-shadow: 0 2px 8px rgba(0,0,0,0.1); z-index: 1000; min-width: 150px; margin-top: 2px;",
          lapply(seq_along(formats), function(i) {
            format <- formats[i]
            tags$div(
              style = "padding: 0;",
              downloadButton(
                formats_id[i],
                paste("Descargar", toupper(format)),
                class = "btn-link",
                style = "display: block; width: 100%; text-align: left; padding: 8px 16px; color: #333; text-decoration: none; border: none; background: none; border-radius: 0;"
              )
            )
          })
        )
      )
    )
  } else {
    # Button display
    div(
      lapply(seq_along(formats), function(i) {
        downloadButton(formats_id[i], formats_lb[i], class = "btn-sm btn-outline-secondary", style = "margin-right: 5px;")
      })
    )
  }
}

#' Download Table Server
#' @param id Module ID
#' @param element Reactive expression returning data to download
#' @param formats File formats to support
#' @param file_prefix Prefix for downloaded files
#' @param debug Boolean to control console debug output
#' @export
downloadTableServer <- function(id, element = NULL, formats, file_prefix = "table", debug = FALSE) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tbl_formats <- formats
    
    if (debug) message("📥 Download server initialized for ID: ", id)
    if (debug) message("📥 Formats: ", paste(formats, collapse = ", "))

    lapply(tbl_formats, function(z) {
      handler_id <- paste0("DownloadTbl", z)
      if (debug) message("📥 Creating download handler: ", handler_id)
      
      output[[handler_id]] <- downloadHandler(
        filename = function() {
          file_prefix_val <- if(is.reactive(file_prefix)) file_prefix() else file_prefix
          timestamp <- gsub("[ _:]", "-", substr(as.POSIXct(Sys.time()), 1, 19))
          filename <- paste0(file_prefix_val, "_", timestamp, ".", z)
          if (debug) message("📥 Download filename: ", filename)
          filename
        },
        content = function(file) {
          if (debug) message("📥 Download content function called for format: ", z)
          
          # Get data
          if(is.reactive(element)) {
            data_val <- element()
          } else {
            data_val <- element
          }
          
          if (debug) {
            message("📥 Data is null: ", is.null(data_val))
            if (!is.null(data_val)) {
              message("📥 Data rows: ", nrow(data_val))
              message("📥 Data columns: ", ncol(data_val))
            }
          }
          
          # Save table
          tryCatch({
            saveTable(data_val, filename = file, format = z)
            if (debug) message("📥 File saved successfully: ", file)
          }, error = function(e) {
            if (debug) message("📥 Error saving file: ", e$message)
          })
        }
      )
    })
  })
}

#' Save Table to File
#' @param tbl Data frame to save
#' @param filename Output filename
#' @param format File format (csv, xlsx, json)
saveTable <- function(tbl, filename, format = NULL) {
  
  if (is.null(format)) {
    format <- tools::file_ext(filename) %||% "csv"
  }
  
  # Remove empty rows
  c0 <- c()
  lapply(1:nrow(tbl), function (i) {
    c0[i] <<- all(is.na(tbl[i, ]))
  })
  tbl <- tbl[!c0, ]
  
  # Extract base filename without extension
  base_filename <- gsub("([^.]+)\\.[[:alnum:]]+$", "\\1", filename)
  
  if (format == "csv") {
    write.csv(tbl, paste0(base_filename, ".csv"), na = "", row.names = FALSE)
  }
  if (format == "xlsx") {
    openxlsx::write.xlsx(tbl, paste0(base_filename, ".xlsx"))
  }
  if (format == "json") {
    jsonlite::write_json(tbl, paste0(base_filename, ".json"))
  }
}
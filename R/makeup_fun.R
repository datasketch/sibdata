#' Makeup Functions
#'
#' Funciones para formatear números, caracteres y fechas.
#' Migradas del paquete makeup para uso interno.
#'
#' @name makeup_fun
NULL

#' Format character strings
#'
#' Formatea cadenas de caracteres según el estilo especificado.
#'
#' @param x Vector de caracteres a formatear.
#' @param style Estilo de formato. Opciones: "Title", "UPPER", "lower", etc.
#'
#' @return Vector de caracteres formateado.
#'
#' @examples
#' makeup_chr("hello world", "Title")
#' #> [1] "Hello World"
#'
#' @export
makeup_chr <- function(x, style = "Title") {
  if (!is.character(x)) {
    x <- as.character(x)
  }
  
  switch(
    style,
    "Title" = {
      # Convert to title case using base R
      s <- strsplit(x, "\\s+")
      s <- lapply(s, function(words) {
        paste(toupper(substring(words, 1, 1)), 
              tolower(substring(words, 2)), 
              sep = "", collapse = " ")
      })
      unlist(s)
    },
    "UPPER" = toupper(x),
    "lower" = tolower(x),
    "sentence" = {
      # Sentence case: first letter uppercase, rest lowercase
      paste0(toupper(substring(x, 1, 1)), 
             tolower(substring(x, 2)))
    },
    x
  )
}

#' Format numbers
#'
#' Formatea números según un patrón especificado. El patrón indica el formato
#' deseado usando ejemplos como "45.343,00" donde el punto es separador de
#' miles y la coma es separador decimal.
#'
#' @param x Vector numérico a formatear.
#' @param pattern Patrón de formato (ej: "45.343,00" indica separador de miles
#'   "." y decimal "," con 2 decimales).
#'
#' @return Vector de caracteres con números formateados.
#'
#' @examples
#' makeup(45343.5, "45.343,00")
#' #> [1] "45.343,50"
#'
#' @export
makeup <- function(x, pattern = "45.343,00") {
  if (!is.numeric(x)) {
    x <- as.numeric(x)
  }
  
  # Parse pattern to extract format info
  # Pattern like "45.343,00" tells us:
  # - thousands separator: "."
  # - decimal separator: ","
  # - decimal places: count digits after comma
  
  # Extract decimal places from pattern
  decimal_places <- 0
  if (grepl(",", pattern)) {
    decimal_part <- sub(".*,", "", pattern)
    decimal_places <- nchar(decimal_part)
  }
  
  # Determine separators
  has_thousands_sep <- grepl("\\.", pattern)
  decimal_sep <- if (grepl(",", pattern)) "," else "."
  
  # Format numbers
  result <- character(length(x))
  for (i in seq_along(x)) {
    if (is.na(x[i])) {
      result[i] <- NA_character_
      next
    }
    
    # Round to specified decimal places
    rounded <- round(x[i], digits = decimal_places)
    
    # Format with separators
    if (decimal_places > 0) {
      # Split integer and decimal parts
      int_part <- floor(abs(rounded))
      dec_part <- round((abs(rounded) - int_part) * 10^decimal_places)
      
      # Ensure decimal part doesn't exceed max (e.g., if rounding causes overflow)
      if (dec_part >= 10^decimal_places) {
        dec_part <- dec_part - 10^decimal_places
        int_part <- int_part + 1
      }
      
      # Add thousands separator to integer part
      if (has_thousands_sep && int_part >= 1000) {
        int_str <- format(int_part, big.mark = ".", 
                         scientific = FALSE, trim = TRUE)
      } else {
        int_str <- as.character(int_part)
      }
      
      # Format decimal part with leading zeros if needed
      dec_str <- sprintf(paste0("%0", decimal_places, "d"), dec_part)
      
      # Combine with sign
      sign <- if (rounded < 0) "-" else ""
      result[i] <- paste0(sign, int_str, decimal_sep, dec_str)
    } else {
      # No decimal places
      if (has_thousands_sep && abs(rounded) >= 1000) {
        result[i] <- format(rounded, big.mark = ".", 
                           scientific = FALSE, trim = TRUE)
      } else {
        result[i] <- as.character(rounded)
      }
    }
  }
  
  result
}

#' Format dates
#'
#' Formatea fechas según un patrón y locale especificados.
#'
#' @param x Vector de fechas (Date o POSIXt).
#' @param pattern Patrón de formato de fecha (ej: "diciembre 12 de 2030").
#' @param locale Locale para el formato (ej: "es" para español).
#'
#' @return Vector de caracteres con fechas formateadas.
#'
#' @examples
#' makeup_dat(as.Date("2030-12-12"), "diciembre 12 de 2030", locale = "es")
#' #> [1] "diciembre 12 de 2030"
#'
#' @export
makeup_dat <- function(x, pattern = "diciembre 12 de 2030", locale = "es") {
  if (!inherits(x, c("Date", "POSIXt"))) {
    stop("x must be a Date or POSIXt object")
  }
  
  # Parse pattern to determine format
  # Common patterns:
  # - "diciembre 12 de 2030" -> "%B %d de %Y" (Spanish)
  # - "12 de diciembre de 2030" -> "%d de %B de %Y" (Spanish)
  # - "12/12/2030" -> "%d/%m/%Y"
  
  # Detect format from pattern
  if (grepl("de [0-9]{4}$", pattern)) {
    # Pattern like "diciembre 12 de 2030" or "12 de diciembre de 2030"
    if (grepl("^[a-zA-Z]+", pattern)) {
      # Month name first: "diciembre 12 de 2030"
      format_str <- "%d de %B de %Y"
      # Actually, let's check the pattern more carefully
      if (grepl("^[a-zA-Z]+ [0-9]+ de", pattern)) {
        format_str <- "%B %d de %Y"
      } else {
        format_str <- "%d de %B de %Y"
      }
    } else {
      format_str <- "%d de %B de %Y"
    }
  } else if (grepl("/", pattern)) {
    # Pattern like "12/12/2030"
    format_str <- "%d/%m/%Y"
  } else {
    # Default format
    format_str <- "%Y-%m-%d"
  }
  
  # Set locale for month names
  if (locale == "es") {
    # Spanish month names
    old_locale <- Sys.getlocale("LC_TIME")
    on.exit(Sys.setlocale("LC_TIME", old_locale), add = TRUE)
    
    # Try to set Spanish locale
    tryCatch({
      Sys.setlocale("LC_TIME", "es_ES.UTF-8")
    }, warning = function(w) {
      tryCatch({
        Sys.setlocale("LC_TIME", "Spanish")
      }, warning = function(w2) {
        # If locale setting fails, we'll use a manual mapping
      })
    })
    
    # Manual Spanish month names mapping if locale setting fails
    result <- character(length(x))
    for (i in seq_along(x)) {
      if (is.na(x[i])) {
        result[i] <- NA_character_
        next
      }
      
      # Format with locale
      formatted <- format(x[i], format = format_str)
      
      # If locale didn't work, manually replace English months with Spanish
      if (grepl("January|February|March|April|May|June|July|August|September|October|November|December", formatted)) {
        month_map <- c(
          "January" = "enero", "February" = "febrero", 
          "March" = "marzo", "April" = "abril",
          "May" = "mayo", "June" = "junio",
          "July" = "julio", "August" = "agosto",
          "September" = "septiembre", "October" = "octubre",
          "November" = "noviembre", "December" = "diciembre"
        )
        for (eng_month in names(month_map)) {
          formatted <- gsub(eng_month, month_map[eng_month], formatted)
        }
      }
      
      result[i] <- formatted
    }
    
    return(result)
  }
  
  # For other locales, use standard formatting
  format(x, format = format_str)
}


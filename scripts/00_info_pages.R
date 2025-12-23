library(sibdata)
library(tictoc)

# ============================================================================
# FUNCIÓN: Encontrar la raíz del proyecto buscando DESCRIPTION
# ============================================================================
find_project_root <- function(start_path = NULL) {
  # Si no se proporciona ruta inicial, usar la del script actual
  if (is.null(start_path)) {
    # Intentar obtener la ruta del script desde diferentes métodos
    script_path <- NULL
    
    # Método 1: RStudio
    if (requireNamespace("rstudioapi", quietly = TRUE)) {
      tryCatch({
        script_path <- rstudioapi::getActiveDocumentContext()$path
        if (script_path != "") {
          start_path <- dirname(script_path)
        }
      }, error = function(e) NULL)
    }
    
    # Método 2: sys.frame (cuando se ejecuta con source())
    if (is.null(start_path)) {
      tryCatch({
        script_path <- normalizePath(sys.frame(1)$ofile)
        start_path <- dirname(script_path)
      }, error = function(e) NULL)
    }
    
    # Método 3: Fallback al directorio actual
    if (is.null(start_path)) {
      start_path <- getwd()
    }
  }
  
  # Normalizar la ruta
  start_path <- normalizePath(start_path, mustWork = FALSE)
  current_path <- start_path
  
  # Buscar hacia arriba hasta encontrar DESCRIPTION
  max_depth <- 10  # Evitar bucles infinitos
  depth <- 0
  
  while (depth < max_depth) {
    desc_file <- file.path(current_path, "DESCRIPTION")
    
    # Verificar si existe DESCRIPTION y contiene "Package:"
    if (file.exists(desc_file)) {
      first_line <- readLines(desc_file, n = 1, warn = FALSE)
      if (!is.null(first_line) && grepl("^Package:", first_line)) {
        return(normalizePath(current_path))
      }
    }
    
    # Subir un nivel
    parent_path <- dirname(current_path)
    
    # Si llegamos a la raíz del sistema, detener
    if (parent_path == current_path) {
      break
    }
    
    current_path <- parent_path
    depth <- depth + 1
  }
  
  # Si no se encuentra, intentar usar here::here()
  if (requireNamespace("here", quietly = TRUE)) {
    tryCatch({
      here::dr_here()
      return(here::here())
    }, error = function(e) NULL)
  }
  
  # Último recurso: usar el directorio actual
  warning(
    "No se encontró el archivo DESCRIPTION. ",
    "Usando directorio actual: ", getwd()
  )
  return(getwd())
}

# ============================================================================
# ENCONTRAR RUTA BASE DEL PROYECTO
# ============================================================================
project_root <- find_project_root()
message("📁 Raíz del proyecto: ", project_root)

# ============================================================================
# BUSCAR BASE DE DATOS
# ============================================================================
# Buscar en múltiples ubicaciones posibles
db_paths <- c(
  file.path(project_root, "inst/db/sibdata.sqlite"),
  file.path(project_root, "db/sibdata.sqlite"),
  file.path(project_root, "sibdata.sqlite"),
  sys_file_sibdata("db/sibdata.sqlite")  # Fallback al paquete instalado
)

db_path <- NULL
for (path in db_paths) {
  if (file.exists(path)) {
    db_path <- normalizePath(path)
    break
  }
}

if (is.null(db_path)) {
  stop(
    "❌ No se encontró la base de datos.\n",
    "Buscó en las siguientes ubicaciones:\n",
    paste("  -", db_paths, collapse = "\n"),
    "\n\nPor favor, asegúrate de que la base de datos existe ",
    "en una de estas ubicaciones."
  )
}

message("🗄️  Base de datos encontrada: ", db_path)

# ============================================================================
# PREPARAR DIRECTORIO DE SALIDA
# ============================================================================
output_dir <- file.path(project_root, "static/data")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  message("📂 Directorio creado: ", output_dir)
}

# ============================================================================
# CONECTAR A BASE DE DATOS Y GENERAR PÁGINAS
# ============================================================================
tic()

con <- DBI::dbConnect(
  RSQLite::SQLite(),
  db_path,
  read_only = TRUE
)

message("🚀 Generando páginas de información...")
save_info_page(output_dir, con)

DBI::dbDisconnect(con)

toc()
message("✅ Proceso completado. Archivos guardados en: ", output_dir)



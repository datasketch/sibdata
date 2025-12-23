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
# PREPARAR DIRECTORIO DE SALIDA
# ============================================================================
save_path <- file.path(project_root, "static", "data")
if (!dir.exists(save_path)) {
  dir.create(save_path, recursive = TRUE)
  message("📂 Directorio creado: ", save_path)
}
message("📂 Ruta de salida: ", save_path)

# ============================================================================
# BUSCAR BASE DE DATOS
# ============================================================================
# Buscar en múltiples ubicaciones posibles
db_paths <- c(
  file.path(project_root, "inst/db/sibdata.sqlite"),
  file.path(project_root, "inst/db/sibdata.duckdb"),
  file.path(project_root, "db/sibdata.sqlite"),
  file.path(project_root, "sibdata.sqlite"),
  sys_file_sibdata("db/sibdata.sqlite"),  # Fallback al paquete instalado
  sys_file_sibdata("db/sibdata.duckdb")
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
# CONECTAR A BASE DE DATOS
# ============================================================================
# Determinar el tipo de base de datos por la extensión
is_duckdb <- grepl("\\.duckdb$", db_path, ignore.case = TRUE)

if (is_duckdb) {
  con <- DBI::dbConnect(duckdb::duckdb(), db_path, read_only = TRUE)
} else {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path, read_only = TRUE)
}

tic()
message("🚀 Generando datos mundiales...")





# Mapa mundi
ranking <- sibdata_ranking(con) |>
  filter(slug == 'ranking-biodiverdad-mundo') |>
  collect()

country_ranking <- ranking |>
  select(puesto, pais) |>
  gt_match(map_name = "world_countries") |>
  rename(lat = ..gt_lat, lon = ..gt_lon)

refs <- sibdata_referencia_estimada(con) |>
  collect()

parse_ref <- function(str){
  #str <- "56 | 92"
  r_ids <- strsplit(map_chr(str, ~ gsub(" ","",.)), "\\|")
  map_chr(r_ids, function(rs){
    rs <- as.numeric(rs)
    ref_txt <- refs |> filter(ref_id %in% rs) |>
      pull(label)
    ref_txt <- paste0(ref_txt, collapse = " ")
  })
}

ref_principal <- parse_ref(ranking$ref_id[1])


dato_relevante <- sibdata_dato_relevante(con) |> collect()

positions <- sibdata_referencias_home(con) |>
  filter(active == 1) |>
  select(-active) |>
  collect()

# positions <- tibble::tribble(
#   ~position, ~suffix,  ~position_text,
#   1, "er", "Primer lugar en diversidad de aves (111), orquídeas (41) y mariposas (13)",
#   2, "do","Segundo en variedad de anfibios(52), peces dulceacuícolas(10), palmas(54) y murciélagos (53)",
#   3, "er", "Tercero en diversidad de plantas(115)",
#   6, "to", "Sexto en mamíferos (55)",
#   7, "mo","Séptimo en reptiles(6)"
# )

extract_numbers <- function(text_vector) {
  # Extract all contents inside parentheses
  matches <- regmatches(text_vector, gregexpr("\\(([^)]+)\\)", text_vector))
  # Flatten and split by "|"
  numbers <- unlist(lapply(matches, function(x) {
    # Remove parentheses
    inside <- gsub("[()]", "", x)
    # Split by "|" and trim spaces
    unlist(strsplit(inside, "\\|")) |> trimws()
  }))
  # Convert to numeric
  as.numeric(numbers)
}

ref_ids <- extract_numbers(positions$position_text)
#ref_ids <- c(42, 41, 13, 51, 52, 10, 53, 54, 53, 55)

position_refs <- refs |>
  select(ref_id, label, -zotero) |>
  filter(ref_id %in% ref_ids)

lista_mapa <- list(
  country_ranking = country_ranking,
  ref_principal = ref_principal,
  positions = positions,
  position_refs = position_refs
)





# Tarjetas Destacado

regs <- sibdata_region_tematica(con) |> collect()

# destacados <- c(
#   "region-amazonia",
#   "tolima", "boyaca", "narino", "santander",
#   "resguardo-indigena-pialapi-pueblo-viejo",
#   "reserva-forestal-la-planada"
# )
destacados <- c(
  "region-amazonia",
  "amazonas", "caqueta", "cauca", "guainia", "guaviare", "meta",
  "putumayo", "vaupes"
)


destacados_regiones <- regs |>
  select(slug_region,
         observadas = registros_region_total,
         especies_estimadas = especies_region_estimadas,
         especies_total = especies_region_total
         ) |>
  filter(slug_region %in% destacados) |>
  sib_merge_region_label(con = con)


l <- list(
  lista_mapa = lista_mapa,
  destacados_regiones = destacados_regiones
)

# ============================================================================
# GUARDAR ARCHIVO JSON
# ============================================================================
output_file <- file.path(save_path, "home.json")
jsonlite::write_json(
  l,
  output_file,
  auto_unbox = TRUE,
  pretty = TRUE
)

message("✅ Archivo guardado: ", output_file)




toc()

# ============================================================================
# LIMPIAR
# ============================================================================
DBI::dbDisconnect(con)
message("🔌 Conexión cerrada")
message("✅ Proceso completado. Archivos guardados en: ", save_path)

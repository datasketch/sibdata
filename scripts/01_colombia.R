library(sibdata)
library(tictoc)

# ============================================================================
# FUNCIÓN: Encontrar la raíz del proyecto buscando DESCRIPTION
# ============================================================================
find_project_root <- function(start_path = NULL) {
  if (is.null(start_path)) {
    script_path <- NULL
    
    if (requireNamespace("rstudioapi", quietly = TRUE)) {
      tryCatch({
        script_path <- rstudioapi::getActiveDocumentContext()$path
        if (script_path != "") {
          start_path <- dirname(script_path)
        }
      }, error = function(e) NULL)
    }
    
    if (is.null(start_path)) {
      tryCatch({
        script_path <- normalizePath(sys.frame(1)$ofile)
        start_path <- dirname(script_path)
      }, error = function(e) NULL)
    }
    
    if (is.null(start_path)) {
      start_path <- getwd()
    }
  }
  
  start_path <- normalizePath(start_path, mustWork = FALSE)
  current_path <- start_path
  max_depth <- 10
  depth <- 0
  
  while (depth < max_depth) {
    desc_file <- file.path(current_path, "DESCRIPTION")
    
    if (file.exists(desc_file)) {
      first_line <- readLines(desc_file, n = 1, warn = FALSE)
      if (!is.null(first_line) && grepl("^Package:", first_line)) {
        return(normalizePath(current_path))
      }
    }
    
    parent_path <- dirname(current_path)
    if (parent_path == current_path) {
      break
    }
    
    current_path <- parent_path
    depth <- depth + 1
  }
  
  if (requireNamespace("here", quietly = TRUE)) {
    tryCatch({
      here::dr_here()
      return(here::here())
    }, error = function(e) NULL)
  }
  
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
db_paths <- c(
  file.path(project_root, "inst/db/sibdata.sqlite"),
  file.path(project_root, "inst/db/sibdata.duckdb"),
  file.path(project_root, "db/sibdata.sqlite"),
  file.path(project_root, "sibdata.sqlite"),
  sys_file_sibdata("db/sibdata.sqlite"),
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
is_duckdb <- grepl("\\.duckdb$", db_path, ignore.case = TRUE)

if (is_duckdb) {
  con <- DBI::dbConnect(duckdb::duckdb(), db_path, read_only = TRUE)
} else {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path, read_only = TRUE)
}

tic()
message("🚀 Generando datos para Colombia...")

region <- "colombia"
message("Región: ", region)

nav_tematica <- navigation_trees("tematica", con = con)
nav_grupo_biologico <- navigation_trees("grupo_biologico", con = con)
nav_grupo_interes <- navigation_trees("grupo_interes", con = con)
nav_territorio <- navigation_trees("territorio", region = region, con = con)

general_info <- sib_region_general(region, con)
gallery <- make_gallery(region, con)
slides <- make_region_slides(region, con)
reg_gr_bio <- region_grupo_data(region, tipo = "biologico", verbose = TRUE, con)
reg_gr_int <- region_grupo_data(region, tipo = "interes", verbose = TRUE, con)

# Temáticas
tem_list <- tematica_list_col(region, con)

# Territorio
charts_dir <- file.path(project_root, "static/charts", region)
if (!dir.exists(charts_dir)) {
  dir.create(charts_dir, recursive = TRUE)
}

subreg_tematica <- subregion_tematica(region, con)
d <- subreg_tematica |>
  collect()

dd <- d |>
  select(slug_region, especies_region_total, registros_region_total)

map_name <- "col_departments2"

deptos <- sibdata_departamento(con) |> collect()
dd <- dd |>
  left_join(deptos, by = c("slug_region" = "slug"), copy = TRUE)

dd_esp <- dd |> select(cod_dane, value = especies_region_total, label) |>
  rename(n_especies = value)
dd_reg <- dd |> select(cod_dane, value = registros_region_total, label) |>
  rename(n_registros = value)
dd_map <- left_join(dd_esp, dd_reg) |>
  select(id = cod_dane, label, n_especies, n_registros)

conmap <- gt_con()
tj <- gt_sf("col_departments", conmap) |> left_join(dd_map)

region_tipo <- "municipio"
if(region == "colombia") region_tipo <- "departamento"
territorio <- list(
  list(
    slug = "municipios",
    label = "Municipios",
    map_data = tj,
    charts = list(
      list(title = glue::glue("Especies por {region_tipo}"), path = "", layout = "title/chart"),
      list(title =  glue::glue("Registros por {region_tipo}"), path = "", layout = "title/chart")
    )
  ),
  list(
    slug = "areas-protegidas",
    label = "Áreas protegidas",
    title = "Próximamente tendrás acceso a la información de las áreas protegidas",
    charts = list()
  ),
  list(
    slug = "ecosistemas-estrategicos",
    label = "Ecosistemas estratégicos",
    title = "Próximamente tendrás acceso a la información de ecosistemas estratégicos",
    charts = list()
  )
)

##
patrocinadores <- sibdata_patrocinador(con)
patrocinador <- sibdata_region_patrocinador(con) |>
  filter(slug_region == region)
patrocinador <- patrocinador |>
  left_join(patrocinadores, by = c("slug_patrocinador" = "slug")) |>
  collect()

publicadores <- sibdata_region_publicador(con) |>
  filter(slug_region == region) |>
  left_join(sibdata_publicador(con) |> select(-especies, -registros),
            by = c("slug_publicador" = "slug")) |>
  collect()

publicadores_tipo <- publicadores |>
  select(tipo_organizacion, registros) |>
  mutate(tipo_organizacion = ifelse(is.na(tipo_organizacion), "No definido", tipo_organizacion)) |>
  summarise(n_tipo = n(),
            n_tipo_obs = sum(registros),
            .by = tipo_organizacion) |>
  mutate(pct_tipo = n_tipo/sum(n_tipo),
         pct_tipo_obs = n_tipo_obs/sum(n_tipo_obs))

publicadores_list <- publicadores |>
  select(slug_publicador, registros = registros, especies = especies,
         label, pais_publicacion,
         url_logo, url_socio) |>
  arrange(desc(registros))

publicadores <- list(
  publicadores_tipo = publicadores_tipo,
  publicadores_list = publicadores_list
)

municipios_lista <- subreg_tematica |>
  select(slug =slug_region, label) |>
  collect()
departamentos_lista <- tribble(
  ~slug, ~label,
  "boyaca", "Boyacá",
  "narino", "Nariño",
  "santander", "Santander",
  "tolima", "Tolima"
)
departamentos_lista <- deptos |> select(slug, label)

l <- list(
  general_info = general_info,
  nav_tematica = nav_tematica,
  nav_grupo_biologico = nav_grupo_biologico,
  nav_grupo_interes = nav_grupo_interes,
  nav_territorio = nav_territorio,
  gallery = gallery,
  slides = slides,
  tematica = tem_list,
  grupos_biologicos = reg_gr_bio,
  grupos_interes = reg_gr_int,
  territorio = territorio,
  patrocinador = patrocinador,
  publicadores = publicadores,
  municipios_lista = list(),
  departamentos_lista = departamentos_lista
)

region_dir <- file.path(save_path, region)
if (!dir.exists(region_dir)) {
  dir.create(region_dir, recursive = TRUE)
}

jsonlite::write_json(
  l,
  file.path(region_dir, paste0(region, ".json")),
  auto_unbox = TRUE,
  pretty = TRUE
)

sf::write_sf(
  tj,
  file.path(region_dir, paste0(region, ".geojson")),
  delete_dsn = TRUE
)

toc()

# ============================================================================
# LIMPIAR
# ============================================================================
DBI::dbDisconnect(con)
message("🔌 Conexión cerrada")
message("✅ Proceso completado. Archivos guardados en: ", region_dir)

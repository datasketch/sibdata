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
  file.path(project_root, "inst/db/sibdata.duckdb"),
  file.path(project_root, "inst/db/sibdata.sqlite"),
  file.path(project_root, "db/sibdata.duckdb"),
  file.path(project_root, "db/sibdata.sqlite"),
  sys_file_sibdata("db/sibdata.duckdb"),
  sys_file_sibdata("db/sibdata.sqlite")
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
message("🚀 Generando datos para municipios...")

#av_regions <- sib_available_regions(subtipo = c("Municipio"))
av_regions2 <- sib_available_regions(subtipo = c("Municipio"),
                                    departamento = "tolima", con = con)

av_regions1 <- sib_available_regions(subtipo = c("Municipio"),
                                    departamento = "narino", con = con)

av_regions3 <- sib_available_regions(subtipo = c("Municipio"),
                                     departamento = "santander", con = con)

av_regions4 <- sib_available_regions(subtipo = c("Municipio"),
                                     departamento = "boyaca", con = con)
# Amazonas
av_regions5 <- sib_available_regions(subtipo = c("Municipio"),
                                     departamento = "amazonas", con = con)
av_regions6 <- sib_available_regions(subtipo = c("Municipio"),
                                     departamento = "caqueta", con = con)
av_regions7 <- sib_available_regions(subtipo = c("Municipio"),
                                     departamento = "guaviare", con = con)
av_regions8 <- sib_available_regions(subtipo = c("Municipio"),
                                     departamento = "guainia", con = con)
av_regions9 <- sib_available_regions(subtipo = c("Municipio"),
                                     departamento = "putumayo", con = con)
av_regions10 <- sib_available_regions(subtipo = c("Municipio"),
                                     departamento = "vaupes", con = con)
av_regions11 <- sib_available_regions(subtipo = c("Municipio"),
                                    departamento = "cauca", con = con)
av_regions12 <- sib_available_regions(subtipo = c("Municipio"),
                                      departamento = "meta", con = con)



av_regions <- c(
  # "reserva-forestal-la-planada",
  # "resguardo-indigena-pialapi-pueblo-viejo",
  av_regions1,
  av_regions2,
  av_regions3,
  av_regions4,
  av_regions5,
  av_regions6,
  av_regions7,
  av_regions8,
  av_regions9,
  av_regions10,
  av_regions11,
  av_regions12
)
# av_regions <- av_regions12



n <- length(av_regions)
i <<- 1

library(tictoc)

tic()

av_regions <- av_regions

map(av_regions, function(region){

  #region <- "puerto-lopez"
  parent <- sib_parent_region(region, con = con)
  message("\n################################  ", parent)
  message("................................. ",region, paste0("(",i," de ",n,")"))
  i <<- i + 1
  # region <- "tunja"
  # region <- "boyaca-boy"
  # region <- "ibague"
  # region <- "chiquinquira"
  # region <- "alpujarra"
  # region <- "alvarado"
  # region <- "reserva-forestal-la-planada"
  # region <-  "resguardo-indigena-pialapi-pueblo-viejo"
  # region <- "purificacion"

  nav_tematica <- navigation_trees("tematica", con = con)
  nav_grupo_biologico <- navigation_trees("grupo_biologico", con = con)
  nav_grupo_interes <- navigation_trees("grupo_interes", con = con)

  # No hay territorio
  nav_territorio <- list()

  general_info <- sib_region_general(region, con = con)

  # No hay galería
  gallery <- list()


  slides <- make_region_slides2(region, con = con)

  parent <- sib_parent_region(region, con = con)

  general_info$parent <- parent
  parent_depto <- parent
  general_info$parent_label <- sibdata_region(con) |>
    collect() |>
    filter(slug == parent_depto) |> pull(label)


  reg_gr_bio <- list()
  reg_gr_int <- list()

  #if(parent %in% c("narino", "tolima"))
  reg_gr_bio <- region_grupo_data(region, tipo = "biologico", verbose = TRUE, con = con)
  reg_gr_int <- region_grupo_data(region, tipo = "interes", verbose = TRUE,con = con)


  # Temáticas

  tem_list <- tematica_list(region, con = con)
  #tem_list <- NA




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




  l <- list(
    nav_tematica = nav_tematica,
    nav_grupo_biologico = nav_grupo_biologico,
    nav_grupo_interes = nav_grupo_interes,
    nav_territorio = list(),

    general_info = general_info,

    gallery = list(),
    slides = slides,
    tematica = tem_list,
    grupos_biologicos = reg_gr_bio,
    grupos_interes = reg_gr_int,

    territorio = list(),

    patrocinador = patrocinador,
    publicadores = publicadores,
    municipios_lista = list()
  )
  parent_dir <- file.path(save_path, parent)
  if (!dir.exists(parent_dir)) {
    dir.create(parent_dir, recursive = TRUE)
  }
  
  jsonlite::write_json(
    l,
    file.path(parent_dir, paste0(region, ".json")),
    auto_unbox = TRUE,
    pretty = TRUE
  )


})


toc()

# ============================================================================
# LIMPIAR
# ============================================================================
DBI::dbDisconnect(con)
message("🔌 Conexión cerrada")
message("✅ Proceso completado. Archivos guardados en: ", save_path)





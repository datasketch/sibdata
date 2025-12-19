#' Save info pages to JSON files
#'
#' Genera archivos JSON con información de páginas informativas (publicadores,
#' preguntas frecuentes, glosario, tooltips).
#'
#' @param path Ruta del directorio donde se guardarán los archivos JSON.
#' @param con Conexión a la base de datos.
#'
#' @return Invisible, guarda archivos JSON en el directorio especificado.
#'
#' @export
save_info_page <- function(path, con) {
  # Copy icons
  copy_icons("static")

  l <- info_pages(con)

  purrr::map2(l, names(l), function(page, nm) {
    jsonlite::write_json(
      page,
      file.path(path, paste0(nm, ".json")),
      auto_unbox = TRUE,
      pretty = TRUE
    )
  })
}

#' Generate info pages data
#'
#' Genera una lista con datos de páginas informativas.
#'
#' @param con Conexión a la base de datos.
#'
#' @return Lista con datos de publicadores, preguntas frecuentes, glosario y
#'   tooltips.
#'
#' @keywords internal
info_pages <- function(con) {
  list(
    publicador = info_publicador(con),
    preg_frecuentes = sibdata_preg_frecuentes(con) |> collect(),
    glosario = sibdata_glosario(con) |> collect(),
    tooltips = sibdata_tematica(con) |>
      select(slug, tooltip) |>
      collect()
  )
}

#' Generate publicador information
#'
#' Genera información sobre publicadores de datos de biodiversidad, incluyendo
#' estadísticas por región y filtros de navegación.
#'
#' @param con Conexión a la base de datos.
#'
#' @return Lista con información de publicadores por región y filtros de
#'   navegación.
#'
#' @export
info_publicador <- function(con) {
  deptos <- sibdata_departamento(con) |> pull(slug)

  which_regs <- c("colombia",
                  deptos,
                  "region-amazonia",
                  "reserva-forestal-la-planada", "resguardo-indigena-pialapi-pueblo-viejo")
  # which_regs <- sibdata_region(con) |> collect() |>
  #   filter(parent %in% which_regs) |>
  #   pull(slug)


  pubs <- sibdata_publicador(con) |> collect() |>
    rename(slug_publicador = slug,
           especies_publicador = especies,
           registros_publicador = registros)

  pub_reg <- sibdata_region_publicador(con) |>
    collect() |>
    filter(slug_region %in% which_regs) |>
    sib_merge_region_label("slug_region", con = con) |>
    left_join(pubs)

  keys <- pub_reg |>
    group_by(slug_region) |>
    group_keys() |>
    pull(slug_region)
  pub_reg_list <- pub_reg |>
    group_split(slug_region)
  names(pub_reg_list) <- keys

  pub_reg_list <- purrr::map(pub_reg_list, function(r) {
    # r <- pub_reg_list[[18]]
    list(
      publicadores = r |>
        select(slug_publicador, label, registros, especies, tipo_publicador,
               tipo_organizacion, tipo_publicador, pais_publicacion,
               url_logo, url_socio),
      stats = list(
        total_publicadores = nrow(r),
        nacionales = r |> filter(tipo_publicador == "Nacional") |> nrow(),
        internacionales = r |> filter(tipo_publicador == "Internacional") |> nrow(),
        tipo_organizacion = r |> count(tipo_organizacion),
        registros_tipo_organizacion = r |>
          select(tipo_organizacion, registros) |>
          group_by(tipo_organizacion) |>
          summarise(registros = sum(registros))
      )
    )
  })

  region_nav <- list(
    nacional = "colombia",
    Departamentos = deptos,
    `Áreas protegidas` = "reserva-forestal-la-planada",
    `Territorios indígenas` = "resguardo-indigena-pialapi-pueblo-viejo",
    "Regiones naturales" =  "region-amazonia"
  )

  list(
    region_publicador = pub_reg_list,
    filters = list(
      region = region_nav,
      tipo_organizacion = pubs |> select(tipo_organizacion) |> distinct() |>
        pull() |> sort(),
      pais_publicacion = pubs |> select(pais_publicacion) |> distinct() |>
        pull() |> sort()
    )
  )
}



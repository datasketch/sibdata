library(sibdata)
#library(lfltmagic)
library(geodato)
library(tictoc)

devtools::load_all()

#here::i_am("static")

here::dr_here()
# here::set_here()
# here::dr_here()
# setwd("../")
# here::dr_here()
# tic()

#
# con <- DBI::dbConnect(duckdb::duckdb(), "../inst/db/sibdata.duckdb",
#                       read_only = TRUE)

con <- DBI::dbConnect(RSQLite::SQLite(), sys_file("db/sibdata.sqlite"),
                      read_only = TRUE)


# Mapa mundi

dato_home <- sibdata_dato_relevante(con) |>
  filter(slug_region == "home") |>
  collect()
ranking <- sibdata_ranking(con) |> collect()

refs <- sibdata_referencia_estimada(con) |>
  collect()


common_categories <- intersect(dato_home$slug_grupo, ranking$slug)

dato_home <- dato_home |>
  filter(slug_grupo %in% common_categories)

dato_home_by_group <- dato_home |>
  group_by(slug_grupo) |>
  group_split()
names(dato_home_by_group) <- dato_home |>
  group_by(slug_grupo) |>
  group_keys() |> pull(1)

ranking <- ranking |>
  filter(slug %in% common_categories) |>
  group_by(slug) |>
  arrange(puesto, .by_group = TRUE)
unique(ranking$slug)

ranking_by_group <- ranking |>
  group_split()
names(ranking_by_group) <- ranking |> group_keys() |> pull(1)

all(names(dato_home_by_group) == names(ranking_by_group))

lista_mapa <- map2(dato_home_by_group, ranking_by_group, function(x,y){
  #x <- dato_home_by_group[[1]]
  #y <- ranking_by_group[[1]]

  parse_ref <- function(str){
    #str <- "3| 52"
    r_ids <- strsplit(map_chr(str, ~ gsub(" ","",.)), "\\|")
    map_chr(r_ids, function(rs){
      rs <- as.numeric(rs)
      ref_txt <- refs |> filter(ref_id %in% rs) |>
        pull(label)
      ref_txt <- paste0(ref_txt, collapse = " ")
    })
  }

  str <- x$ref_id


  descriptions <- x |>
    select(descripcion, ref_id) |>
    mutate(refs = parse_ref(ref_id)) |>
    select(-ref_id)

  country_ranking <- y |>
    select(puesto, pais) |>
    gd_match(map_name = "world_countries", col = "pais")


  list(
    slug = x$slug_grupo[1],
    title = x$titulo[1],
    descriptions = descriptions,
    ranking = country_ranking,
    ranking_refs = parse_ref(unique(y$ref_id))
  )
})
lista_mapa <- unname(lista_mapa)
# Tarjetas Destacado

regs <- sibdata_region_tematica(con) |> collect()

destacados <- c(
  "tolima", "boyaca", "narino", "santander",
  "resguardo-indigena-pialapi-pueblo-viejo",
  "reserva-forestal-la-planada"
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
#dir.create(file.path("static/data/home"))
jsonlite::write_json(l, paste0("static/data/home.json"),
                     auto_unbox = TRUE, pretty =TRUE)




toc()





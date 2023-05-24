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
ranking <- sibdata_ranking(con) |>
  filter(slug == 'ranking-biodiverdad-mundo') |>
  collect()

country_ranking <- ranking |>
  select(puesto, pais) |>
  gd_match(map_name = "world_countries", col = "pais")

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



positions <- tibble::tribble(
  ~position,  ~position_text,
  1, "Primer lugar en diversidad de aves(42), orquídeas (41) y mariposas (13)",
  2, "Segundo en plantas (51), anfibios(52), peces dulceacuícolas(10), reptiles(53), palmas(54) y murciélagos (53)",
  3, "quinto en mamíferos (55)"
)

ref_ids <- c(42, 41, 13, 51, 52, 10, 53, 54, 53, 55)
position_refs <- refs |>
  select(ref_id, label, zotero) |>
  filter(ref_id %in% ref_ids)

lista_mapa <- list(
  country_ranking = country_ranking,
  ref_principal = ref_principal,
  positions = positions,
  position_refs = position_refs
)





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





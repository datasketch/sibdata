library(sibdata)
library(tictoc)

devtools::load_all()

#here::i_am("static")

here::dr_here()
save_path <- here::here("static", "data")
message("Save path: ", save_path)

# con <- DBI::dbConnect(duckdb::duckdb(), "../inst/db/sibdata.duckdb",
#                       read_only = TRUE)

con <- DBI::dbConnect(RSQLite::SQLite(), sys_file_sibdata("db/sibdata.sqlite"),
                      read_only = TRUE)





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
#dir.create(file.path("static/data/home"))
jsonlite::write_json(l, paste0("static/data/home.json"),
                     auto_unbox = TRUE, pretty =TRUE)




toc()





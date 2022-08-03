

files <- list.files("data-raw/db-cifras-sib", full.names = TRUE)

# All sheets 
table_names <- gsub(".tsv", "",basename(files))
all(tables$table %in% table_names)
all(table_names %in% tables$table)
mop::which_not_in(table_names, tables$table)

#dfs <- map(x, read_tsv)
#names(dfs) <- basename(x)

for(i in files){
  message(i)
  d <- read_delim(i)
  assign(basename(tools::file_path_sans_ext(i)),d)
}

## 

dm0 <- dm(grupo_biologico, # pk: slug
          tematica, # pk: slug
          especie_meta, # pk: slug
          especie, # pk:slug
          estimada, # pk: slug_grupo -> rename to slug_grupo_biologico
          publicador, #pk: url_socio
          region, # no pk
          region_grupo_biologico, # no pk
          region_tematica, # pk: slug_region
          especie_grupo_biologico, # no pk
          especie_region, # no pk
          grupo_interes_conservacion,  # pk: slug
          especie_grupo_interes_conservacion, # no pk
          region_grupo_interes_conservacion, # no pk
          region_publicador, # no pk
          especie_tematica, # no pk
          departamento, # pk: slug
          municipio, # pk: slug
          dato_relevante,
)

#dm_enum_pk_candidates(dm = dm, table = region_tematica)

dm_pk <-   dm0 %>%
  dm_add_pk(table = grupo_biologico, columns = slug) |> 
  dm_add_pk(table = region_grupo_biologico, columns = c(slug_region, slug_grupo_biologico)) |> 
  dm_add_pk(table = especie, columns = slug) |> 
  dm_add_pk(table = tematica, columns = slug) |> 
  dm_add_pk(table = region_tematica, columns = slug_region) |> 
  dm_add_pk(table = region, columns = slug) |> 
  dm_add_pk(table = especie_meta, columns = slug) |> 
  dm_add_pk(table = estimada, columns = slug_grupo) |> 
  dm_add_pk(table = publicador, columns = slug) |> 
  dm_add_pk(table = departamento, columns = slug) |> 
  dm_add_pk(table = municipio, columns = slug) |> 
  dm_add_pk(table = grupo_interes_conservacion, columns = slug)

#dm_enum_fk_candidates(dm = dm0, table = grupo_biologico, ref_table = especie_meta)

dm_fk <- dm_pk |> 
  dm_add_fk(table = especie_meta, columns = slug, ref_table = especie) |> 
  dm_add_fk(table = estimada, columns = slug_grupo, ref_table = grupo_biologico) |> 
  dm_add_fk(table = especie_region, columns = slug_especie, ref_table = especie) |> 
  dm_add_fk(table = especie_region, columns = slug_region, ref_table = region) |> 
  dm_add_fk(table = region_grupo_biologico, columns = slug_region, ref_table = region) |> 
  dm_add_fk(table = region_grupo_biologico, columns = slug_grupo_biologico, ref_table = grupo_biologico) |> 
  dm_add_fk(table = especie_grupo_biologico, columns = slug_especie, ref_table = especie) |> 
  dm_add_fk(table = especie_grupo_biologico, columns = slug_grupo_biologico, ref_table = grupo_biologico) |> 
  dm_add_fk(table = region_tematica, columns = slug_region, ref_table = region) |> 
  dm_add_fk(table = especie_tematica, columns = slug_especie, ref_table = especie) |> 
  dm_add_fk(table = especie_tematica, columns = slug_region, ref_table = region) |> 
  dm_add_fk(table = especie_tematica, columns = slug_tematica, ref_table = tematica) |> 
  dm_add_fk(table = region_publicador, columns = slug_publicador, ref_table = publicador) |> 
  dm_add_fk(table = region_publicador, columns = slug_region, ref_table = region) |> 
  dm_add_fk(table = departamento, columns = slug, ref_table = region) |> 
  dm_add_fk(table = municipio, columns = slug, ref_table = region) |> 
  dm_add_fk(table = dato_relevante, columns = slug_region, ref_table = region) |> 
  dm_add_fk(table = especie_grupo_interes_conservacion, columns = slug_especie, ref_table = especie) |> 
  dm_add_fk(table = especie_grupo_interes_conservacion, columns = slug_grupo_interes_conservacion, ref_table = grupo_interes_conservacion) |>
  dm_add_fk(table = region_grupo_interes_conservacion, columns = slug_region, ref_table = region) |> 
  dm_add_fk(table = region_grupo_interes_conservacion, columns = slug_grupo_interes_conservacion, ref_table = grupo_interes_conservacion) |> 
  dm_add_fk(table = region, columns = parent, ref_table = region) |> 
  dm_add_fk(table = tematica, columns = parent, ref_table = tematica) |> 
  dm_add_fk(table = grupo_biologico, columns = parent, ref_table = grupo_biologico) |> 
  dm_add_fk(table = grupo_interes_conservacion, columns = parent, ref_table = grupo_interes_conservacion)

diag <- dm_fk |> dm_draw()
diag

mop::which_not_in(table_names, names(dm_fk))


dm_fk |> dm_examine_constraints()


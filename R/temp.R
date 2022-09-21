#
#
# region_tematica_long <- function(region){
#
#   d1 <- region_tematica(region) |>
#     dplyr::select(-fecha_corte)
#
#   d2 <- d1 |>
#     tidyr::pivot_longer(-starts_with("slug"),
#                         names_to = c("indicador"),
#                         values_to = "count")
#
#   inds <- sib_tables("ind_meta") #|>
#   #filter(indicador %in% names(d))
#
#   d3 <- dplyr::left_join(d2, inds)
#   d4 <- d3 |>
#     dplyr::select_if(~length(unique(.))!= 1)
#   #|>
#   #  dplyr::select(-indicador)
#   d5 <- d4 %>% dplyr::relocate(count, .after = last_col())
#
#   cats <- c("cr","en","vu", "i","i_ii","ii","iii",
#             "exoticas", "invasoras", "exoticas_riesgo_invasion")
#
#   d6 <- d5 |>
#     dplyr::filter(cobertura == "total")|>
#     #dplyr::filter(cobertura == "total" || categorias_tematicas %in% cats)|>
#     #dplyr::filter(tipo == "especies") |>
#     dplyr::filter(!is.na(slug_tematica))
#   #|>
#   #  dplyr::select(indicador, count, label, slug_tematica, tipo, tematica, categorias_tematicas)
#   d6
#   # d7 <- d6 |>
#   #   select(indicador, tipo, count) |>
#   #   #filter(!is.na(count)) |> # OJOOOOO EXÓTICAS TIENE MUCHOS NA
#   #   pivot_wider(names_from = "tipo", values_from = "count")
#   # d7
# }
#
#

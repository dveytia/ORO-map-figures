tmp <- mitAdaptPubs |> 
  filter(stringr::str_detect(affiliation, pattern = paste(stringr::str_to_upper(countries_ls$name_en), collapse = "|")) == TRUE)


tmp2 = mitPubs |> 
  filter(stringr::str_detect(affiliation, pattern = "Mali") == TRUE)


oroAffiliations |> 
  filter(analysis_id == 60791)

tmp = mitPubs |> 
  filter(analysis_id == 293101) ; tmp$affiliation

tmp2 = test |> 
  filter(analysis_id == 60791) ; tmp$affiliation

tmp2 = NA_country_ada2 |> 
  filter(stringr::str_detect(affiliation, pattern = "Korea \\(the Republic of\\)") == TRUE)


test <- sf::st_read(here::here("data", "external", "eez_shp", "eez_boundaries_v12.shp"))

test2 <- raster::shapefile(here::here("data", "external", "eez_shp", "eez_boundaries_v12.shp"))
raster::plot(test)
raster::plot(test2)





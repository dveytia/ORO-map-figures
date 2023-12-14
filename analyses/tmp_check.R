tmp <- mitAdaptPubs |> 
  filter(stringr::str_detect(affiliation, pattern = paste(stringr::str_to_upper(countries_ls$name_en), collapse = "|")) == TRUE)


tmp2 = oroAffiliations |> 
  filter(stringr::str_detect(affiliation, pattern = "Mali") == TRUE)


oroAffiliations |> 
  filter(analysis_id == 60791)

tmp = mitAdaptPubs |> 
  filter(analysis_id == 60791) ; tmp$affiliation

tmp2 = test |> 
  filter(analysis_id == 60791) ; tmp$affiliation

tmp2 = NA_country_ada2 |> 
  filter(stringr::str_detect(affiliation, pattern = "Korea \\(the Republic of\\)") == TRUE)

tmp <- mitAdaptPubs |> 
  filter(stringr::str_detect(affiliation, pattern = paste(stringr::str_to_upper(countries_ls$name_en), collapse = "|")) == TRUE)


tmp2 = oroAffiliations |> 
  filter(stringr::str_detect(affiliation, pattern = ", SINGAPORE") == TRUE)


oroAffiliations_country1stA |> 
  filter(analysis_id == 228405)

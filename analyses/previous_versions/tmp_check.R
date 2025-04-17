tmp <- mitAdaptPubs |> 
  filter(stringr::str_detect(affiliation, pattern = paste(stringr::str_to_upper(countries_ls$name_en), collapse = "|")) == TRUE)


tmp2 = mitPubs |> 
  filter(stringr::str_detect(affiliation, pattern = "Mali") == TRUE)


oroAffiliations |> 
  filter(analysis_id == 60791)

tmp = mitPubs |> 
  filter(analysis_id == 293101) ; tmp$affiliation



tmp2 = NA_country_ada2 |> 
  filter(stringr::str_detect(affiliation, pattern = "Korea \\(the Republic of\\)") == TRUE)


test <- sf::st_read(here::here("data", "external", "eez_shp", "eez_boundaries_v12.shp"))

test2 <- raster::shapefile(here::here("data", "external", "eez_shp", "eez_boundaries_v12.shp"))
raster::plot(test)
raster::plot(test2)

id = 207370

testX <- uniquerefs |> 
  collect() |> 
  filter(analysis_id == id) ; testX$doi ; testX$title ; testX$keywords # testX$author ; testX$affiliation ; testX$abstract

tmp_x <- tbl(dbcon, "geoparsed-text_shp_df_matches") |> 
  filter(analysis_id == id) |> 
  collect()

temp_all <- tbl(dbcon, "geoparsed_text") |>  collect()
length(unique(temp_all$analysis_id))

temp <- tbl(dbcon, "geoparsed_text") |>  
  filter(analysis_id == id) |> 
  collect()

text_i <- geoparsedText |> 
  # dplyr::select(word, place_name, analysis_id) |> 
  filter(word == "Douglas-fir") |> 
  collect() ; text_i$title

library(maps)
cities <- as.data.frame(world.cities) 
city_pattern <-  str_c(cities$name, collapse = '|')
country_pattern <-  str_c(unique(cities$country.etc), collapse = '|')

word_in_capital_letters_TRUE <- tbl(dbcon, "geoparsed_text") |>  
  dplyr::select(analysis_id, word, country_predicted, place_name) |> 
  collect() |> 
  # Filter row with a matching pattern in CAPITAL letters
  filter(stringr::str_detect(word, pattern = "^[[:upper:][:space:]]+$")) |> 
  mutate(word_title = stringr::str_to_title(word),
         # New column to identify CAPITAL letters patterns that are country names
         id_country = countrycode(sourcevar   = word,
                                  origin      = "country.name",
                                  destination = "country.name"),
         # New column to identify CAPITAL letters patterns that are city names
         city_T_F  = word_title %in% cities$name) 


capital_NA <- word_in_capital_letters_TRUE |> filter(is.na(id_country) & city_T_F == FALSE) # |> group_by(word) |> summarise(n = n())
capital_noNA <- word_in_capital_letters_TRUE |>  filter(!is.na(id_country) | city_T_F == TRUE)
  
  filter(capital, !is.na(id_country)) |> dplyr::select(analysis_id, word, country_predicted, place_name, id_country) |> distinct(word, .keep_all = T)


capital_cities <- tbl(dbcon, "geoparsed_text") |>  
  collect() |> 
  ## Filter row with a matching pattern in CAPITAL letters
  filter(stringr::str_detect(word, pattern = "^[[:upper:][:space:]]+$")) |> 
  filter(word %in% cities$country.etc)
  mutate(id_country = countrycode(sourcevar   = word,
                                  origin      = "country.name",
                                  destination = "country.name"))
  
capital_cities_NA <- capital_cities |> dplyr::select(analysis_id, word, country_predicted, place_name) |> distinct(word, .keep_all = T)
capital_cities_noNA <- capital_cities |> dplyr::select(analysis_id, word, country_predicted, place_name) |> distinct(word, .keep_all = T)
  
  
countrycode(sourcevar   = "CHE",
            origin      = "iso3c",
            destination = "country.name")

test_china <- geoP_data_empirical_cell |> 
  filter(word == "China") |> 
  left_join(grid_df_eez_land |>  dplyr::select(-LAT, -LON), by = "grid_df_id")

shp_df_matches_grid <- shp_df_matches |> 
  collect() |> 
  left_join(grid_df_eez_land |>  dplyr::select(-LAT, -LON), by = "grid_df_id") 

shp_df_matches_china <- shp_df_matches |> 
  collect() |> 
  left_join(grid_df_eez_land |>  dplyr::select(-LAT, -LON), by = "grid_df_id") |> 
  filter(shp_id == 195)
  # filter(shp_id == 195)

analysis_id_i <- geoP_data_empirical_cell_acronym_CR_200km_Q95_word_num_sea_title |> 
  filter(analysis_id == ana_id_unique[i]) |> 
  left_join(uniquerefs_kw, by = "analysis_id", copy = TRUE) |> 
  mutate(country_in_kw  = case_when(stringr::str_detect(keywords, stringr::str_to_lower(TERRITORY1)) ~ TRUE,
                                    (!stringr::str_detect(keywords, stringr::str_to_lower(TERRITORY1)) & sum(stringr::str_detect(keywords, unique(stringr::str_to_lower(oceans_seas_intersect$ocean_sea))) == TRUE, na.rm=T) > 0) ~ TRUE,
                                    TRUE ~ FALSE),         
         country_in_kwo = case_when(stringr::str_detect(keywords_other, stringr::str_to_lower(TERRITORY1)) ~ TRUE,
                                    (!stringr::str_detect(keywords_other, stringr::str_to_lower(TERRITORY1)) & sum(stringr::str_detect(keywords_other, unique(stringr::str_to_lower(oceans_seas_intersect$ocean_sea))) == TRUE, na.rm=T) > 0) ~ TRUE,
                                    TRUE ~ FALSE),
         US_state_in_kw = case_when(stringr::str_detect(keywords, paste(stringr::str_to_lower(state.name), collapse = "|")) == TRUE & TERRITORY1 == "United States" ~ TRUE, 
                                    TRUE ~ FALSE)) |>
  tidyr::replace_na(list(country_in_kw = FALSE, country_in_kwo = FALSE, US_state_in_kw = FALSE))

stringr::str_detect(keywords, unique(stringr::str_to_lower(oceans_seas_intersect$ocean_sea)))
sum(stringr::str_detect(keywords, unique(stringr::str_to_lower(oceans_seas_intersect$ocean_sea))) == TRUE, na.rm=T) > 0



# Select all row for the ith analysis_id
analysis_id_i <- geoP_data_empirical_cell_acronym_CR_200km_Q95_word_num_sea |> 
  filter(analysis_id == ana_id_unique[i]) |> 
  left_join(geopparsedText_sel, by = "analysis_id", copy = TRUE)

# Replace abbreviations by country names
if(sum(stringr::str_detect(analysis_id_i$title, c("UK|USA|US"))) > 0){
  
  abbrev_i <- unique(stringr::str_extract(analysis_id_i$title, c("UK|USA|US")))
  country_i <- countrycode::countrycode(sourcevar   = abbrev_i,
                                        origin      = "country.name",
                                        destination = "country.name")
  analysis_id_i <- analysis_id_i |> 
    mutate(title = stringr::str_replace(title, abbrev_i, country_i))
  
}

# Keep only TRUE data if country nale identified in the title, else, keep all data
analysis_id_i <- analysis_id_i |>  
  mutate(country_in_title = stringr::str_detect(title, TERRITORY1) | stringr::str_detect(title, c("UK|USA|US")))

if(sum(analysis_id_i$country_in_title == TRUE) > 0){
  keep_data <- analysis_id_i |> filter(country_in_title == TRUE) |> dplyr::select(-country_in_title, -title)
  drop_data <- rbind(drop_data, analysis_id_i |> filter(country_in_title == FALSE))
} else {keep_data <- analysis_id_i |> dplyr::select(-country_in_title, -title)}


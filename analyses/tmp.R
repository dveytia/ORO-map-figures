test_v4 <- tbl(dbcon, "geoparsed-text_shp_df_matches") |> 
  # filter(analysis_id == 35143) |>
  left_join(grid_df_eez_land, by = "grid_df_id", copy = TRUE) |>
  filter(country_id == "Norway") |> 
  distinct(analysis_id, TERRITORY1, country_id) |>
  collect()

tmp_mitig <- pred_oro_branch |> 
  left_join(pred_relevance, by = "analysis_id") |> 
  filter(0.5 <= relevance_mean) |> 
  mutate(mitigation = ifelse(0.5 <= `oro_branch.Mitigation - mean_prediction`, 1 ,0)) |>  
  filter(mitigation == 1) |>
  dplyr::select(analysis_id, mitigation) |> 
  # Select only relevant articles and get there shp_id
  left_join(shp_df_matches |> distinct(grid_df_id, analysis_id, shp_id, place), by = "analysis_id") |>
  right_join(grid_df_eez_land, by = "grid_df_id", copy = TRUE) |> 
  filter(country_id == "Japan") |> 
  distinct(analysis_id, TERRITORY1, country_id) |>
  collect()

countryConf <- tbl(dbcon, "geoparsed_text") |> 
  dplyr::select(country_conf, analysis_id, country_predicted) |>
  collect() |> 
  mutate(country = countrycode(sourcevar   = country_predicted,
                               origin      = "iso3c",
                               destination = "country.name")) 

testX <- uniquerefs |> 
  collect() |>   
  right_join(bottom_countries_papers, by = "analysis_id") |> 
  dplyr::select(analysis_id, title, author, abstract, TERRITORY1, country_id) |> 
  # filter(analysis_id %in% bottom_countries_papers$analysis_id) |> 
  arrange(analysis_id) |> 
  left_join(countryConf, by = "analysis_id") |> 
  filter(country %in% bottom_countries_papers$TERRITORY1)
  # filter(analysis_id %in% tmp_mitig$analysis_id) 

x = 33 ; id = testX$analysis_id[x] ; id ; testX$title[x] ; testX$author[x] ; testX$country_conf[x] 
# testX$abstract[x]

test_id <- tbl(dbcon, "geoparsed-text_shp_df_matches") |>
  filter(analysis_id == id) |>
  left_join(grid_df_eez_land, by = "grid_df_id", copy = TRUE) |>
  # distinct(analysis_id, TERRITORY1, country_id) |>
  # filter(country_id == "Switzerland") |> 
  collect() ; unique(test_id$TERRITORY1) ; unique(test_id$country_id) ; unique(test_id$place)

test_id <-  pred_oro_branch |> 
  left_join(pred_relevance, by = "analysis_id") |> 
  filter(0.5 <= relevance_mean) |> 
  mutate(mitigation = ifelse(0.5 <= `oro_branch.Mitigation - mean_prediction`, 1 ,0)) |>  
  filter(mitigation == 1) |>
  dplyr::select(analysis_id, mitigation) |>
  # Select only relevant articles and get there shp_id
  left_join(shp_df_matches |> distinct(grid_df_id, analysis_id, shp_id, place), by = "analysis_id") |> 
  # filter(!is.na(shp_id)) |> 
  collect() ; length(unique(test_id$analysis_id))

library(readxl)
btm_country_data <- read_excel("data/geoparsing/test_geop_results_consistency.xlsx", sheet = 5)

sum(btm_country_data$geop_true_01 == 1)
sum(btm_country_data$geop_true_01 == 0.5)
sum(btm_country_data$geop_true_01 == 0)

ggplot(btm_country_data, aes(confidence)) + facet_wrap(~ `Reason of 0`) + geom_density() + theme_bw()

btm_country_data_grp <- btm_country_data |> 
  group_by(`Reason of 0`) |> 
  summarise(n = n()) 



tmp_mit_grid <- pred_oro_branch |> 
  left_join(pred_relevance, by = "analysis_id") |> 
  filter(0.5 <= relevance_mean) |> 
  mutate(mitigation = ifelse(0.5 <= `oro_branch.Mitigation - mean_prediction`, 1 ,0)) |>  
  filter(mitigation == 1) |>
  dplyr::select(analysis_id, mitigation) |> 
  # Select only relevant articles and get there shp_id
  left_join(shp_df_matches |> distinct(grid_df_id, analysis_id, shp_id, place), by = "analysis_id") |>
  right_join(grid_df_eez_land, by = "grid_df_id", copy = TRUE) |> 
  collect()

test_id_sf <- sf::st_as_sf(tmp_mit_grid |> filter(analysis_id == 357439), coords = c("LON", "LAT"), crs = 4326)

ggplot(data = test_id_sf) +
  geom_sf(data = world_shp, fill = "grey95") +
  geom_sf(color = "red", size = 3) +
  theme_bw()

ggplot(test_id) +
  # geom_sf(data = test_id, mapping = aes(color = log(n_articles_weighted))) +
  # scale_color_viridis_c() +
  geom_sf(data = world_shp, fill = "grey95") +
  geom_sf(color = "red", size = 3) +
  theme_bw()




ggplot(countryConf, aes(country_conf))+geom_density()+theme_bw()

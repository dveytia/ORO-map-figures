#################################################################################################
#                                                                                               #
# FIGURE 5                                                                                      #
# Panel A: Map of mitigation options and CO2 country emisssions (two color scales)              #  
#          (1) Bivariate CS (EEZ's color) ==> CO2 em. per country ~ # ORO pub on BC and MRE)    # 
#          (2) Univariate color scale (countrie's color) ==> # pub mitigations ORO (1st author) #
# Panel B:                               #
#                                                                                               #
#################################################################################################
rm(list = ls(), envir = .GlobalEnv) # clean the environment

### ----- LOAD LIBRARIES -----
library(dplyr)
library(dbplyr)
library(R.utils)
library(RSQLite)
library(ggplot2)
library(tidyr)
library(stringr)
library(viridis)
library(rgdal)
library(countrycode)
library(broom)
library(raster)

### ----- LOAD FUNCTIONS -----
source(here::here("R", "functions_to_format.R")) # all functions needed to format data
source(here::here("R", "functions_to_plot.R")) # all functions needed to plot data


### ----- CONNECTION TO THE LATEST VERSION OF THE SQL DATABASE -----
sqliteDir <- here::here("data/sqlite-databases")
sqliteFiles <- dir(sqliteDir)
sqliteVersions <- as.numeric(gsub(".sqlite","",substring(sqliteFiles, regexpr("_v", sqliteFiles) + 2)))
latestVersion <- sqliteFiles[which.max(sqliteVersions)]
dbcon <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(sqliteDir, latestVersion), create=FALSE)


### ----- PANEL A -----

  ## ---- LOAD DATA
  grid_df <- tbl(dbcon, "grid_df_res2.5") 
  shp_df_matches <- tbl(dbcon, "geoparsed-text_shp_df_matches") 
  shp_id_match <- tbl(dbcon, "shp_df_natural-earth-shapes") |> dplyr::select(shpfile_id, sovereignt, sov_a3, admin, adm0_a3) 
  pred_oro_any_mitigation <- tbl(dbcon, "pred_oro_any_mitigation")
  pred_blue_carbon <- tbl(dbcon, "pred_blue_carbon")
  uniquerefs <- tbl(dbcon, "uniquerefs") # metadata on the unique references
  pred_oro_branch <- tbl(dbcon, "pred_oro_branch") # predictions for ORO branch
  pred_relevance <- tbl(dbcon, "pred_relevance") # which articles are relevant to OROs
  
  GHGemi_country <- read.csv(file = here::here("data", "external", "ghg-emissions", "owid-co2-data.csv")) # Countrie's GHG emissions
  countries_ls <- read.csv(file = here::here("data", "external", "list_of_countries", "sql-pays.csv"), sep = ";") # Countries names
  world_shp <- sf::read_sf(here::here("data", "external", "world_shp")) |>  # shape file of the world
    mutate(NA2_DESCRI = countrycode(sourcevar   = NA2_DESCRI,
                                    origin      = "country.name",
                                    destination = "country.name",
                                    nomatch     = NULL),
           iso_NA2 = countrycode(sourcevar   = NA2_DESCRI,
                                 origin      = "country.name",
                                 destination = "iso3c"))

  
  landlocked <- read.csv(file = here::here("data", "external", "special_country_groups", "landlocked-countries-2024.csv"), sep = ",") |> 
    dplyr::select(country, LandlockedCountries) |> 
    rename(Country    = country, 
           group_land = LandlockedCountries) |> 
    mutate(iso_code = countrycode(sourcevar   = Country,
                                  origin      = "country.name",
                                  destination = "iso3c"),
           group_land = case_when(group_land == "yes" ~ "Land-locked",
                                  TRUE ~ "NA"))
  
  AMUNRC <- c("American Samoa", "Anguilla", "Aruba", "Bermuda", "British", "Virgin Islands", "Cayman Islands", "Commonwealth of Northern Marianas",
              "Curacao", "French Polynesia", "Guadeloupe", "Guam", "Martinique", "Montserrat", "New Caledonia", "Puerto Rico", "Sint Maarten",
              "Turks and Caicos Islands", "United States Virgin Islands")
  
  country_grp <- read.csv(file = here::here("data", "external", "special_country_groups", "special-country-groups.csv"), sep = ",") |> 
    dplyr::select(Country, LLDC, SIDS) |> 
    mutate(iso_code = countrycode(sourcevar   = Country,
                                  origin      = "country.name",
                                  destination = "iso3c"),
           group_land = case_when(LLDC == "No" & SIDS == "Yes" ~ "SIDS",
                                  LLDC == "Yes" & SIDS == "No" ~ "Land-locked",
                                  TRUE ~ "Coastal")) |> 
    dplyr::select(-LLDC, -SIDS) |> 
    rbind(landlocked) |> 
    distinct() |> 
    mutate(group_land = case_when(Country %in% AMUNRC ~ "AMUNRC",
                                  TRUE ~ group_land))
  
  eez_shp <- sf::st_read(here::here("data", "external", "eez_shp", "eez_v12.shp")) |>  # shape file of countrie's EEZ
    dplyr::select(MRGID, POL_TYPE, TERRITORY1, SOVEREIGN1, ISO_SOV1) |> 
    dplyr::rename(Country = SOVEREIGN1) |> 
    dplyr::mutate(Country = countrycode(sourcevar   = ISO_SOV1,
                                        origin      = "iso3c",
                                        destination = "country.name"),
                  iso_code = countrycode(sourcevar   = Country,
                                         origin      = "country.name",
                                         destination = "iso3c"),
                  TERRITORY1 = countrycode(sourcevar   = TERRITORY1,
                                           origin      = "iso3c",
                                           destination = "country.name",
                                           nomatch     = NULL),
                  iso_NA2 = countrycode(sourcevar   = TERRITORY1,
                                        origin      = "country.name",
                                        destination = "iso3c")) |> 
    full_join(country_grp |>  dplyr::select(-iso_code), by = c("TERRITORY1" = "Country")) |> 
    mutate(group_land = case_when(Country == "Cape Verde" ~"Island", TRUE ~ group_land))
  # Flanders Marine Institute (2023). Maritime Boundaries Geodatabase: Maritime Boundaries and Exclusive Economic Zones (200NM), version 12. Available online at https://www.marineregions.org/. https://doi.org/10.14284/632
  
  # test <- sf::st_union(world_shp, eez_shp)
  
  ## ---- FORMAT DATA
  
    # --- Shapefile of grid cells with the country & administration they belong to.
    grid_sf <- tbl(dbcon, "grid_df_res2.5") |> collect() |> 
      sf::st_as_sf(coords = c("LON", "LAT"), crs = 4326)
    
      # - Join with land data
      sf::sf_use_s2(FALSE)
      grid_sf_land <- sf::st_join(grid_sf, world_shp) |> 
        dplyr::select(grid_df_id, is_land, NA2_DESCRI, iso_NA2)
      
      # - Join with eez data
      grid_sf_eez <- sf::st_join(grid_sf, eez_shp) |> 
        dplyr::select(grid_df_id, is_land, MRGID, TERRITORY1, iso_NA2, Country, iso_code) |> 
        distinct(grid_df_id, .keep_all = TRUE)
      
      # - Land + eez data
      grid_sf_eez_land <- sf::st_join(grid_sf_land, grid_sf_eez |> dplyr::select(-grid_df_id))
      grid_df_eez_land <- sf::st_drop_geometry(grid_sf_eez_land) |> 
        mutate(country_id = case_when(is.na(Country) & !is.na(NA2_DESCRI) ~ NA2_DESCRI,
                                      is.na(NA2_DESCRI) & !is.na(Country) ~ Country,
                                      is.na(NA2_DESCRI) & is.na(Country)  ~ "High-seas",
                                      !is.na(NA2_DESCRI) & !is.na(Country) & Country != NA2_DESCRI ~ "Verif",
                                      !is.na(NA2_DESCRI) & !is.na(Country) & Country == NA2_DESCRI ~ Country),
               TERRITORY1 = case_when(!is.na(TERRITORY1) ~ TERRITORY1,
                                      is.na(TERRITORY1) ~ country_id)) |> 
        dplyr::select(grid_df_id, TERRITORY1, country_id) |> 
        left_join(grid_df |>  dplyr::select(-area_km, -is_land), by = "grid_df_id", copy = TRUE)
      
      # sum(grid_df_eez_land$country_id == "High-seas")/nrow(grid_df_eez_land) * 100

      
    # --- Mitigation data
    mitigation_grid_df <- pred_oro_branch |> 
      left_join(pred_relevance, by = "analysis_id") |> 
      filter(0.5 <= relevance_mean) |> 
      mutate(mitigation = ifelse(0.5 <= `oro_branch.Mitigation - mean_prediction`, 1 ,0)) |>  
      filter(mitigation == 1) |>
      dplyr::select(analysis_id, mitigation) |> 
      # Select only relevant articles and get there shp_id
      left_join(shp_df_matches |> distinct(grid_df_id, analysis_id, shp_id, place), by = "analysis_id") |>
      right_join(grid_df_eez_land, by = "grid_df_id", copy = TRUE) |>  
      filter(! is.na(analysis_id)) |> 
      mutate(country_id = case_when(TERRITORY1 == "Greenland" ~ "Denmark",
                                    TRUE ~ country_id)) |> 
      distinct(analysis_id, TERRITORY1, country_id) |>
      collect()
    
    mitigation_geop_paper_country <- mitigation_grid_df |> 
      group_by(TERRITORY1, country_id) |> 
      summarise(n_geop_paper = n()) 
    
    ## Select the 100 papers of the countries with the least number of paper
    bottom_countries_papers <- mitigation_geop_paper_country |> 
      ungroup() |> 
      arrange(n_geop_paper) |> 
      filter(cumsum(n_geop_paper) > 150 & cumsum(n_geop_paper) < 250) |> # to have ~ 100 papers. length(unique(bottom_countries_papers$analysis_id))
      left_join(mitigation_grid_df |>  dplyr::select(-country_id), by = "TERRITORY1") |> 
      distinct(analysis_id, .keep_all = TRUE) |> 
      arrange(analysis_id) 
      
    length(unique(bottom_countries_papers$analysis_id))
    
    subset_data527   <- sf::st_as_sf(mitigation_grid_df |> filter(analysis_id == 527), coords = c("LON", "LAT"), crs = 4326) # UK
    subset_data1820  <- sf::st_as_sf(mitigation_grid_df |> filter(analysis_id == 1820), coords = c("LON", "LAT"), crs = 4326) # UK
    subset_data4605  <- sf::st_as_sf(mitigation_grid_df |> filter(analysis_id == 4605), coords = c("LON", "LAT"), crs = 4326) # France
    subset_data2858  <- sf::st_as_sf(mitigation_grid_df |> filter(analysis_id == 2858), coords = c("LON", "LAT"), crs = 4326) # United States
    subset_data2932  <- sf::st_as_sf(mitigation_grid_df |> filter(analysis_id == 2932), coords = c("LON", "LAT"), crs = 4326) # United States
    
    
    ggplot(data = subset_data35143) +
      geom_sf(data = world_shp, fill = "grey95") +
      geom_sf(color = "red", size = 3) +
      theme_bw()
    
    
    check <- pred_oro_branch |> 
      left_join(pred_relevance, by = "analysis_id") |> 
      filter(0.5 <= relevance_mean) |> 
      mutate(mitigation = ifelse(0.5 <= `oro_branch.Mitigation - mean_prediction`, 1 ,0)) |>  
      filter(mitigation == 1) |>
      dplyr::select(analysis_id, mitigation) |> 
      # Select only relevant articles and get there shp_id
      left_join(shp_df_matches |> distinct(grid_df_id, analysis_id, shp_id, place), by = "analysis_id") |>
      collect()|> 
      filter(str_detect(place, "taiwan")) |> 
      # right_join(grid_df_eez_land, by = "grid_df_id", copy = TRUE) |> 
      filter(! is.na(analysis_id)) |> 
      distinct(analysis_id) 
    
    
      test <- mitigation_grid_df |>  filter(analysis_id == "266632") |>  distinct(analysis_id, TERRITORY1, country_id, .keep_all = TRUE)
      
      tmp <- mitigation_grid_df |>  group_by(analysis_id) |> summarise(n = n())

    ## Here, you have some NA in the shp_id column. Is that oceanic cells?
      
      
      # Join with info about which grid cells they match to
      # left_join(shp_df_matches, by = "analysis_id") %>% 
      # replace_na(list(shp_id = -999)) |> # correspond to the high seas????????
      # for each unique grid cell, take the weighted sum
      # group_by(grid_df_id, shp_id) %>%
  # group_by(shp_id) %>%
  # summarise(n_articles_weighted = sum(cell_weight, na.rm = TRUE)) %>% 
      # Join with information about where to plot the grid cells
      # right_join(grid_df, by = "grid_df_id") %>%
      # full_join(shp_id_match, by = c("shp_id" = "shpfile_id")) |>
      # This last step collects all the relevant data from the database 
      collect() 
    
    
    country_grid_df <- shp_df_matches |> collect()
    
    test <- shp_df_matches |>
      # Select only relevant articles
      right_join(mitigation_grid_df, by = "analysis_id")
    full_join(shp_id_match, by = c("shp_id" = "shpfile_id")) |> 
      collect(shp_id_match)
    
    tmp <- 
      collect()
    
    # --- Identify the country of the geoparsed data (in terrestrial land)
    
      # - Transform into a spatial object
      mitigation_grid_sf_land <- mitigation_grid_df2 |> 
        filter(is_land == 1) |> 
        sf::st_as_sf(coords = c("LON", "LAT"), crs = 4326)
      
      # - Intersection with land shapefile
      sf::sf_use_s2(FALSE)
      mitigation_geoP_sf_land <- sf::st_join(world_shp, mitigation_grid_sf_land) |> 
        sf::st_drop_geometry() |> 
        left_join(grid_df |>  dplyr::select(-area_km, -is_land), by = "grid_df_id", copy = TRUE) |>
        # filter(!is.na(LAT) & !is.na(LON)) |>
        dplyr::select(NA2_DESCRI, iso_NA2, grid_df_id, n_articles_weighted) |> 
        group_by(NA2_DESCRI, iso_NA2) |>
        summarise(n_articles_weighted_land = sum(n_articles_weighted, na.rm = TRUE)) |> 
        ungroup()
    
    # --- Identify the country of the geoparsed data (in eez land)
    
      # - Transform into a spatial object
      mitigation_grid_sf_eez <- mitigation_grid_df |> 
        filter(is_land == 0) |> 
        sf::st_as_sf(coords = c("LON", "LAT"), crs = 4326)
      
      # - Intersection with eez shapefile
      mitigation_geoP_sf_eez <- sf::st_join(eez_shp, mitigation_grid_sf_eez) |> 
        sf::st_drop_geometry() |> 
        left_join(grid_df |>  dplyr::select(-area_km, -is_land), by = "grid_df_id", copy = TRUE) |>
        # filter(!is.na(LAT) & !is.na(LON)) |>
        dplyr::select(TERRITORY1, iso_NA2, Country, group_land, n_articles_weighted) |> 
        group_by(TERRITORY1, iso_NA2, Country, group_land) |>
        summarise(n_articles_weighted_eez = sum(n_articles_weighted, na.rm = TRUE)) |> 
        ungroup() |> 
        mutate(Country = str_replace_all(Country, c("Côte d’Ivoire"       = "Ivory Coast",
                                                    "Congo - Brazzaville" = "Republic of the Congo",
                                                    "Congo - Kinshasa"    = "Democratic Republic of the Congo",
                                                    "Somalia"             = "Federal Republic of Somalia")),
               group_land = case_when(group_land %in% c("Island", "Land-locked", "SIDS", "Coastal") ~ group_land,
                                      !is.na(group_land) & TERRITORY1 != Country ~ "Island",
                                      is.na(group_land)  & TERRITORY1 != Country ~ "Island",
                                      is.na(group_land)  & TERRITORY1 == Country ~ "Coastal"))
      
    # --- Merge eez + land data
    mitigation_geoP_sf <- full_join(mitigation_geoP_sf_eez, mitigation_geoP_sf_land |>  dplyr::select(-iso_NA2), 
                                    by = c("TERRITORY1" = "NA2_DESCRI")) |> 
      replace_na(list(n_articles_weighted_eez = 0, n_articles_weighted_land = 0)) |> 
      mutate(n_article = n_articles_weighted_eez + n_articles_weighted_land)
    
    mitigation_geoP_sf2 <- full_join(mitigation_geoP_sf_eez, mitigation_geoP_sf_land, by = "iso_NA2") |> 
      replace_na(list(n_articles_weighted_eez = 0, n_articles_weighted_land = 0)) |> 
      mutate(n_article = n_articles_weighted_eez + n_articles_weighted_land)
    
    
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
  pred_oro_any_mitigation <- tbl(dbcon, "pred_oro_any_mitigation")
  pred_blue_carbon <- tbl(dbcon, "pred_blue_carbon")
  uniquerefs <- tbl(dbcon, "uniquerefs") # metadata on the unique references
  pred_oro_branch <- tbl(dbcon, "pred_oro_branch") # predictions for ORO branch
  pred_relevance <- tbl(dbcon, "pred_relevance") # which articles are relevant to OROs
  
  GHGemi_country <- read.csv(file = here::here("data", "external", "ghg-emissions", "owid-co2-data.csv")) # Countrie's GHG emissions
  countries_ls <- read.csv(file = here::here("data", "external", "list_of_countries", "sql-pays.csv"), sep = ";") # Countries names
  world_shp <- sf::read_sf(here::here("data", "external", "world_shp")) # shape file of the world
  eez_shp <- sf::st_read(here::here("data", "external", "eez_shp", "eez_v12.shp")) |>  # shape file of countrie's EEZ
    dplyr::select(MRGID, POL_TYPE, TERRITORY1, SOVEREIGN1, ISO_SOV1) |> 
    dplyr::rename(Country = SOVEREIGN1) |> 
    dplyr::mutate(Country = countrycode(sourcevar   = ISO_SOV1,
                                        origin      = "iso3c",
                                        destination = "country.name"),
                  iso_code = countrycode(sourcevar   = Country,
                                         origin      = "country.name",
                                         destination = "iso3c"))
  # Flanders Marine Institute (2023). Maritime Boundaries Geodatabase: Maritime Boundaries and Exclusive Economic Zones (200NM), version 12. Available online at https://www.marineregions.org/. https://doi.org/10.14284/632

  ## ---- FORMAT DATA
  
    # --- Mitigation data
    mitigation_grid_df <- pred_blue_carbon %>%
      # Get predictions for blue carbon and MRE for each article
      full_join(pred_oro_any_mitigation, by = "analysis_id") %>%
      rename(blue_carbon = `0 - relevance - mean_prediction`,
             marine_renewable_energy = `oro_any.M_Renewables - mean_prediction`) %>%
      dplyr::select(analysis_id, blue_carbon, marine_renewable_energy) %>%
      # Filter to only the articles that are relevant for either
      mutate(blue_carbon = ifelse(0.5 <= blue_carbon, 1,0),
             marine_renewable_energy = ifelse(0.5 <= marine_renewable_energy, 1, 0)) %>%
      filter(blue_carbon == 1 | marine_renewable_energy == 1) %>%
      # Join with info about which grid cells they match to
      left_join(shp_df_matches, by = "analysis_id") %>% 
      # for each unique grid cell, take the weighted sum
      group_by(grid_df_id) %>%
      summarise(n_articles_weighted = sum(cell_weight, na.rm = TRUE)) %>%
      # Join with information about where to plot the grid cells
      right_join(grid_df, by = "grid_df_id") %>%
      # This last step collects all the relevant data from the database 
      collect() 
    
      # - Trasform into a spatial object
      mitigation_grid_sf <- sf::st_as_sf(mitigation_grid_df, coords = c("LON", "LAT"), crs = 4326)
      
      # - Collect only data geoparsed in the eez
      sf::sf_use_s2(FALSE)
      MRE_BC_mean_eez_sf <- eez_shp |> 
        sf::st_join(mitigation_grid_sf) |> 
        sf::st_drop_geometry() |> 
        left_join(grid_df, by = "grid_df_id", copy = TRUE) |>
        filter(!is.na(LAT) & !is.na(LON)) |> 
        group_by(MRGID, Country, ISO_SOV1) |> 
        summarise(n_articles_weighted = sum(n_articles_weighted, na.rm = TRUE)) |> 
        ungroup()

      
      MRE_BC_mean_eez_sf2 <- eez_shp |>
        left_join(MRE_BC_mean_eez_sf |> dplyr::select(MRGID, n_articles_weighted), by = "MRGID") |> 
        replace_na(list(n_articles_weighted = 0))
      
      tmp2 <- sf::st_drop_geometry(MRE_BC_mean_eez_sf2) 
      
    # --- Emissions data
    GHGemi_country2 <- GHGemi_country |> 
      filter(year == 2020) |>
      dplyr::select(country, cumulative_co2_including_luc, cumulative_co2, iso_code) |>
      mutate(country = stringr::str_replace_all(country, c("Micronesia \\(country\\)" = "Micronesia (Federated States of)"))) |> 
      # filter(!is.na(cumulative_co2_including_luc)) |> 
      filter(iso_code != "") |> 
      mutate(country = countrycode(sourcevar   = country,
                                   origin      = "country.name",
                                   destination = "country.name"),
             cumulative_co2_including_luc = case_when(is.na(cumulative_co2_including_luc) == FALSE ~ cumulative_co2_including_luc,
                                                      is.na(cumulative_co2_including_luc) == TRUE ~ cumulative_co2)) |> 
      filter(!is.na(cumulative_co2_including_luc))
    
    test <-  GHGemi_country |> 
      dplyr::select(country, cumulative_co2_including_luc, cumulative_co2, iso_code)
    
    # --- Merge data
    data_panelA <- MRE_BC_mean_eez_sf2 |> 
      full_join(GHGemi_country2 |>  dplyr::select(-country), by = "iso_code") |> 
      replace_na(list(n_articles_weighted = 0)) |> 
      # is.na(MRGID) corresponds to land-locked countries and some oversea territories whose emissions equals all country emissions
      filter(!is.na(cumulative_co2_including_luc) & !is.na(MRGID))
    
    # !!! Check NA in emisssions data.  cumulative_co2_including_luc = 0 !!!!!
    
    tmp <- sf::st_drop_geometry(data_panelA) 
    
    # --- Bivariate color scale
    bivariate_color_scale <- color_bivariate_map(nquantiles  = 10, 
                                                 upperleft   = "#0096EB", # rgb(130,0,80, maxColorValue = 255), 
                                                 upperright  = "#FFE60F", # rgb(255,230,15, maxColorValue = 255),
                                                 bottomleft  = "#e1edf7",
                                                 bottomright = "#820050", # rgb(0,150,235, maxColorValue = 255)  
                                                 ylab        = "CO2 emission (cum)",
                                                 xlab        = "n_weighted_papers")
    
    # --- Adapt it to the data
    data_bivar_n_article_CO2em <- format_data_bivariate_map(data        = data_panelA,
                                                            data.x      = "n_articles_weighted",
                                                            data.y      = "cumulative_co2_including_luc",
                                                            color_table = bivariate_color_scale,
                                                            probs.quant = seq(0,1,0.1)) |> 
      sf::st_transform(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
    tmp <- sf::st_drop_geometry(data_bivar_n_article_CO2em) 
    
    data_2_map_panelA <- format_data2map(data = data_bivar_n_article_CO2em,
                                         PROJ = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    world_shp_robin <- sf::st_transform(world_shp, crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
  ## ---- PLOT PANEL A
  bivariate_map(data_map   = data_2_map_panelA,
                data_world = world_shp_robin,
                color      = bivariate_color_scale,
                ylab       = "CO2eq. emissions",
                xlab       = "# mit. paper",
                name       = "main/Figure5_PanelA_geop")
  
### -----
  
### ----- PANEL B -----
  
  ## ---- LOAD DATA
  grid_df <- tbl(dbcon, "grid_df_res2.5")
  shp_df_matches <- tbl(dbcon, "geoparsed-text_shp_df_matches")
  pred_oro_branch <- tbl(dbcon, "pred_oro_branch") # predictions for ORO branch
  
  uniquerefs <- tbl(dbcon, "uniquerefs") # metadata on the unique references
  pred_oro_branch <- tbl(dbcon, "pred_oro_branch") # predictions for ORO branch
  pred_relevance <- tbl(dbcon, "pred_relevance") # which articles are relevant to OROs
  
  mhvs_rast <- raster::raster(here::here("data", "external", "climate-hazard-maps", "coastal_mhvs_1982-2018.tif"))
  sst_rast <- raster::raster(here::here("data", "external", "climate-hazard-maps", "coastal_sst_1980-2018.tif"))
  storms_rast <- raster::raster(here::here("data", "external", "climate-hazard-maps", "coastal_tracks_1980-2018_points.tif"))
  surges_rast <- raster::raster(here::here("data", "external", "climate-hazard-maps", "Coastal_surges_1974-2014_R.tif"))
  
  countries_ls <- read.csv(file = here::here("data", "external", "list_of_countries", "sql-pays.csv"), sep = ";") # Countries names
  world_shp <- sf::read_sf(here::here("data", "external", "world_shp")) # shape file of the world
  eez_shp <- sf::st_read(here::here("data", "external", "eez_shp", "eez_v12.shp")) |>  # shape file of countrie's EEZ
    dplyr::select(MRGID, POL_TYPE, TERRITORY1, SOVEREIGN1, ISO_SOV1) |> 
    dplyr::rename(Country = SOVEREIGN1) |> 
    dplyr::mutate(Country = countrycode(sourcevar   = ISO_SOV1,
                                        origin      = "iso3c",
                                        destination = "country.name"),
                  iso_code = countrycode(sourcevar   = Country,
                                         origin      = "country.name",
                                         destination = "iso3c"))
  
  ## ---- FORMAT DATA
  
    # --- Vulnerability

      # - List of raster
      rast_ls <- list(mhvs = mhvs_rast, sst = sst_rast, storms = storms_rast, surges = surges_rast)
      raster::plot(stack(rast_ls))
      
      # - Normalize values between 0 and 1
      rast_ls01 <- stack(lapply(rast_ls, normalize_rast_values_01))
      raster::plot(rast_ls01, colNA = "darkblue")
      
      # - Take the mean normalized values of the 4 variables
      vulne_mean_rast <- calc(rast_ls01, fun = mean)
      vulne_mean_rast1 <- raster::aggregate(vulne_mean_rast, fact = 4, fun = mean)
      raster::plot(vulne_mean_rast1, colNA = "darkblue")
      
      # - Shapefile and summarize across eez and find the corresponding country
      sf::sf_use_s2(FALSE)
      vulne_shp <- sf::st_as_sf(stars::st_as_stars(vulne_mean_rast1)) 
      vulne_shp_country <- eez_shp |> 
        sf::st_join(vulne_shp) 
      
      vulne_df_per_grid <- sf::st_drop_geometry(vulne_shp_country) |> 
        rename(vulnerability = layer)
      
      vulne_country_df <- vulne_shp_country |> 
        sf::st_drop_geometry() |> 
        group_by(MRGID, TERRITORY1, Country, iso_code) |> 
        summarise(vulnerability = mean(layer, na.rm = TRUE)) |> 
        ungroup()
      
    # --- Adaptation ORO
      
      # - Select articles
      adaptation_grid_df <- pred_oro_branch |> 
        # Get predictions for adaptation relevance each article and filter
        # to only relevant articles
        mutate(adaptation = ifelse(0.5 <= `oro_branch.Nature - mean_prediction` |
                                   0.5 <= `oro_branch.Societal - mean_prediction`,
                                   1,0)) |> 
        dplyr::select(analysis_id, adaptation) |> 
        filter(adaptation == 1) |> 
        # Join with info about which grid cells they match to
        left_join(shp_df_matches, by = "analysis_id") |> 
        # for each unique grid cell, take the weighted sum
        group_by(grid_df_id) |> 
        summarise(n_articles_weighted = sum(cell_weight, na.rm = TRUE)) |>  
        # Join with information about where to plot the grid cells
        right_join(grid_df, by = "grid_df_id") |> 
        # This last step collects all the relevant data from the database 
        collect() 
      
      # - Sum over EEZ
      adapt_sf <- sf::st_as_sf(adaptation_grid_df, coords = c("LON", "LAT"), crs = 4326)
      adapt_sf_eez <- eez_shp |> 
        sf::st_join(adapt_sf) |> 
        sf::st_drop_geometry() |> 
        left_join(grid_df, by = "grid_df_id", copy = TRUE) |>
        filter(!is.na(LAT) & !is.na(LON)) |> 
        group_by(MRGID, Country, ISO_SOV1) |> 
        summarise(n_articles_weighted = sum(n_articles_weighted, na.rm = TRUE)) |> 
        ungroup()
      
      adapt_sf_eez2 <- eez_shp |> 
        left_join(adapt_sf_eez |> dplyr::select(MRGID, n_articles_weighted), by = "MRGID") |> 
        replace_na(list(n_articles_weighted = 0))
      
      tmp2 <- sf::st_drop_geometry(adapt_sf_eez2) 
      tmp3 <- sf::st_drop_geometry(eez_shp) 
      
      
    # --- Bivariate color scale
      
      # --- Merge data
      data_panelB <- adapt_sf_eez2 |> 
        full_join(vulne_country_df |>  dplyr::select(-Country, -iso_code, -TERRITORY1), by = "MRGID") |> 
        replace_na(list(n_articles_weighted = 0)) 

      tmp <- sf::st_drop_geometry(data_panelB) 
      
      # --- Create the bivariate color scale
      bivariate_color_scale <- color_bivariate_map(nquantiles  = 10, 
                                                   upperleft   = "#0096EB", # rgb(130,0,80, maxColorValue = 255), 
                                                   upperright  = "#FFE60F", # rgb(255,230,15, maxColorValue = 255),
                                                   bottomleft  = "#e1edf7",
                                                   bottomright = "#820050", # rgb(0,150,235, maxColorValue = 255)  
                                                   ylab        = "vulnerability",
                                                   xlab        = "n_weighted_papers")
      
      # --- Adapt it to the data
      data_bivar_n_article_vulne <- format_data_bivariate_map(data        = data_panelB,
                                                              data.x      = "n_articles_weighted",
                                                              data.y      = "vulnerability",
                                                              color_table = bivariate_color_scale,
                                                              probs.quant = seq(0,1,0.1)) |> 
        sf::st_transform(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
      
      tmp <- sf::st_drop_geometry(data_bivar_n_article_vulne) 
      
      data_2_map_panelB <- format_data2map(data = data_bivar_n_article_vulne,
                                           PROJ = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
      world_shp_robin <- sf::st_transform(world_shp, crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
      
      ## ---- PLOT PANEL A
      bivariate_map(data_map   = data_2_map_panelB,
                    data_world = world_shp_robin,
                    color      = bivariate_color_scale,
                    ylab       = "Vulnerability",
                    xlab       = "# ada. paper",
                    name       = "main/Figure5_PanelB_geop2")
      
      
    

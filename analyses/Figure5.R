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
library(broom)

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
  
  GHGemi_country <- read.csv(file = here::here("data", "external", "ghg-emissions", "owid-co2-data.csv")) # Countrie's GHG emissions
  countries_ls <- read.csv(file = here::here("data", "external", "list_of_countries", "sql-pays.csv"), sep = ";") # Countries names
  world_shp <- sf::read_sf(here::here("data", "external", "world_shp")) # shape file of the world
  eez_shp <- sf::st_read(here::here("data", "external", "eez_shp", "eez_v12.shp")) |>  # shape file of countrie's EEZ
    dplyr::select(MRGID, POL_TYPE, TERRITORY1, SOVEREIGN1, ISO_SOV1) |> 
    dplyr::rename(Country = SOVEREIGN1)
  # Flanders Marine Institute (2023). Maritime Boundaries Geodatabase: Maritime Boundaries and Exclusive Economic Zones (200NM), version 12. Available online at https://www.marineregions.org/. https://doi.org/10.14284/632

  ## ---- FORMAT DATA
  
    # --- Univariate color scale: # pub of Mitigation OROs by 1st author affiliation
    mitPubs <- pred_oro_branch %>%
      mutate(mitigation = ifelse(0.5 <= `oro_branch.Mitigation - mean_prediction`, 1,0))%>%
      select(analysis_id, mitigation) %>%
      # Join with affiliation information
      left_join(uniquerefs %>% select(analysis_id, affiliation), by = "analysis_id") %>%
      collect()
    
      # - Extract the country of the first author for each relevant publications
      mitPubs_aff <- extract_1stA_affiliation(data         = mitPubs, 
                                              countries_ls = countries_ls)
      
      # - Number of publications per country
      mitPubs_per_country <- mitPubs_aff$oroAff_1stA |> 
        dplyr::filter(!is.na(country_aff)) |>
        dplyr::group_by(country_aff) |> 
        dplyr::summarise(Count_ORO = n())|> 
        dplyr::filter(!is.na(country_aff)) |> 
        rename(Country = country_aff)
      
      # - Format the shapefile of the world countries polygon and bind data
      world_shp_boundaries <- format_shp_of_the_world(world_shp    = world_shp,
                                                      data_to_bind = mitPubs_per_country,
                                                      PROJ         = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
      
      
    # --- Bivariate color scale:
    # --- Weighted # of articles relevant to blue carbon or mitigation per grid cell
    PROJ = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
      
    mitigation_grid_df <- pred_blue_carbon %>%
      # Get predictions for blue carbon and MRE for each article
      full_join(pred_oro_any_mitigation, by = "analysis_id") %>%
      rename(blue_carbon = `0 - relevance - mean_prediction`,
             marine_renewable_energy = `oro_any.M_Renewables - mean_prediction`) %>%
      select(analysis_id, blue_carbon, marine_renewable_energy) %>%
      # Filter to only the articles that are relevant for either
      mutate(blue_carbon = ifelse(0.5 <= blue_carbon, 1,0),
             marine_renewable_energy = ifelse(0.5 <= marine_renewable_energy, 1, 0)) %>%
      filter(blue_carbon == 1 | marine_renewable_energy == 1) %>%
      # Join with info about which grid cells they match to
      left_join(shp_df_matches, by = "analysis_id") %>%
      # for each unique grid cell, take the weighted sum
      group_by(grid_df_id)%>%
      summarise(n_articles_weighted = sum(cell_weight))%>%
      # Join with information about where to plot the grid cells
      right_join(grid_df, by = "grid_df_id") %>%
      # This last step collects all the relevant data from the database 
      collect() 
    
      # - Trasform into a spatial object
      mitigation_grid_sf <- sf::st_as_sf(mitigation_grid_df, coords = c("LON", "LAT"), crs = 4326) |> 
        sf::st_transform(crs = PROJ)
      
      mit_grid_land <- filter(mitigation_grid_sf, is_land == 1)
      range(mit_grid_land$n_articles_weighted, na.rm=T)
      sum(mit_grid_land$n_articles_weighted > 0, na.rm = T) # 2656 terrestrial cells with 
      
      ggplot(data = mit_grid_land) +
        geom_sf(aes(color = n_articles_weighted)) +
        scale_colour_viridis_c(na.value = "grey80") +
        theme_bw()
      
      mit_grid_sea <- filter(mitigation_grid_sf, is_land == 0)
      range(mit_grid_sea$n_articles_weighted, na.rm=T)
      ggplot(data = mit_grid_sea) +
        geom_sf(aes(color = n_articles_weighted)) +
        scale_colour_viridis_c(na.value = "grey80") +
        theme_bw()
      


    
      # - Annual mean of countries's GHG emissions over the 2000-2020 period 
      # - Full code book available here: https://github.com/owid/co2-data
      mean_GHGemi_country <- GHGemi_country |> 
        select(year, iso_code, total_ghg) |>  # total_ghg refers to total GHG emissions (in MtCO2eq) including land-use change and forestry.
        filter(year >= 2000 & year <=2020) |> 
        group_by(iso_code) |> 
        summarise(an_mean_co2eq_em = mean(total_ghg, na.rm = T),
                  sd_co2eq_em      = sd(total_ghg, na.rm = T)) |> 
        filter(iso_code != "") 
      
      # - Bind data with eez shapefile
      GHGemi_country_eez <- eez_shp |> 
        left_join(mean_GHGemi_country, by = c("ISO_SOV1" = "iso_code")) |> 
        sf::st_transform(crs = PROJ) 
      
      plot(GHGemi_country_eez["an_mean_co2eq_em"])
      
      # - Intersection: merge point matching in eez polygons
      GHGemi_mitigationGRID_eez <- sf::st_join(mitigation_grid_sf, GHGemi_country_eez)
      
      GHGemi_mitigationGRID_eez <- sf::st_intersection(GHGemi_country_eez, mitigation_grid_sf)
    
      # !!!!! Check the period !!!!!
      
      
      

    
    

    
    

    


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
  pred_oro_any_mitigation <- tbl(dbcon, "pred_oro_any_mitigation")
  pred_blue_carbon <- tbl(dbcon, "pred_blue_carbon")
  uniquerefs <- tbl(dbcon, "uniquerefs") # metadata on the unique references
  pred_oro_branch <- tbl(dbcon, "pred_oro_branch") # predictions for ORO branch
  pred_relevance <- tbl(dbcon, "pred_relevance") # which articles are relevant to OROs
  
  GHGemi_country <- read.csv(file = here::here("data", "external", "ghg-emissions", "owid-co2-data.csv")) # Countrie's GHG emissions
  countries_ls <- read.csv(file = here::here("data", "external", "list_of_countries", "sql-pays.csv"), sep = ";") # Countries names
  world_shp <- sf::read_sf(here::here("data", "external", "world_shp")) # shape file of the world

  ## ---- FORMAT DATA
  
    # --- Bivariate scale for emission ~ f(# mitigation papers)
    mitPubs <- pred_oro_branch %>%
      left_join(pred_relevance, by = "analysis_id") |> 
      filter(0.5 <= relevance_mean) %>% 
      mutate(mitigation = ifelse(0.5 <= `oro_branch.Mitigation - mean_prediction`, 1,0)) %>%
      dplyr::select(analysis_id, mitigation) %>%
      filter(mitigation == 1) |> 
      # Join with affiliation information
      left_join(uniquerefs %>% dplyr::select(analysis_id, affiliation) , by = "analysis_id") %>% 
      collect()
    
    # --- Extract the country of the first author for each relevant publications
    mitPubs_aff <- extract_1stA_affiliation(data         = mitPubs, 
                                            countries_ls = countries_ls) 
    
    # --- Number of publications per country
    mitPubs_per_country <- mitPubs_aff$oroAff_1stA |> 
      # Give the same name to countries with varying names (Congo (Republic democratic of) & Republic democratic of)
      dplyr::mutate(country_aff = countrycode(sourcevar   = country_aff,
                                              origin      = "country.name",
                                              destination = "country.name"),
                    iso_code = countrycode(sourcevar   = country_aff,
                                           origin      = "country.name",
                                           destination = "iso3c")) |> 
      dplyr::group_by(country_aff) |> 
      dplyr::summarise(Count_ORO = n())|> 
      rename(Country = country_aff) 
    
    # --- Cumulative CO2 emissions per country
    GHGemi_mitPubs_country <- GHGemi_country |> 
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
      # Filter rows returning NA because not identified as a country (reason behing the warning message)
      filter(!is.na(cumulative_co2_including_luc)) |> 
      full_join(mitPubs_per_country, by = c("country" = "Country")) |> 
      mutate(iso_code = countrycode(sourcevar   = country,
                                    origin      = "country.name",
                                    destination = "iso3c")) |> 
      rename(Country = country) |> 
      replace_na(list(Count_ORO = 0))


    # --- Create the bivariate color scale
    bivariate_color_scale <- color_bivariate_map(nquantiles  = 10, 
                                                 upperleft   = "#0096EB", # rgb(130,0,80, maxColorValue = 255), 
                                                 upperright  = "#FFE60F", # rgb(255,230,15, maxColorValue = 255),
                                                 bottomleft  = "#e1edf7",
                                                 bottomright = "#820050", # rgb(0,150,235, maxColorValue = 255)  
                                                 ylab        = "CO2 emission (cum)",
                                                 xlab        = "n_weighted_papers")
    
    # --- Adapt it to the data
    data_bivar_n_article_CO2em <- format_data_bivariate_map(data        = GHGemi_mitPubs_country,
                                                            data.x      = "Count_ORO",
                                                            data.y      = "cumulative_co2_including_luc",
                                                            color_table = bivariate_color_scale,
                                                            probs.quant = seq(0,1,0.1))
    
    
    # --- Format the shapefile of the world countries polygon and bind data
    world_shp_data <- format_shp_of_the_world(world_shp    = world_shp,
                                              data_to_bind = data_bivar_n_article_CO2em,
                                              PROJ         = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
    tmp <- world_shp_data |> sf::st_drop_geometry()
    
    data_2_map <- format_data2map(data = world_shp_data,
                                  PROJ = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
  ## ---- PLOT PANEL A
  bivariate_map(data_map   = data_2_map,
                data_world = NULL,
                color      = bivariate_color_scale,
                ylab       = "CO2eq. emissions",
                xlab       = "# mit. paper",
                name       = "main/Figure5_PanelA2")

### -----
  
### ----- PANEL B -----
  
  ## ---- LOAD DATA
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
  
    # --- Adaptation ORO
  
      # - Load data
      adaptPubs <- pred_oro_branch %>%
        mutate(adaptation = ifelse(0.5 <= `oro_branch.Nature - mean_prediction` |
                                   0.5 <= `oro_branch.Societal - mean_prediction`,
                                   1,0))%>%
        dplyr::select(analysis_id, adaptation) %>%
        filter(adaptation == 1) %>%
        # Join with affiliation information
        left_join(uniquerefs %>% dplyr::select(analysis_id, affiliation), by = "analysis_id") %>%
        collect()
      
      # --- Extract the country of the first author for each relevant publications
      adaptPubs_aff <- extract_1stA_affiliation(data         = adaptPubs, 
                                                countries_ls = countries_ls)
      
      # tmp = adaptPubs_aff$oroAff_1stA
      
      # --- Number of publications per country
      adaptPubs_per_country <- adaptPubs_aff$oroAff_1stA |> 
        # Give the same name to countries with varying names (Congo (Republic democratic of) & Republic democratic of)
        dplyr::mutate(country_aff = countrycode(sourcevar   = country_aff,
                                                origin      = "country.name",
                                                destination = "country.name"),
                      iso_code = countrycode(sourcevar   = country_aff,
                                             origin      = "country.name",
                                             destination = "iso3c")) |> 
        dplyr::group_by(country_aff, iso_code) |> 
        dplyr::summarise(Count_ORO = n()) |> 
        rename(Country = country_aff)
    
    # --- Vulnerability data 
    
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
      
    # --- Bind all data together
      
      # - Merge data (mean value per eez)
      vulne_adaPubs_data_eez <- full_join(vulne_country_df, adaptPubs_per_country |> ungroup() |> dplyr::select(-Country), by = "iso_code") |> 
        replace_na(list(Count_ORO = 0)) |> 
        filter(!is.na(vulnerability)) |> 
        ungroup()
      
      # - Create the bivariate color scale
      bivariate_color_scale <- color_bivariate_map(nquantiles  = 10, 
                                                   upperleft   = "#0096EB", # rgb(130,0,80, maxColorValue = 255), 
                                                   upperright  = "#FFE60F", # rgb(255,230,15, maxColorValue = 255),
                                                   bottomleft  = "#e1edf7",
                                                   bottomright = "#820050", # rgb(0,150,235, maxColorValue = 255)  
                                                   ylab        = "vulnerability",
                                                   xlab        = "n_weighted_papers")
      
      
        # - Data per grid cells
        vulne_adaPubs_data_eez_grid <- full_join(vulne_df_per_grid, adaptPubs_per_country |> ungroup() |> dplyr::select(-Country), by = "iso_code") |> 
          tidyr::replace_na(list(Count_ORO = 0))
          # mutate(Country = case_when(is.na(Country) == TRUE ~ TERRITORY1,
          #                            TRUE ~ Country)) 
        
      

      # - Adapt it to the data
      vulne_adaPubs_data_eez_bivar <- format_data_bivariate_map(data        = vulne_adaPubs_data_eez,
                                                                data.x      = "Count_ORO",
                                                                data.y      = "vulnerability",
                                                                color_table = bivariate_color_scale,
                                                                probs.quant = seq(0,1,0.1))
      # - Construct the shapefile
      vulne_adaPubs_data_eez_bivar_shp <- left_join(eez_shp, 
                                                    vulne_adaPubs_data_eez_bivar |>  dplyr::select(-Country, -iso_code, -TERRITORY1),
                                                    by = "MRGID") |> 
        sf::st_transform(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
      
      world_shp_robin <- sf::st_transform(world_shp, crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
      
      data_2_map_adapt_vulne_eez <- format_data2map(data = vulne_adaPubs_data_eez_bivar_shp,
                                                    PROJ = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
      
      bivariate_map(data_map   = data_2_map_adapt_vulne_eez,
                    data_world = world_shp_robin,
                    color      = bivariate_color_scale,
                    ylab       = "Vulnerability",
                    xlab       = "# Adaptation papers",
                    name       = "main/Figure5_PanelB_eez3")
      
      ggplot(vulne_adaPubs_data_eez_bivar_shp) +
        geom_sf(aes(fill = vulnerability)) +
        scale_fill_viridis_c() +
        geom_sf(data = world_shp_robin, color = "black", fill = "grey90", size = 0.1) +
        theme_bw()
      
      ggplot2::ggsave(here::here("figures", "main", "vulnerability.jpeg"), width = 7, height = 4.5, device = "jpeg")
      
      tmp <- sf::st_drop_geometry(vulne_adaPubs_data_eez_bivar_shp)
      
      # - Data to plot color inside the country
      vulne_adaPubs_data_country <- vulne_country_df |> #  adaptPubs_per_country |> ungroup() |> dplyr::select(-Country), by = "iso_code") |> 
        group_by(TERRITORY1, Country, iso_code) |> 
        summarise(vulnerability = mean(vulnerability, na.rm = TRUE)) |> 
        full_join(adaptPubs_per_country |> ungroup() |> dplyr::select(-Country), by = "iso_code") |> 
        replace_na(list(Count_ORO = 0)) |>
        ungroup() |>
        dplyr::select(TERRITORY1, vulnerability, Count_ORO, iso_code) |>
        rename(Country = TERRITORY1) |> 
        replace_na(list(vulnerability = 0)) |> 
        mutate(Country = case_when(is.na(Country) == TRUE ~ countrycode(sourcevar   = iso_code,
                                                                        origin      = "iso3c",
                                                                        destination = "country.name"),
                                   TRUE ~ Country))
    


    # --- Bind data with countries without EEZ
    tmp_sf <- format_shp_of_the_world(world_shp    = world_shp,
                                      data_to_bind = vulne_adaPubs_data_country,
                                      PROJ         = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") |> 
      sf::st_drop_geometry() |> 
      replace_na(list(vulnerability = 0, Count_ORO = 0)) |> 
      rename(Country = NA2_DESCRI)
    
    data_bivar_adapt_vulne <- format_data_bivariate_map(data        = tmp_sf,
                                                        data.x      = "Count_ORO",
                                                        data.y      = "vulnerability",
                                                        color_table = bivariate_color_scale,
                                                        probs.quant = seq(0,1,0.1))
    
    
    # --- Format the shapefile of the world countries polygon and bind data
    world_shp_data_vulne <- format_shp_of_the_world(world_shp    = world_shp,
                                                    data_to_bind = data_bivar_adapt_vulne,
                                                    PROJ         = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
    data_2_map_vulne <- format_data2map(data = world_shp_data_vulne,
                                        PROJ = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
  ## ---- PLOT PANEL A
  bivariate_map(data_map   = data_2_map_vulne,
                data_world = world_shp,
                color      = bivariate_color_scale,
                ylab       = "Vulnerability",
                xlab       = "# ada. paper",
                name       = "main/Figure5_PanelB4")
    

  tmp <- data_2_map_vulne$data |>  sf::st_drop_geometry()
  tmp2 <- world_shp |>  sf::st_drop_geometry()
  

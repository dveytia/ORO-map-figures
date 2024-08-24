#################################################################################################
#                                                                                               #
# FIGURE 5                                                                                      #
# Panel A: Map of mitigation options and CO2 country emisssions (two color scales)              #  
#          (1) Bivariate CS (EEZ's color) ==> CO2 em. per country ~ # ORO pub on BC and MRE)    # 
#          (2) Univariate color scale (countrie's color) ==> # pub mitigations ORO (1st author) #
# Panel B:                                                                                      #
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
  
    # --- Cleaned geoparsed data (see .Rmd script called 0_data-processing-cleaning.Rmd)
    geoparsed_data_clean <- get(load(here::here("data", "geoparsing", "tmp_clean.RData")))

    # --- Other data
    shp_df_matches <- tbl(dbcon, "geoparsed-text_shp_df_matches") 
    shp_id_match <- tbl(dbcon, "shp_df_natural-earth-shapes") |> dplyr::select(shpfile_id, sovereignt, sov_a3, admin, adm0_a3) 
    grid_df <- tbl(dbcon, "grid_df_res2.5") 
    pred_oro_any_mitigation <- tbl(dbcon, "pred_oro_any_mitigation")
    pred_blue_carbon <- tbl(dbcon, "pred_blue_carbon")
    uniquerefs <- tbl(dbcon, "uniquerefs") # metadata on the unique references
    pred_oro_branch <- tbl(dbcon, "pred_oro_branch") # predictions for ORO branch
    pred_relevance <- tbl(dbcon, "pred_relevance") # which articles are relevant to OROs
    
    # --- GHG emissions data per country (cumulative emissions)
    GHGemi_country <- read.csv(file = here::here("data", "external", "ghg-emissions", "owid-co2-data.csv")) |>  
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
      filter(!is.na(cumulative_co2_including_luc))
    
    # --- List of countries
    countries_ls <- read.csv(file = here::here("data", "external", "list_of_countries", "sql-pays.csv"), sep = ";") |>  # Countries names
      dplyr::mutate(country = countrycode(sourcevar   = name_en,
                                          origin      = "country.name",
                                          destination = "country.name"),
                    iso_code = countrycode(sourcevar   = country,
                                           origin      = "country.name",
                                           destination = "iso3c"))  
    
    # --- AMUNRC: Associate Members of United Nations Regional Commissions
    # AMUNRC <- c("American Samoa", "Anguilla", "Aruba", "Bermuda", "British Virgin Islands", "Cayman Islands", "Commonwealth of Northern Marianas",
    #             "Curacao", "French Polynesia", "Guadeloupe", "Guam", "Martinique", "Montserrat", "New Caledonia", "Puerto Rico", "Sint Maarten",
    #             "Turks and Caicos Islands", "United States Virgin Islands")
    
    # --- Category of each country (SIDS, land-locked, coastal)
    # landlocked <- read.csv(file = here::here("data", "external", "special_country_groups", "landlocked-countries-2024.csv"), sep = ",") |> 
    #   dplyr::select(country, LandlockedCountries) |> 
    #   rename(Country    = country, 
    #          group_land = LandlockedCountries) |> 
    #   mutate(iso_code = countrycode(sourcevar   = Country,
    #                                 origin      = "country.name",
    #                                 destination = "iso3c"),
    #          group_land = case_when(group_land == "yes" ~ "Land-locked",
    #                                 TRUE ~ "NA"))
    
    # country_grp <- read.csv(file = here::here("data", "external", "special_country_groups", "special-country-groups.csv"), sep = ",") |> 
    #   dplyr::select(Country, LLDC, SIDS) |> 
    #   mutate(iso_code = countrycode(sourcevar   = Country,
    #                                 origin      = "country.name",
    #                                 destination = "iso3c"),
    #          group_land = case_when(LLDC == "No" & SIDS == "Yes" ~ "SIDS",
    #                                 LLDC == "Yes" & SIDS == "No" ~ "Land-locked",
    #                                 TRUE ~ "Coastal")) |> 
    #   dplyr::select(-LLDC, -SIDS) |> 
    #   rbind(landlocked) |> 
    #   distinct() |> 
    #   mutate(group_land = case_when(Country %in% AMUNRC ~ "AMUNRC",
    #                                 TRUE ~ group_land))
    
    # --- Shapefile of the world
    world_shp <- sf::read_sf(here::here("data", "external", "world_shp")) |>  # shape file of the world
      mutate(NA2_DESCRI = countrycode(sourcevar   = NA2_DESCRI,
                                      origin      = "country.name",
                                      destination = "country.name",
                                      nomatch     = NULL),
             iso_NA2 = countrycode(sourcevar   = NA2_DESCRI,
                                   origin      = "country.name",
                                   destination = "iso3c"))
    
    # --- Shape file of countrie's EEZ
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
      mutate(ISO_SOV1 = case_when(TERRITORY1 == "Cook Islands" ~ "COK",
                                  TERRITORY1 == "Niue" ~ "NIU",
                                  TERRITORY1 == "Sint-Maarten" ~ "SXM",
                                  TRUE ~ ISO_SOV1)) 
      # full_join(country_grp |>  dplyr::select(-Country), by = c("ISO_SOV1" = "iso_code")) 
    
    # area_mrgid <-  sf::st_area(eez_shp)
    # 
    # eez_shp <- eez_shp |> 
    #   mutate(area_mrgid = area_mrgid)
    # 
    tmp <- eez_shp |>  sf::st_drop_geometry()

    
  ## ---- FORMAT DATA: Produce a shapefile of grid cells with the country & administration they belong to.
    
    # - Load the grid and format it into a shapefile
    # grid_sf <- tbl(dbcon, "grid_df_res2.5") |> collect() |> 
    #   sf::st_as_sf(coords = c("LON", "LAT"), crs = 4326)
    # 
    # # - Join with land data
    # sf::sf_use_s2(FALSE)
    # grid_sf_land <- sf::st_join(grid_sf, world_shp) |> 
    #   dplyr::select(grid_df_id, is_land, NA2_DESCRI, iso_NA2)
    # 
    # # - Join with eez data
    # grid_sf_eez <- sf::st_join(grid_sf, eez_shp) |> 
    #   dplyr::select(grid_df_id, is_land, MRGID, TERRITORY1, iso_NA2, Country, iso_code) |> 
    #   distinct(grid_df_id, .keep_all = TRUE)
    # 
    # # - Land + eez data
    # grid_sf_eez_land <- sf::st_join(grid_sf_land, grid_sf_eez |> dplyr::select(-grid_df_id))
    # grid_df_eez_land <- sf::st_drop_geometry(grid_sf_eez_land) |> 
    #   mutate(country_id = case_when(is.na(Country) & !is.na(NA2_DESCRI) ~ NA2_DESCRI,
    #                                 is.na(NA2_DESCRI) & !is.na(Country) ~ Country,
    #                                 is.na(NA2_DESCRI) & is.na(Country)  ~ "High-seas",
    #                                 !is.na(NA2_DESCRI) & !is.na(Country) & Country != NA2_DESCRI ~ "Verif",
    #                                 !is.na(NA2_DESCRI) & !is.na(Country) & Country == NA2_DESCRI ~ Country),
    #          TERRITORY1 = case_when(!is.na(TERRITORY1) ~ TERRITORY1,
    #                                 is.na(TERRITORY1) ~ country_id)) |> 
    #   dplyr::select(grid_df_id, TERRITORY1, country_id) |> 
    #   left_join(grid_df |>  dplyr::select(-area_km, -is_land), by = "grid_df_id", copy = TRUE)
    # 
    # save(grid_df_eez_land, file = here::here("data", "geoparsing", "land_eez_grid_country.RData"))
    # load(here::here("data", "geoparsing", "land_eez_grid_country.RData"))
    
  ## ---- FORMAT DATA: Mitigation data
    
    # --- Select only mitigation data
    mitigation_grid_df <- geoparsed_data_clean |>
      left_join(pred_oro_branch, by = "analysis_id", copy = TRUE) |> 
      left_join(pred_relevance, by = "analysis_id", copy = TRUE) |> 
      filter(0.5 <= relevance_mean) |> 
      mutate(mitigation = ifelse(0.5 <= `oro_branch.Mitigation - mean_prediction`, 1 ,0)) |>  
      filter(mitigation == 1) |> 
      dplyr::select(analysis_id, mitigation, shp_id, grid_df_id, TERRITORY1, country_id) |> 
      mutate(country_id = case_when(TERRITORY1 == "Greenland" ~ "Denmark",
                                    TRUE ~ country_id)) |> 
      distinct(analysis_id, TERRITORY1, country_id)
    
    # --- Group data by country and territory
    mitigation_geop_paper_country_territory <- mitigation_grid_df |> 
      mutate(country_id = case_when(TERRITORY1 == "Niue" ~ "Niue",
                                    TERRITORY1 == "Cook Islands" ~ "Cook Islands",
                                    TRUE ~ country_id)) |> 
      group_by(TERRITORY1, country_id) |> 
      summarise(Count_ORO = n()) |> 
      mutate(iso_code = countrycode(sourcevar   = TERRITORY1,
                                    origin      = "country.name",
                                    destination = "iso3c"))
    
    # --- Group data by country only
    mitigation_geop_paper_country <- mitigation_grid_df |> 
      mutate(country_id = case_when(TERRITORY1 == "Niue" ~ "Niue",
                                    TERRITORY1 == "Cook Islands" ~ "Cook Islands",
                                    TRUE ~ country_id)) |> 
      group_by(country_id) |> 
      summarise(Count_ORO = n()) |> 
      mutate(iso_code = countrycode(sourcevar   = country_id,
                                    origin      = "country.name",
                                    destination = "iso3c"))
    
    
  ## ---- FORMAT DATA: Bivariate color scale
    
    # --- Join mitigation ORO data with GHG emissions data
    GHGemi_mitPubs_country <- mitigation_geop_paper_country_territory |> 
      full_join(GHGemi_country, by = "iso_code") |> 
      mutate(country_id = case_when(is.na(country_id) == TRUE ~ country,
                                    TRUE ~ country_id)) |> 
      group_by(country_id) |> 
      summarise(Count_ORO = sum(Count_ORO, na.rm = TRUE),
                cumulative_co2 = sum(cumulative_co2, na.rm = TRUE),
                cumulative_co2_including_luc = sum(cumulative_co2_including_luc, na.rm = TRUE)) |> 
      ungroup()
    
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
                                                            nquantiles  = 10,
                                                            probs.quant.x = seq(0, 1, 0.1),
                                                            probs.quant.y = seq(0, 1, 0.1)) |> 
      mutate(iso_code = countrycode::countrycode(sourcevar   = country_id,
                                                 origin      = "country.name",
                                                 destination = "iso3c"))
    
    # --- Format the shapefile of the world countries polygon and bind data
    world_shp_data <- format_shp_of_the_world(world_shp    = world_shp,
                                              data_to_bind = data_bivar_n_article_CO2em,
                                              PROJ         = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
    data_2_map <- format_data2map(data = world_shp_data,
                                  PROJ = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
    # --- Format the shapefile of the eez countries polygon and bind data
    eez_shp_data <- eez_shp |>
      # filter(group_land == "SIDS") |>
      left_join(data_bivar_n_article_CO2em, by = c("ISO_SOV1" = "iso_code")) |>
      filter(!is.na(MRGID)) |> 
      sf::st_transform(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
    tmp <- eez_shp_data |> sf::st_drop_geometry()
    
  ## ---- PLOT DATA
  panelA <- bivariate_map(data_map   = data_2_map,
                          eez        = eez_shp_data, 
                          data_world = NULL,
                          color      = bivariate_color_scale,
                          ylab       = "CO2eq. emissions",
                          xlab       = "# mit. paper (GeoP)",
                          name       = "main/final_version/bivar_map_GHGemi_mitPubsGeop")
  
  ## ---- Test for correlation between co2 emissions and mitigation publications (by geoparsing)
  
    # --- Scale values by dividing by their sd and mean center
    x <- scale(data_bivar_n_article_CO2em$Count_ORO, center = TRUE, scale = TRUE)
    y <- scale(data_bivar_n_article_CO2em$cumulative_co2_including_luc, center = TRUE, scale = TRUE)
    
    # --- Test for normality
    shapiro.test(x) # what is the p value? if >0.05, normal
    shapiro.test(y) # same -- report p value
    
    # --- Test for linear relationship
    plot(y, x)
    
    # --- calculate Spearman's correlation coefficienct (not Pearson's coefficient due to the non-normality of the data
    mit_cor <- cor.test(x, y, method = "spearman")
    mit_cor
      
### -----
    
    
### ----- PANEL B -----
  
  ## ---- LOAD DATA (vulnerability)
  # mhvs_rast <- raster::raster(here::here("data", "external", "climate-hazard-maps", "coastal_mhvs_1982-2018.tif"))
  # sst_rast <- raster::raster(here::here("data", "external", "climate-hazard-maps", "coastal_sst_1980-2018.tif"))
  # storms_rast <- raster::raster(here::here("data", "external", "climate-hazard-maps", "coastal_tracks_1980-2018_points.tif"))
  surges_rast <- raster::raster(here::here("data", "external", "climate-hazard-maps", "Coastal_surges_1974-2014_R.tif")) 
  area_rast <- raster::area(surges_rast)
  # surges_area_rast <- surges_rast*area_rast
  
  ## ---- LOAD DATA (population in low elevation coastal zones): https://sedac.ciesin.columbia.edu/data/set/lecz-urban-rural-population-estimates-v1/data-download
  pop_lecz <- readxl::read_excel(here::here("data", "external", "population_lecz", "10mlecz-grumpv1.xls"), sheet = "country_lecz")
  
  ## ---- LOAD DATA (coastline by country): https://sedac.ciesin.columbia.edu/data/set/lecz-urban-rural-population-estimates-v1/data-download
  coastline <- readr::read_csv("data/external/coastline_by_country/countries-by-coastline-2024.csv")
  
  ## ---- FORMAT DATA: Vulneralibility data
  
    # - List of raster
    # rast_ls <- list(mhvs = mhvs_rast, sst = sst_rast, storms = storms_rast, surges = surges_rast)
    # raster::plot(stack(rast_ls))
    # rast_ls <- list(surges = surges_rast, area = area_rast)
    raster::plot(surges_rast)
    raster::plot(area_rast)
    # raster::plot(surges_area_rast)
    
    # - Filter cell in area_rast with a value in surges_rast
    area_rast <- terra::mask(area_rast, surges_rast)
    raster::plot(area_rast)
    
    # - Normalize values between 0 and 1
    # rast_ls01 <- stack(lapply(rast_ls, normalize_rast_values_01))
    # raster::plot(rast_ls01, colNA = "darkblue")
    
    # - Take the mean normalized values of the 4 variables
    # vulne_mean_rast <- calc(rast_ls01, fun = mean)
    # vulne_mean_rast1 <- raster::aggregate(vulne_mean_rast, fact = 4, fun = mean)
    # raster::plot(vulne_mean_rast1, colNA = "darkblue")
    
    # - Shapefile and summarize across eez and find the corresponding country
    sf::sf_use_s2(FALSE)
    vulne_shp <- sf::st_as_sf(stars::st_as_stars(surges_rast)) # vulne_mean_rast1
    area_shp <- sf::st_as_sf(stars::st_as_stars(area_rast)) 
    # surges_area_shp <- sf::st_as_sf(stars::st_as_stars(surges_area_rast)) 
    
    vulne_area_shp <- vulne_shp |> # vulne_shp
      mutate(area   = area_shp$layer) |>
      rename(surges = Coastal_surges_1974.2014_R) 

    vulne_shp_country <- eez_shp |> 
      sf::st_join(vulne_area_shp) 
    
    tmp <- vulne_shp_country |>  sf::st_drop_geometry() |>  filter(!is.na(surges))
    
    
    # vulne_df_per_grid <- sf::st_drop_geometry(vulne_shp_country)
      # rename(vulnerability = layer)
    
    # --- Compute the weighted mean by country (weighted by cells area)
    vulne_mean_weighted_country_df <- vulne_shp_country |> 
      sf::st_drop_geometry() |> 
      # group_by(MRGID, TERRITORY1, Country, iso_code, iso_NA2) |> 
      group_by(Country, iso_code) |>  
      summarise(storm_surges_weighted = weighted.mean(surges, area, na.rm = TRUE)) |> 
      ungroup() |> 
      filter(!is.na(storm_surges_weighted)) 
    
    # vulne_mean_weighted_country_df2 <- vulne_shp_country |> 
    #   sf::st_drop_geometry() |> 
    #   # group_by(MRGID, TERRITORY1, Country, iso_code, iso_NA2) |> 
    #   group_by(Country, iso_code) |>  
    #   summarise(storm_surges = mean(surges, na.rm = TRUE)) |> 
    #   ungroup() |> 
    #   filter(!is.na(storm_surges)) 
    
      # --- Map to check the results
      check_map <- format_shp_of_the_world(world_shp    = world_shp,
                                           data_to_bind = vulne_mean_weighted_country_df,
                                           PROJ         = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") |> 
        dplyr::rename(layer = storm_surges_weighted)
        
      check_map_data <- format_data2map(data = check_map,
                                        PROJ = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
      
      
      tmp <- check_map_data$data |> sf::st_drop_geometry()
      
      univariate_map(data_map          = check_map_data,
                     eez               = NULL,
                     color_scale       = viridis::turbo(10, direction = 1),
                     midpoint          = NULL,
                     second.var        = NULL,
                     # vals_colors_scale = NULL,
                     title_color       = "Exposure (weighted)",
                     title_size        = NULL,
                     show.legend       = TRUE,
                     name              = "main/final_version/country_exposure_weighted_by_cell_area")
      

      
      # --- Compute the weighted mean by mrgid (weighted by cells area)
      vulne_mean_weighted_mrgid_df <- vulne_shp_country |> # vulne_country_df
        sf::st_drop_geometry() |> 
        group_by(MRGID, TERRITORY1, Country, iso_code, iso_NA2) |>
        summarise(storm_surges_weighted = weighted.mean(surges, area, na.rm = TRUE)) |> 
        # summarise(storm_surges = mean(surges, na.rm = TRUE)) |> 
        ungroup() |> 
        filter(!is.na(storm_surges_weighted)) |> 
        mutate(iso_NA2 = case_when(TERRITORY1 %in% c("Sint-Maarten", "Curaçao", "Bonaire", "Sint-Eustatius") ~ "ANT",
                                   TERRITORY1 == "Micronesia" ~ "FSM",
                                   TERRITORY1 == "Aruba" ~ "ANT Aruba",
                                   TERRITORY1 == "Comores" ~ "COM",
                                   iso_code == "KIR" ~ "KIR",
                                   TERRITORY1 == "Galapagos" ~ "ECU",
                                   TRUE ~ iso_NA2))
      
      # --- Country list and their territories
      countries_territories_df <- vulne_mean_weighted_mrgid_df |>
        dplyr::select(TERRITORY1, Country, iso_code, iso_NA2) |>
        distinct() |>
        dplyr::mutate(iso_NA2 = case_when(TERRITORY1 == "Comores" ~ "COM",
                                          TERRITORY1 == "Federated State of Micronesia" ~ "FSM",
                                          iso_code == "KIR" ~ "KIR",
                                          TRUE ~ iso_NA2))
      
    
      # --- Check map
      world_shp_data_mrgid <- format_shp_of_the_world(world_shp    = world_shp,
                                                      data_to_bind = vulne_mean_weighted_mrgid_df,
                                                      PROJ         = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") |> 
        dplyr::rename(layer = storm_surges_weighted)
      
      data_2_map_mrgid <- format_data2map(data = world_shp_data_mrgid,
                                          PROJ = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
      
      eez_shp_data_mrgid <- eez_shp |>
        left_join(vulne_mean_weighted_mrgid_df, by = "MRGID") |> #c("ISO_SOV1" = "iso_code")
        filter(!is.na(storm_surges_weighted)) |> 
        dplyr::rename(layer = storm_surges_weighted) |> 
        sf::st_transform(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
      
      data_2_map_mrgid$data <- eez_shp_data_mrgid

      tmp <- eez_shp_data_mrgid |>  sf::st_drop_geometry()
      
      univariate_map(data_map          = data_2_map_mrgid,
                     eez               = eez_shp_data_mrgid,
                     color_scale       = viridis::turbo(10, direction = 1),
                     midpoint          = NULL,
                     second.var        = NULL,
                     # vals_colors_scale = NULL,
                     title_color       = "Exposure (weighted)",
                     title_size        = NULL,
                     show.legend       = TRUE,
                     name              = "main/final_version/mrgid_exposure_weighted_by_cell_area")
      
      
    
  ## ---- FORMAT DATA: coastline (at the country scale )
  coastline_country <- coastline |> 
    dplyr::mutate(country  = case_when(country == "Micronesia" ~ "Federated State of Micronesia",
                                       country == "Saint Martin" ~ "Collectivity of Saint Martin",
                                       TRUE ~ country),
                  iso_NA2  = countrycode(sourcevar   = country,
                                         origin      = "country.name",
                                         destination = "iso3c")) |> 
    dplyr::left_join(countries_territories_df |> dplyr::select(-iso_code), by = "iso_NA2") |> 
    dplyr::mutate(Country  = case_when(country == "Macau" ~ "China",
                                       country == "Hong Kong" ~ "China",
                                       country == "Aruba" ~ "Netherlands",
                                       country == "Federated State of Micronesia" ~"Micronesia (Federated States of)",
                                       TRUE ~ Country)) |> 
    dplyr::distinct() |> 
    dplyr::group_by(Country) |> 
    dplyr::summarise(Coastline = sum(Coastline, na.rm = TRUE)) |> 
    dplyr::mutate(iso_code = countrycode(sourcevar   = Country,
                                         origin      = "country.name",
                                         destination = "iso3c"))
    
    
  ## ---- FORMAT DATA: population data
  pop_country <- pop_lecz |>
    dplyr::select(CONTINENT, ISO3V10, Country, ProjectRegion, G00PT_lecz, G00PT_ctry, Landlocked) |>
    dplyr::mutate(G00PT_lecz = dplyr::case_when(G00PT_lecz == -9999 ~ 0,
                                                TRUE ~ G00PT_lecz),
                  G00PT_ctry = dplyr::case_when(G00PT_ctry == -9999 ~ 0,
                                                TRUE ~ G00PT_ctry),
                  Country  = case_when(Country == "Morocco (includes Western Sahara)" ~ "Morocco",
                                       Country == "Hong Kong" ~ "China",
                                       Country == "Macao" ~ "China",
                                       Country == "Serbia and Montenegro" ~ "Montenegro",
                                       TRUE ~ Country),
                  iso_NA2 = countrycode(sourcevar   = Country,
                                        origin      = "country.name",
                                        destination = "iso3c"),
                  iso_NA2 = case_when(ISO3V10 == "ANT" ~ "ANT",
                                      ISO3V10 == "ABW" ~ "ANT",
                                      TRUE ~ iso_NA2)) |> 
    dplyr::select(-ISO3V10) |> 
    rename(country_pop = Country) |> 
    dplyr::left_join(countries_territories_df |> dplyr::select(-iso_code), by = "iso_NA2") |> 
    dplyr::distinct() |> 
    dplyr::mutate(Country = dplyr::case_when(is.na(Country) == TRUE ~ country_pop,
                                             TRUE ~ Country)) |> 
    dplyr::group_by(Country) |> 
    dplyr::summarise(population = sum(G00PT_lecz, na.rm = TRUE),
                     pop_total  = sum(G00PT_ctry, na.rm = TRUE),
                     perc_in_lecz = (population/pop_total)*100,
                     landlocked = sum(Landlocked)) |> 
    dplyr::mutate(iso_code = countrycode(sourcevar   = Country,
                                         origin      = "country.name",
                                         destination = "iso3c"))
  
  pop_mrgid <- pop_lecz |>
    dplyr::select(CONTINENT, ISO3V10, Country, ProjectRegion, G00PT_lecz, G00PT_ctry, Landlocked) |>
    dplyr::filter(Landlocked == 0) |> 
    dplyr::mutate(G00PT_lecz = dplyr::case_when(G00PT_lecz == -9999 ~ 0,
                                                TRUE ~ G00PT_lecz),
                  G00PT_ctry = dplyr::case_when(G00PT_ctry == -9999 ~ 0,
                                                TRUE ~ G00PT_ctry),
                  Country  = case_when(Country == "Morocco (includes Western Sahara)" ~ "Morocco",
                                       Country == "Hong Kong" ~ "China",
                                       Country == "Macao" ~ "China",
                                       Country == "Serbia and Montenegro" ~ "Montenegro",
                                       TRUE ~ Country),
                  iso_NA2 = countrycode(sourcevar   = Country,
                                        origin      = "country.name",
                                        destination = "iso3c"),
                  iso_NA2 = case_when(ISO3V10 == "ANT" & Country == "Netherland Antilles" ~ "ANT",
                                      ISO3V10 == "ABW" ~ "ANT Aruba",
                                      TRUE ~ iso_NA2)) |> 
    dplyr::select(-ISO3V10) |> 
    rename(territory_pop = Country) |> 
    dplyr::left_join(countries_territories_df |> dplyr::select(-iso_code, -TERRITORY1), by = "iso_NA2") |> 
    dplyr::distinct() |> 
    rename(population = G00PT_lecz, pop_total = G00PT_ctry) |> 
    dplyr::mutate(perc_in_lecz = (population/pop_total)*100)
  
  
 
  ## ---- FORMAR DATA: bind data together
  exposure_country <- pop_country |> 
    dplyr::full_join(vulne_mean_weighted_country_df |> # 
                       dplyr::select(-Country), # vulne_country_df
                       # dplyr::select(MRGID, TERRITORY1, iso_code, iso_NA2, storm_surges) |> 
                       # dplyr::select(iso_code, storm_surges) |>
                       # dplyr::mutate(iso_NA2 = case_when(TERRITORY1 == "Comores" ~ "COM",
                       #                                   TERRITORY1 == "Micronesia" ~ "FSM",
                       #                                   TERRITORY1 == "Line Group" ~ "KIR",
                       #                                   TERRITORY1 == "Gilbert Islands" ~ "KIR",
                       #                                   TRUE ~ iso_NA2)), 
                     by = "iso_code") |> 
    # dplyr::full_join(coastline_country |> dplyr::select(-Country), by = "iso_code") |> 
    dplyr::mutate(exposure = storm_surges_weighted*population,
                  # exposure_norm = (storm_surges_weighted*population)/Coastline,
                  exposure_perc = storm_surges_weighted*perc_in_lecz) |> 
    dplyr::filter(!is.na(Country))
  
  tmp <- exposure |>  filter(landlocked == 0 & is.na(iso_NA2) == TRUE)  
  
  exposure_mrgid <- pop_mrgid |> 
    dplyr::full_join(vulne_mean_weighted_mrgid_df |> # vulne_mean_weighted_country_df
                       dplyr::select(-Country), 
                     by = "iso_NA2") |> 
    # dplyr::full_join(coastline_country |> dplyr::select(-Country), by = "iso_code") |> 
    dplyr::mutate(exposure = storm_surges_weighted*population,
                  # exposure_norm = (storm_surges_weighted*population)/Coastline,
                  exposure_perc = storm_surges_weighted*perc_in_lecz) |> 
    dplyr::filter(!is.na(Country))
  
  
    # --- Check map
    world_shp_data_mrgid_expo <- format_shp_of_the_world(world_shp    = world_shp,
                                                         data_to_bind = exposure_mrgid,
                                                         PROJ         = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") |> 
      dplyr::rename(layer = exposure_perc)
    
    data_2_map_mrgid_expo <- format_data2map(data = world_shp_data_mrgid_expo,
                                             PROJ = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
    eez_shp_data_mrgid_expo <- eez_shp |>
      left_join(exposure_mrgid, by = "MRGID") |> #c("ISO_SOV1" = "iso_code")
      # filter(!is.na(storm_surges_weighted)) |> 
      dplyr::rename(layer = exposure_perc) |> 
      sf::st_transform(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
    data_2_map_mrgid_expo$data <- eez_shp_data_mrgid_expo
    
    tmp <- eez_shp_data_mrgid_expo |>  sf::st_drop_geometry()
    
    univariate_map(data_map          = data_2_map_mrgid_expo,
                   eez               = eez_shp_data_mrgid_expo,
                   color_scale       = viridis::turbo(10, direction = 1),
                   midpoint          = NULL,
                   second.var        = NULL,
                   # vals_colors_scale = NULL,
                   title_color       = "Exposure (weighted)",
                   title_size        = NULL,
                   show.legend       = TRUE,
                   name              = "main/final_version/mrgid_exposure_weighted_by_cell_area_percLECZ")
  
  
    
  # exposure1 <- exposure |>  arrange(desc(exposure))
  # p1 <- ggplot(data = exposure1[1:25,], mapping = aes(x = reorder(Country, -exposure), y = exposure)) +
  #   geom_col() +
  #   labs(x = NULL, y = "Exposure") +
  #   theme_bw() +
  #   theme(axis.text.x = element_text(size = 10, angle = 60, hjust = 1))
  # 
  # exposure2 <- exposure |>  arrange(desc(exposure_norm))
  # p2 <- ggplot(data = exposure2[1:25,] |> arrange(desc(exposure_norm)), mapping = aes(x = reorder(Country, -exposure_norm), y = exposure_norm)) +
  #   geom_col() +
  #   labs(x = NULL, y = "Exposure (normalized)") +
  #   theme_bw() +
  #   theme(axis.text.x = element_text(size = 10, angle = 60, hjust = 1))
  # 
  # exposure3 <- exposure |>  arrange(desc(exposure_perc))
  # p3 <- ggplot(data = exposure3[1:25,], mapping = aes(x = reorder(Country, -exposure_perc), y = exposure_perc)) +
  #   geom_col() +
  #   labs(x = NULL, y = "Exposure (% pop in LECZ)") +
  #   theme_bw() +
  #   theme(axis.text.x = element_text(size = 10, angle = 60, hjust = 1))
  # 
  # cowplot::ggdraw() +  
  #   cowplot::draw_plot(p1, x = 0, y = 0.66, width = 1.0, height = 0.33) +
  #   cowplot::draw_plot(p2, x = 0, y = 0.33, width = 1.0, height = 0.33) +
  #   cowplot::draw_plot(p3, x = 0, y = 0.00, width = 1.0, height = 0.33)
  
  # exposure3 <- pop_lecz |> 
  #   dplyr::select(CONTINENT, ISO3V10, Country, ProjectRegion, G00PT_lecz, Landlocked) |> 
  #   dplyr::mutate(G00PT_lecz = dplyr::case_when(G00PT_lecz == -9999 ~ 0,
  #                                               TRUE ~ G00PT_lecz),
  #                 Country  = case_when(Country == "Morocco (includes Western Sahara)" ~ "Morocco",
  #                                      TRUE ~ Country),
  #                 iso_code = countrycode(sourcevar   = Country,
  #                                        origin      = "country.name",
  #                                        destination = "iso3c")) |> 
  #   dplyr::full_join(vulne_country_df |> dplyr::select(MRGID, TERRITORY1, iso_code, iso_NA2, storm_surges), by = c("Country" = "TERRITORY1"))
  

  ## ---- FORMAT DATA: Adaptation data
  
    # --- Select only adaptation data
    adaptation_grid_df <- geoparsed_data_clean |>
      left_join(pred_oro_branch, by = "analysis_id", copy = TRUE) |> 
      left_join(pred_relevance, by = "analysis_id", copy = TRUE) |> 
      filter(0.5 <= relevance_mean) |> 
      mutate(adaptation = ifelse(0.5 <= `oro_branch.Nature - mean_prediction` |
                                 0.5 <= `oro_branch.Societal - mean_prediction`,
                                 1,0))%>%
      filter(adaptation == 1) %>%
      dplyr::select(analysis_id, adaptation, shp_id, grid_df_id, TERRITORY1, country_id) |> 
      mutate(country_id = case_when(TERRITORY1 == "Greenland" ~ "Denmark",
                                    TRUE ~ country_id)) |> 
      distinct(analysis_id, TERRITORY1, country_id)
  
    # --- Group data by country and territory
    adaptation_geop_paper_country <- adaptation_grid_df |> 
      mutate(country_id = case_when(TERRITORY1 == "Niue" ~ "Niue",
                                    TERRITORY1 == "Cook Islands" ~ "Cook Islands",
                                    TRUE ~ country_id)) |> 
      left_join(countries_territories_df, by = "TERRITORY1") |> 
      group_by(country_id, iso_code) |>
      summarise(Count_ORO = n())
      # mutate(iso_code = countrycode(sourcevar   = country_id, #TERRITORY1
      #                               origin      = "country.name",
      #                               destination = "iso3c"))
      # 
    
    adaptation_geop_paper_territory <- adaptation_grid_df |> 
      mutate(country_id = case_when(TERRITORY1 == "Niue" ~ "Niue",
                                    TERRITORY1 == "Cook Islands" ~ "Cook Islands",
                                    TRUE ~ country_id),
             TERRITORY1 = case_when(TERRITORY1 %in% c("Alaska", "Jarvis Island", "Howland and Baker Islands", "Hawaii") ~ "United States",
                                    TERRITORY1 == "Curaçao" ~ "Netherlands Antilles",
                                    TERRITORY1 == "Azores" ~ "Portugal",
                                    TRUE ~ TERRITORY1)) |> 
      left_join(countries_territories_df, by = "TERRITORY1") |> 
      mutate(iso_NA2 = case_when(TERRITORY1 == "Bosnia & Herzegovina" ~ "BIH",
                                 TERRITORY1 == "Mauritius" ~ "MUS",
                                 TERRITORY1 == "Côte d’Ivoire" ~ "CIV",
                                 TERRITORY1 == "Easter Island" ~ "CHL",
                                 TERRITORY1 == "Galapagos" ~ "ECU",
                                 TERRITORY1 == "Islas San Félix and San Ambrosio" ~ "CHL",
                                 TERRITORY1 == "Matthew and Hunter Islands" ~ "FRA",
                                 TERRITORY1 == "Myanmar (Burma)" ~ "MMR",
                                 TERRITORY1 == "Phoenix Group" ~ "KIR",
                                 TERRITORY1 == "Svalbard & Jan Mayen" ~ "SJM",
                                 TERRITORY1 == "South Georgia and the South Sandwich Islands" ~ "GBR",
                                 TRUE ~ iso_NA2)) |> 
      group_by(iso_NA2, TERRITORY1, country_id) |>
      summarise(Count_ORO = n()) |> 
      filter(!is.na(iso_NA2)) # remove landlocked countries

      # mutate(iso_code = countrycode(sourcevar   = TERRITORY1, #TERRITORY1
      #                               origin      = "country.name",
      #                               destination = "iso3c"))
    
    tmp <- pop_lecz |>  
      dplyr::select(ISO3V10, Country, Landlocked) 

  ## ---- FORMAT DATA: Bivariate color scale
    
    # --- Create the bivariate color scale
    bivariate_color_scale <- color_bivariate_map(nquantiles  = 10, 
                                                 upperleft   = "#0096EB", # rgb(130,0,80, maxColorValue = 255), 
                                                 upperright  = "#FFE60F", # rgb(255,230,15, maxColorValue = 255),
                                                 bottomleft  = "#e1edf7",
                                                 bottomright = "#820050", # rgb(0,150,235, maxColorValue = 255)  
                                                 ylab        = "CO2 emission (cum)",
                                                 xlab        = "n_weighted_papers")
  
    # --- Join adaptation ORO data with GHG emissions data
    vulne_adaPubs_country <- adaptation_geop_paper_country |> 
      full_join(exposure_country, by = "iso_code") |> # vulne_country_df
      mutate(country_id = case_when(is.na(country_id) == TRUE ~ Country,
                                    TRUE ~ country_id),
             Count_ORO = case_when(is.na(Count_ORO) == TRUE ~ 0,
                                   TRUE ~ Count_ORO)) |> 
      ungroup()
    
    vulne_adaPubs_mrgid <- adaptation_geop_paper_territory |> 
      full_join(exposure_mrgid |>  dplyr::select(Landlocked, iso_NA2, MRGID, storm_surges_weighted, exposure, exposure_perc), by = "iso_NA2") |> 
      mutate(Count_ORO = case_when(is.na(Count_ORO) == TRUE ~ 0,
                                   TRUE ~ Count_ORO),
             exposure_perc = case_when(is.nan(exposure_perc) == TRUE ~ 0,
                                       TRUE ~ exposure_perc)) |> 
      rename(iso_code = iso_NA2) |> 
      ungroup()

      # group_by(country_id) |> 
      # summarise(vulnerability = mean(vulnerability, na.rm = TRUE),
      #           Count_ORO = sum(Count_ORO, na.rm = TRUE)) |> 
      # ungroup()
    

    # --- Adapt it to the data
    data_bivar_n_article_expo_country <- format_data_bivariate_map(data        = vulne_adaPubs_country,
                                                                   data.x      = "Count_ORO",
                                                                   data.y      = "exposure_perc", # "vulnerability",
                                                                   color_table = bivariate_color_scale,
                                                                   nquantiles  = 10) 
    
    data_bivar_n_article_expo_mrgid <- format_data_bivariate_map(data        = vulne_adaPubs_mrgid,
                                                                 data.x      = "Count_ORO",
                                                                 data.y      = "exposure_perc", # "vulnerability",
                                                                 color_table = bivariate_color_scale,
                                                                 nquantiles  = 10)
    
                                                            # probs.quant = seq(0,1,0.1)) |> 
      # mutate(iso_code = countrycode::countrycode(sourcevar   = country_id,
      #                                            origin      = "country.name",
      #                                            destination = "iso3c"))
    
    # data_bivar_n_article_expo_norm <- format_data_bivariate_map(data        = vulne_adaPubs_country,
    #                                                             data.x      = "Count_ORO",
    #                                                             data.y      = "exposure_norm", # "vulnerability",
    #                                                             color_table = bivariate_color_scale,
    #                                                             nquantiles  = 10) 
    # 
    # data_bivar_n_article_expo_perc <- format_data_bivariate_map(data        = vulne_adaPubs_country,
    #                                                             data.x      = "Count_ORO",
    #                                                             data.y      = "exposure_perc", # "vulnerability",
    #                                                             color_table = bivariate_color_scale,
    #                                                             nquantiles  = 10) 
      
    
    # --- Format the shapefile of the world countries polygon and bind data
    world_shp_data_country <- format_shp_of_the_world(world_shp    = world_shp,
                                                      data_to_bind = data_bivar_n_article_expo_country,
                                                      PROJ         = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
    world_shp_data_mrgid <- format_shp_of_the_world(world_shp    = world_shp,
                                                    data_to_bind = data_bivar_n_article_expo_mrgid,
                                                    PROJ         = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
    # world_shp_data_norm <- format_shp_of_the_world(world_shp    = world_shp,
    #                                                data_to_bind = data_bivar_n_article_expo_norm,
    #                                                PROJ         = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    # 
    # world_shp_data_perc <- format_shp_of_the_world(world_shp    = world_shp,
    #                                                data_to_bind = data_bivar_n_article_expo_perc,
    #                                                PROJ         = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
    data_2_map_country <- format_data2map(data = world_shp_data_country,
                                          PROJ = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
    data_2_map_mrgid <- format_data2map(data = world_shp_data_mrgid,
                                        PROJ = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
    # data_2_map_norm <- format_data2map(data = world_shp_data_norm,
    #                                    PROJ = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    # 
    # data_2_map_perc <- format_data2map(data = world_shp_data_perc,
    #                                    PROJ = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
    # --- Format the shapefile of the eez countries polygon and bind data
    eez_shp_country <- eez_shp |>
      # filter(group_land == "SIDS") |>
      # left_join(data_bivar_n_article_vulne, by = c("ISO_SOV1" = "iso_code")) |>
      left_join(data_bivar_n_article_expo_country, by = "iso_code") |>
      filter(!is.na(MRGID)) |> 
      sf::st_transform(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
    eez_shp_mrgid <- eez_shp |>
      # filter(group_land == "SIDS") |>
      # left_join(data_bivar_n_article_vulne, by = c("ISO_SOV1" = "iso_code")) |>
      left_join(data_bivar_n_article_expo_mrgid, by = c("ISO_SOV1" = "iso_code")) |>
      # filter(!is.na(MRGID)) |> 
      sf::st_transform(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
    test <- eez_shp_mrgid |>  sf::st_drop_geometry()
    
    # eez_shp_SIDS_norm <- eez_shp |>
    #   # filter(group_land == "SIDS") |>
    #   # left_join(data_bivar_n_article_vulne, by = c("ISO_SOV1" = "iso_code")) |>
    #   left_join(data_bivar_n_article_expo_norm, by = "iso_code") |>
    #   filter(!is.na(MRGID)) |> 
    #   sf::st_transform(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    # 
    # eez_shp_SIDS_perc <- eez_shp |>
    #   # filter(group_land == "SIDS") |>
    #   # left_join(data_bivar_n_article_vulne, by = c("ISO_SOV1" = "iso_code")) |>
    #   left_join(data_bivar_n_article_expo_perc, by = "iso_code") |>
    #   filter(!is.na(MRGID)) |> 
    #   sf::st_transform(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
  
  ## ---- PLOT DATA
  tmp <- bivariate_map(data_map   = data_2_map_country,
                       eez        = eez_shp_country, 
                       data_world = NULL,
                       color      = bivariate_color_scale,
                       ylab       = "Exposure",
                       xlab       = "# ada. paper (GeoP)",
                       name       = "main/final_version/AdaPaperGeop_expo_country2")  
  
  test <- bivariate_map(data_map   = data_2_map_mrgid,
                        eez        = eez_shp_mrgid, 
                        data_world = NULL,
                        color      = bivariate_color_scale,
                        ylab       = "Exposure",
                        xlab       = "# ada. paper (GeoP)",
                        name       = "main/final_version/AdaPaperGeop_expo_territory")  
  
  # bivariate_map(data_map   = data_2_map_perc,
  #               eez        = eez_shp_SIDS_perc, 
  #               data_world = NULL,
  #               color      = bivariate_color_scale,
  #               ylab       = "Exposure",
  #               xlab       = "# ada. paper",
  #               name       = "main/TEST3")  
  #   
  # panelB <- bivariate_map(data_map   = data_2_map,
  #                         eez        = eez_shp_SIDS, 
  #                         data_world = NULL,
  #                         color      = bivariate_color_scale,
  #                         ylab       = "Exposure",
  #                         xlab       = "# ada. paper",
  #                         name       = "main/bivar_map_exposure_adaPubs")
  
  ## ---- Test for correlation between CC exposure and adaptation publications (by geoparsing)
  
    # --- Scale values by dividing by their sd and mean center
    x2.0 <- scale(data_bivar_n_article_expo_country$Count_ORO, center = TRUE, scale = TRUE)
    x2.1 <- scale(data_bivar_n_article_expo_mrgid$Count_ORO, center = TRUE, scale = TRUE)
    
    y2.0 <- scale(data_bivar_n_article_expo_country$exposure_perc, center = TRUE, scale = TRUE)
    y2.1 <- scale(data_bivar_n_article_expo_mrgid$exposure_perc, center = TRUE, scale = TRUE)
    
    # y2.3 <- scale(data_bivar_n_article_expo_perc$exposure_perc, center = TRUE, scale = TRUE)
    
    # --- Test for normality
    shapiro.test(x2.0) # what is the p value? if >0.05, normal
    shapiro.test(x2.1) # same -- report p value
    shapiro.test(y2.0) # same -- report p value
    shapiro.test(y2.1) # same -- report p value
    
    # --- Test for linear relationship
    plot(y2.1, x2)
    plot(y2.2, x2)
    plot(y2.3, x2)
    
    # --- calculate Spearman's correlation coefficienct (not Pearson's coefficient due to the non-normality of the data)
    ada_cor2.1 <- cor.test(x2.0, y2.0, method = "spearman") ; ada_cor2.1
    ada_cor2.2 <- cor.test(x2.1, y2.1, method = "spearman") ; ada_cor2.2
    ada_cor2.3 <- cor.test(x2, y2.3, method = "spearman") ; ada_cor2.3
  
  
    
### -----  
  

### ----- Arrange the figure -----

figure5 <- cowplot::ggdraw() +  
  cowplot::draw_plot(panelA, x = 0.0, y = 0.56, width = 1.0, height = 0.5) +
  cowplot::draw_plot(panelB, x = 0.0, y = 0.20, width = 1.0, height = 0.5) +
  cowplot::draw_plot_label(label = c("(a)", "(b)"),
                           size = 15,
                           x = c(0, 0),
                           y = c(0.97, 0.60)) 

ggplot2::ggsave(plot = figure5, here::here("figures", "main", "maps_bivar_MitiEmi_AdaExpo_2.pdf"), width = 15, height = 15, device = "pdf")


### -----

  
  
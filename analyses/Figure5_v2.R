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
    AMUNRC <- c("American Samoa", "Anguilla", "Aruba", "Bermuda", "British Virgin Islands", "Cayman Islands", "Commonwealth of Northern Marianas",
                "Curacao", "French Polynesia", "Guadeloupe", "Guam", "Martinique", "Montserrat", "New Caledonia", "Puerto Rico", "Sint Maarten",
                "Turks and Caicos Islands", "United States Virgin Islands")
    
    # --- Category of each country (SIDS, land-locked, coastal)
    landlocked <- read.csv(file = here::here("data", "external", "special_country_groups", "landlocked-countries-2024.csv"), sep = ",") |> 
      dplyr::select(country, LandlockedCountries) |> 
      rename(Country    = country, 
             group_land = LandlockedCountries) |> 
      mutate(iso_code = countrycode(sourcevar   = Country,
                                    origin      = "country.name",
                                    destination = "iso3c"),
             group_land = case_when(group_land == "yes" ~ "Land-locked",
                                    TRUE ~ "NA"))
    
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
                                  TRUE ~ ISO_SOV1)) |> 
      full_join(country_grp |>  dplyr::select(-Country), by = c("ISO_SOV1" = "iso_code"))

    
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
    eez_shp_SIDS <- eez_shp |>
      filter(group_land == "SIDS") |>
      left_join(data_bivar_n_article_CO2em, by = c("ISO_SOV1" = "iso_code")) |>
      filter(!is.na(MRGID)) |> 
      sf::st_transform(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
    
  ## ---- PLOT DATA
  panelA <- bivariate_map(data_map   = data_2_map,
                          eez        = eez_shp_SIDS, 
                          data_world = NULL,
                          color      = bivariate_color_scale,
                          ylab       = "CO2eq. emissions",
                          xlab       = "# mit. paper",
                          name       = "main/bivar_map_GHGemi_mitPubs")

    
      
### -----
    
    
### ----- PANEL B -----
  
  ## ---- LOAD DATA (vulnerability)
  mhvs_rast <- raster::raster(here::here("data", "external", "climate-hazard-maps", "coastal_mhvs_1982-2018.tif"))
  sst_rast <- raster::raster(here::here("data", "external", "climate-hazard-maps", "coastal_sst_1980-2018.tif"))
  storms_rast <- raster::raster(here::here("data", "external", "climate-hazard-maps", "coastal_tracks_1980-2018_points.tif"))
  surges_rast <- raster::raster(here::here("data", "external", "climate-hazard-maps", "Coastal_surges_1974-2014_R.tif"))
  
  ## ---- FORMAT DATA: Vulneralibility data
  
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
      group_by(MRGID, TERRITORY1, Country, iso_code, iso_NA2) |> 
      summarise(vulnerability = mean(layer, na.rm = TRUE)) |> 
      ungroup()

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
    adaptation_geop_paper_country_territory <- adaptation_grid_df |> 
      mutate(country_id = case_when(TERRITORY1 == "Niue" ~ "Niue",
                                    TERRITORY1 == "Cook Islands" ~ "Cook Islands",
                                    TRUE ~ country_id)) |> 
      group_by(TERRITORY1, country_id) |> 
      summarise(Count_ORO = n()) |> 
      mutate(iso_code = countrycode(sourcevar   = TERRITORY1,
                                    origin      = "country.name",
                                    destination = "iso3c"))
    
  ## ---- FORMAT DATA: Bivariate color scale
  
    # --- Join adaptation ORO data with GHG emissions data
    vulne_adaPubs_country <- adaptation_geop_paper_country_territory |> 
      full_join(vulne_country_df, by = "iso_code") |> 
      mutate(country_id = case_when(is.na(country_id) == TRUE ~ Country,
                                    TRUE ~ country_id)) |> 
      group_by(country_id) |> 
      summarise(vulnerability = mean(vulnerability, na.rm = TRUE),
                Count_ORO = sum(Count_ORO, na.rm = TRUE)) |> 
      ungroup()
    

    # --- Adapt it to the data
    data_bivar_n_article_vulne <- format_data_bivariate_map(data        = vulne_adaPubs_country,
                                                            data.x      = "Count_ORO",
                                                            data.y      = "vulnerability",
                                                            color_table = bivariate_color_scale,
                                                            nquantiles  = 10) |> 
                                                            # probs.quant = seq(0,1,0.1)) |> 
      mutate(iso_code = countrycode::countrycode(sourcevar   = country_id,
                                                 origin      = "country.name",
                                                 destination = "iso3c"))
    
    # --- Format the shapefile of the world countries polygon and bind data
    world_shp_data <- format_shp_of_the_world(world_shp    = world_shp,
                                              data_to_bind = data_bivar_n_article_vulne,
                                              PROJ         = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
    data_2_map <- format_data2map(data = world_shp_data,
                                  PROJ = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
    # --- Format the shapefile of the eez countries polygon and bind data
    eez_shp_SIDS <- eez_shp |>
      filter(group_land == "SIDS") |>
      left_join(data_bivar_n_article_vulne, by = c("ISO_SOV1" = "iso_code")) |>
      filter(!is.na(MRGID)) |> 
      sf::st_transform(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
  
  ## ---- PLOT DATA
  panelB <- bivariate_map(data_map   = data_2_map,
                          eez        = eez_shp_SIDS, 
                          data_world = NULL,
                          color      = bivariate_color_scale,
                          ylab       = "Exposure",
                          xlab       = "# ada. paper",
                          name       = "main/bivar_map_exposure_adaPubs")
  
  
    
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

  
  
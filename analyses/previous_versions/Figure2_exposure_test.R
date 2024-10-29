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
    
    # --- Country list and their respective territories
    countries_territories_df <- eez_shp |>
      sf::st_drop_geometry() |> 
      dplyr::select(MRGID, TERRITORY1, Country, iso_code, iso_NA2) |>
      distinct() |>
      dplyr::mutate(iso_NA2 = case_when(TERRITORY1 == "Comores" ~ "COM",
                                        TERRITORY1 == "Federated State of Micronesia" ~ "FSM",
                                        iso_code == "KIR" ~ "KIR",
                                        Country == "Netherlands" & TERRITORY1 != "Netherlands" ~ "ANT",
                                        is.na(iso_NA2) == TRUE ~ iso_code,
                                        TRUE ~ iso_NA2))
    
    
  ## ---- LOAD DATA (vulnerability)
  surges_rast <- raster::raster(here::here("data", "external", "climate-hazard-maps", "Coastal_surges_1974-2014_R.tif")) 
  area_rast <- raster::area(surges_rast)

  ## ---- LOAD DATA (population in low elevation coastal zones): https://sedac.ciesin.columbia.edu/data/set/lecz-urban-rural-population-estimates-v1/data-download
  pop_lecz <- readxl::read_excel(here::here("data", "external", "population_lecz", "10mlecz-grumpv1.xls"), sheet = "country_lecz")
  
  ## ---- LOAD DATA (coastline by country): https://sedac.ciesin.columbia.edu/data/set/lecz-urban-rural-population-estimates-v1/data-download
  coastline <- readr::read_csv("data/external/coastline_by_country/countries-by-coastline-2024.csv")
  
  ## ---- FORMAT DATA: Vulneralibility data
    
    # - Filter cell in area_rast with a value in surges_rast
    area_rast <- terra::mask(area_rast, surges_rast)
    raster::plot(area_rast)
    
    # - Shapefile and summarize across eez and find the corresponding country
    sf::sf_use_s2(FALSE)
    vulne_shp <- sf::st_as_sf(stars::st_as_stars(surges_rast)) # vulne_mean_rast1
    area_shp <- sf::st_as_sf(stars::st_as_stars(area_rast)) 

    # - Join with eez data
    vulne_area_shp <- vulne_shp |> # vulne_shp
      mutate(area   = area_shp$layer) |>
      rename(surges = Coastal_surges_1974.2014_R) 
    
    vulne_shp_country <- eez_shp |> 
      sf::st_join(vulne_area_shp) 

    # - Compute the weighted mean by mrgid (weighted by cells area)
    vulne_mean_weighted_mrgid_df <- vulne_shp_country |> # vulne_country_df
      sf::st_drop_geometry() |> 
      group_by(MRGID, TERRITORY1, Country, iso_code, iso_NA2) |>
      summarise(storm_surges_weighted = weighted.mean(surges, area, na.rm = TRUE)) |> 
      ungroup() |> 
      filter(!is.na(storm_surges_weighted)) |> 
      dplyr::select(MRGID, storm_surges_weighted) |> 
      right_join(countries_territories_df, by = "MRGID")
    
      # - Check map -----
      eez_shp_data_mrgid <- eez_shp |>
        left_join(vulne_mean_weighted_mrgid_df, by = "MRGID") |> #c("ISO_SOV1" = "iso_code")
        filter(!is.na(storm_surges_weighted)) |> 
        dplyr::rename(layer = storm_surges_weighted) |> 
        sf::st_transform(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
      
      data_2_map_mrgid <- format_data2map(data = eez_shp_data_mrgid,
                                          PROJ = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
      
      tmp <- data_2_map_mrgid$data |>  sf::st_drop_geometry()
      
      map <- univariate_map(data_map          = data_2_map_mrgid,
                            eez               = NULL,
                            color_scale       = viridis::turbo(10, direction = 1),
                            midpoint          = NULL,
                            second.var        = NULL,
                            # vals_colors_scale = NULL,
                            title_color       = "Exposure (weighted)",
                            title_size        = NULL,
                            show.legend       = TRUE,
                            name              = "main/final_version/mrgid_exposure_weighted_by_cell_area2")
      # -----
  
  ## ---- FORMAT DATA: population data
      
    # --- Population in LECZ at the territory scale
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
                                         Country == "Aruba" ~ "Netherland Antilles",
                                         TRUE ~ Country),
                    iso_NA2 = countrycode(sourcevar   = Country,
                                          origin      = "country.name",
                                          destination = "iso3c"), 
                    iso_NA2 = case_when(Country == "Netherland Antilles" ~ "ANT",
                                        TRUE ~ iso_NA2)) |>
      dplyr::select(-ISO3V10) |> 
      group_by(Country, iso_NA2) |> 
      summarise(G00PT_lecz = sum(G00PT_lecz, na.rm = TRUE),
                G00PT_ctry = sum(G00PT_ctry, na.rm = TRUE),
                Landlocked = max(Landlocked)) |> 
      rename(population = G00PT_lecz, pop_total = G00PT_ctry) |> 
      ungroup() |> 
      dplyr::select(-Country) |> 
      dplyr::right_join(countries_territories_df, by = "iso_NA2") |> 
      mutate(population = case_when(is.na(population) == TRUE ~ 0,
                                    TRUE ~ population),
             pop_total = case_when(is.na(pop_total) == TRUE ~ 0,
                                   TRUE ~ pop_total),
             perc_in_lecz = (population/pop_total)*100) |> 
      mutate(perc_in_lecz = case_when(is.nan(perc_in_lecz) == TRUE ~ 0,
                                      TRUE ~ perc_in_lecz))
       
      
      # --- Check map -----
      eez_shp_data_mrgid_pop <- eez_shp |>
        left_join(pop_mrgid, by = "MRGID") |> #c("ISO_SOV1" = "iso_code")
        sf::st_transform(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
      
      data_2_map_mrgid_pop <- format_data2map(data = eez_shp_data_mrgid_pop,
                                              PROJ = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
      
        # - For the total population in LECZ
        data_2_map_mrgid_popTOT <- data_2_map_mrgid_pop
        data_2_map_mrgid_popTOT$data <- data_2_map_mrgid_popTOT$data |>  
          rename(layer = population)  
          
        
        tmp <- data_2_map_mrgid_popTOT$data |>  sf::st_drop_geometry()
        
        map <- univariate_map(data_map          = data_2_map_mrgid_popTOT,
                              eez               = NULL,
                              color_scale       = viridis::turbo(10, direction = 1),
                              midpoint          = NULL,
                              second.var        = NULL,
                              # vals_colors_scale = NULL,
                              title_color       = "Population in LECZ",
                              title_size        = NULL,
                              show.legend       = TRUE,
                              name              = "main/final_version/mrgid_population_in_LECZ3")
        
        # - For the total population in LECZ
        data_2_map_mrgid_popPERC <- data_2_map_mrgid_pop
        data_2_map_mrgid_popPERC$data <- data_2_map_mrgid_popPERC$data |>  rename(layer = perc_in_lecz)
        
        tmp2 <- data_2_map_mrgid_popPERC$data |>  sf::st_drop_geometry()
        
        map <- univariate_map(data_map          = data_2_map_mrgid_popPERC,
                              eez               = NULL,
                              color_scale       = viridis::turbo(10, direction = 1),
                              midpoint          = NULL,
                              second.var        = NULL,
                              # vals_colors_scale = NULL,
                              title_color       = "% of pop in LECZ",
                              title_size        = NULL,
                              show.legend       = TRUE,
                              name              = "main/final_version/mrgid_population_in_LECZ_perc3")
      # ----- 
        
      
  ## ---- FORMAR DATA: bind EXPOSURE and POPULATION data together
  exposure_mrgid <- pop_mrgid |> 
    dplyr::select(-iso_NA2, -TERRITORY1, -Country, -iso_code) |> 
    dplyr::full_join(vulne_mean_weighted_mrgid_df |>  dplyr::select(-Country), by = "MRGID") |> 
    dplyr::mutate(exposure_perc = storm_surges_weighted*perc_in_lecz) |> 
    ungroup()
        
    # --- Check map -----
    eez_shp_data_mrgid_expo <- eez_shp |>
      left_join(exposure_mrgid, by = "MRGID") |> #c("ISO_SOV1" = "iso_code")
      sf::st_transform(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") |> 
      rename(layer = exposure_perc)
        
    data_2_map_mrgid_expo <- format_data2map(data = eez_shp_data_mrgid_expo,
                                             PROJ = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
    map <- univariate_map(data_map          = data_2_map_mrgid_expo,
                          eez               = NULL,
                          color_scale       = viridis::turbo(10, direction = 1),
                          midpoint          = NULL,
                          second.var        = NULL,
                          # vals_colors_scale = NULL,
                          title_color       = "Exposure * % pop in LECZ",
                          title_size        = NULL,
                          show.legend       = TRUE,
                          name              = "main/final_version/exposure_TIMES_perc_pop")
        
    # -----
    
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
      mutate(country_id = case_when(TERRITORY1 == "Greenland" ~ "Greenland",
                                    TRUE ~ country_id)) |> 
      distinct(analysis_id, TERRITORY1, country_id)
    
    # --- Group data by country and territory
    
      # - To find unmatching patterns
      length(unique(adaptation_grid_df$TERRITORY1))
      length(unique(countries_territories_df$TERRITORY1))
      sum(unique(adaptation_grid_df$TERRITORY1) %in% unique(countries_territories_df$TERRITORY1))
      
      adaptation_grid_df$TERRITORY1[!adaptation_grid_df$TERRITORY1 %in% countries_territories_df$TERRITORY1]
      
      # - Correct unmatching patterns and group data
      adaptation_geop_paper_territory <- adaptation_grid_df |> 
        mutate(TERRITORY1 = case_when(TERRITORY1 == "Bosnia & Herzegovina" ~ "Bosnia and Herzegovina",
                                      TERRITORY1 == "Svalbard & Jan Mayen" ~ "Svalbard",
                                      TERRITORY1 == "Myanmar (Burma)" ~ "Myanmar",
                                      TERRITORY1 == "Côte d’Ivoire" ~ "Ivory Coast",
                                      # TERRITORY1 %in% c("Alaska", "Jarvis Island", "Howland and Baker Islands", "Hawaii") ~ "United States",
                                      # TERRITORY1 == "Curaçao" ~ "Netherlands Antilles",
                                      # TERRITORY1 == "Azores" ~ "Portugal",
                                      TRUE ~ TERRITORY1)) |> 
        group_by(TERRITORY1, country_id) |>
        summarise(Count_ORO = n()) |> 
        right_join(countries_territories_df, by = "TERRITORY1") |> 
        mutate(Count_ORO = case_when(is.na(Count_ORO) == TRUE ~ 0,
                                     TRUE ~ Count_ORO)) |> 
        ungroup()
      
      # --- Check map -----
      eez_shp_data_mrgid_papers <- eez_shp |>
        left_join(adaptation_geop_paper_territory, by = "MRGID") |> #c("ISO_SOV1" = "iso_code")
        sf::st_transform(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") |> 
        rename(layer = Count_ORO)
      
      data_2_map_mrgid_papers <- format_data2map(data = eez_shp_data_mrgid_papers,
                                                 PROJ = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
      
      map <- univariate_map(data_map          = data_2_map_mrgid_papers,
                            eez               = NULL,
                            color_scale       = viridis::turbo(10, direction = 1),
                            midpoint          = NULL,
                            second.var        = NULL,
                            # vals_colors_scale = NULL,
                            title_color       = "# adap papers (GeoP)",
                            title_size        = NULL,
                            show.legend       = TRUE,
                            name              = "main/final_version/adpatation_papers")
      
      # -----
      
  ## ---- FORMAT DATA: Bivariate color scale
      
    # --- Create the bivariate color scale
    bivariate_color_scale <- color_bivariate_map(nquantiles  = 10, 
                                                 upperleft   = "#0096EB", # rgb(130,0,80, maxColorValue = 255), 
                                                 upperright  = "#FFE60F", # rgb(255,230,15, maxColorValue = 255),
                                                 bottomleft  = "#e1edf7",
                                                 bottomright = "#820050", # rgb(0,150,235, maxColorValue = 255)  
                                                 ylab        = "CO2 emission (cum)",
                                                 xlab        = "n_weighted_papers")
      
    # --- Join adaptation ORO data with Exposure data
    expo_adaPubs_mrgid <- adaptation_geop_paper_territory |> 
      dplyr::select(-TERRITORY1, -Country, -iso_code, -iso_NA2, -country_id) |> 
      full_join(exposure_mrgid, by = "MRGID") |> 
      filter(!is.na(exposure_perc)) |> 
      ungroup()
    
    # --- Adapt it to the data
    data_bivar_n_article_expo_mrgid <- format_data_bivariate_map(data        = expo_adaPubs_mrgid,
                                                                 data.x      = "Count_ORO",
                                                                 data.y      = "exposure_perc", # "vulnerability",
                                                                 color_table = bivariate_color_scale,
                                                                 nquantiles  = 10) 
    
    # --- Format the shapefile of the world countries polygon and bind data
    world_shp_data_mrgid <- format_shp_of_the_world(world_shp    = world_shp,
                                                    data_to_bind = data_bivar_n_article_expo_mrgid,
                                                    PROJ         = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
    data_2_map_mrgid <- format_data2map(data = world_shp_data_mrgid,
                                        PROJ = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

    eez_shp_mrgid <- eez_shp |>
      left_join(data_bivar_n_article_expo_mrgid, by = "MRGID") |>
      sf::st_transform(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
        
  ## ---- MAP DATA
  map <- bivariate_map(data_map   = data_2_map_mrgid,
                       eez        = eez_shp_mrgid, 
                       data_world = NULL,
                       color      = bivariate_color_scale,
                       ylab       = "Exposure",
                       xlab       = "# ada. paper (GeoP)",
                       name       = "main/final_version/AdaPaperGeop_expo_territory3") 
  
  ## ---- Test for correlation between CC exposure and adaptation publications (by geoparsing)
  
    # --- Scale values by dividing by their sd and mean center
    x <- scale(data_bivar_n_article_expo_mrgid$Count_ORO, center = TRUE, scale = TRUE)
    y <- scale(data_bivar_n_article_expo_mrgid$exposure_perc, center = TRUE, scale = TRUE)
    
    data_without_zero <- data_bivar_n_article_expo_mrgid |> 
      filter(Count_ORO > 0) |> 
      filter(exposure_perc > 0)
    x2 <- scale(data_without_zero$Count_ORO, center = TRUE, scale = TRUE)
    y2 <- scale(data_without_zero$exposure_perc, center = TRUE, scale = TRUE)
    
    # --- Test for normality
    shapiro.test(x) # what is the p value? if >0.05, normal
    shapiro.test(y) # same -- report p value
    
    shapiro.test(x2) # what is the p value? if >0.05, normal
    shapiro.test(y2) # same -- report p value
    
    # --- Test for linear relationship
    plot(y, x)
    plot(y2, x2)
    
    # --- calculate Spearman's correlation coefficienct (not Pearson's coefficient due to the non-normality of the data)
    ada_cor <- cor.test(x, y, method = "spearman") ; ada_cor
    ada_cor2 <- cor.test(x2, y2, method = "spearman") ; ada_cor2
    
  ## ---- Test for correlation between CC exposure and adaptation publications (by geoparsing)
  fit <- glm(Count_ORO ~ exposure_perc, data = data_bivar_n_article_expo_mrgid, family = poisson) 
  summary(fit)
  exp(0.0052082)
  
  range(data_bivar_n_article_expo_mrgid$exposure_perc)
  
  fit2 <- glm(Prop_ORO ~ exposure_perc_normalized, data = data_bivar_n_article_expo_mrgid, familiy = binomial)
  fit2 <- glm(cbind(Count_adaptation, Count_mitigation) ~ exposure_perc_normalized, data = data_bivar_n_article_expo_mrgid, familiy = binomial)
  summary(fit2)
  # to transform the coefficient (B) into % likelihood, it would be:
  (exp(B)-1)*100
  
        
    
      
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
      filter(iso_code != "") |> 
      mutate(country = countrycode(sourcevar   = country,
                                   origin      = "country.name",
                                   destination = "country.name"),
             cumulative_co2_including_luc = case_when(is.na(cumulative_co2_including_luc) == FALSE ~ cumulative_co2_including_luc,
                                                      is.na(cumulative_co2_including_luc) == TRUE ~ cumulative_co2)) |> 
      # Filter rows returning NA because not identified as a country (reason behing the warning message)
      filter(!is.na(cumulative_co2_including_luc)) |> 
      rename(iso_NA2 = iso_code)
    
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
                                        TERRITORY1 == "Bassas da India" ~ "FRA",
                                        iso_code == "KIR" ~ "KIR",
                                        Country == "Netherlands" & TERRITORY1 != "Netherlands" ~ "ANT",
                                        is.na(iso_NA2) == TRUE ~ iso_code,
                                        TRUE ~ iso_NA2))
    
### ----- PANEL A -----    
    
  ## ---- FORMAT DATA: Mitigation data
    
    # --- Select only mitigation data (OK)
    mitigation_grid_df <- geoparsed_data_clean |>
      left_join(pred_oro_branch, by = "analysis_id", copy = TRUE) |> 
      left_join(pred_relevance, by = "analysis_id", copy = TRUE) |> 
      filter(0.5 <= relevance_mean) |> 
      mutate(mitigation = ifelse(0.5 <= `oro_branch.Mitigation - mean_prediction`, 1 ,0)) |>  
      filter(mitigation == 1) |> 
      dplyr::select(analysis_id, mitigation, shp_id, grid_df_id, TERRITORY1, country_id) |> 
      mutate(country_id = case_when(TERRITORY1 == "Greenland" ~ "Denmark",
                                    country_id == "Svalbard & Jan Mayen" ~ "Norway",
                                    TRUE ~ country_id)) |> 
      distinct(analysis_id, TERRITORY1, country_id)
    
    
    # --- Group data by country and territory
    
      # --- Find unmatching patterns
      length(unique(mitigation_grid_df$TERRITORY1))
      length(unique(countries_territories_df$TERRITORY1))
      sum(unique(mitigation_grid_df$TERRITORY1) %in% unique(countries_territories_df$TERRITORY1))
      
      unique(mitigation_grid_df$TERRITORY1[!mitigation_grid_df$TERRITORY1 %in% countries_territories_df$TERRITORY1])
      
      
      # - Correct unmatching patterns and group data
      mitigation_geop_paper_country_territory <- mitigation_grid_df |> 
        mutate(TERRITORY1 = case_when(TERRITORY1 == "Bosnia & Herzegovina" ~ "Bosnia and Herzegovina",
                                      TERRITORY1 == "Svalbard & Jan Mayen" ~ "Svalbard",
                                      TERRITORY1 == "Myanmar (Burma)" ~ "Myanmar",
                                      # TERRITORY1 == "North Macedonia" ~ "Ivory Coast",
                                      TERRITORY1 == "Somalia" ~ "Federal Republic of Somalia",
                                      TRUE ~ TERRITORY1)) |> 
        group_by(TERRITORY1, country_id) |> 
        summarise(Count_ORO_mit = n()) |> 
        right_join(countries_territories_df, by = "TERRITORY1") |> 
        mutate(Count_ORO_mit = case_when(is.na(Count_ORO_mit) == TRUE ~ 0,
                                         TRUE ~ Count_ORO_mit)) |> 
        ungroup()
      
      # - Checks -----
      
        # Number of papers (OK)
        length(unique(mitigation_grid_df$analysis_id)) # 2948 unique papers but 4861 rows since papers are geoparsed in different countries
        tmp <- mitigation_geop_paper_country_territory |> 
          dplyr::select(Country, TERRITORY1, Count_ORO_mit) |> 
          distinct(); sum(tmp$Count_ORO_mit)
          
        4861 - sum(tmp$Count_ORO_mit)
        length(unique(mitigation_grid_df$analysis_id[mitigation_grid_df$country_id %in% c("Antarctica", "Laos", "North Macedonia")])) # must be 18
    
      # ---- 

    
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
                                    country_id == "Svalbard & Jan Mayen" ~ "Norway",
                                    TRUE ~ country_id)) |> 
      distinct(analysis_id, TERRITORY1, country_id)
    
    # --- Group data by country and territory
    
      # - To find unmatching patterns
      length(unique(adaptation_grid_df$TERRITORY1))
      length(unique(countries_territories_df$TERRITORY1))
      sum(unique(adaptation_grid_df$TERRITORY1) %in% unique(countries_territories_df$TERRITORY1))
      
      unique(adaptation_grid_df$TERRITORY1[!adaptation_grid_df$TERRITORY1 %in% countries_territories_df$TERRITORY1])
      
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
        summarise(Count_ORO_ada = n()) |> 
        right_join(countries_territories_df, by = "TERRITORY1") |>
        mutate(Count_ORO_ada = case_when(is.na(Count_ORO_ada) == TRUE ~ 0,
                                         TRUE ~ Count_ORO_ada)) |> 
        ungroup() |> 
        dplyr::select(-country_id)
      
      # - Checks -----
      
        # Number of papers
        length(unique(adaptation_grid_df$analysis_id)) # 2144 unique papers but 2899 rows since papers are geoparsed in different countries
        tmp <- adaptation_geop_paper_territory |> 
          dplyr::select(Country, TERRITORY1, Count_ORO_ada) |> 
          distinct() # distinct to avoid duplicates with MRGID (e.g. USA)
        
        sum(tmp$Count_ORO_ada)
        
        2899 - sum(tmp$Count_ORO_ada)
        length(unique(adaptation_grid_df$analysis_id[adaptation_grid_df$country_id %in% c("Antarctica", "Bolivia")])) # must be 2 
      
      # ---- 
      
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
    
  ## ---- Join adaptation and mitigation data to get the proportion of mitigation and adaptation papers per countries
  adap_miti_geoP_paper_mrgid <- adaptation_geop_paper_territory |> 
      dplyr::select(Count_ORO_ada, MRGID) |> 
      full_join(mitigation_geop_paper_country_territory, by = "MRGID") |> 
      mutate(Count_ORO_adap_miti = Count_ORO_ada + Count_ORO_mit,
             perc_mit = Count_ORO_mit/Count_ORO_adap_miti,
             perc_ada = Count_ORO_ada/Count_ORO_adap_miti) 
    
    # - Checks -----
    length(unique(adaptation_geop_paper_territory$MRGID))
    length(unique(adaptation_geop_paper_territory$MRGID))
    sum(unique(adaptation_geop_paper_territory$MRGID) %in% unique(adaptation_geop_paper_territory$MRGID))
    sum(unique(adaptation_geop_paper_territory$MRGID) %in% unique(adaptation_geop_paper_territory$MRGID))
    
    unique(adaptation_geop_paper_territory$MRGID[!adaptation_geop_paper_territory$MRGID %in% adaptation_geop_paper_territory$MRGID])
    
      # Number of papers
      tmp <- adap_miti_geoP_paper_mrgid |> 
        dplyr::select(Country, TERRITORY1, Count_ORO_ada, Count_ORO_mit, Count_ORO_adap_miti) |> 
        distinct() 
      
      sum(tmp$Count_ORO_ada) # must be 2897
      sum(tmp$Count_ORO_mit) # must be 4843
      sum(tmp$Count_ORO_adap_miti) # 7740

    # ---- 
    
    
  ## ---- Format emissions data
  GHGemi_mrgid <- GHGemi_country |> 
    left_join(countries_territories_df |> dplyr::select(-Country, -TERRITORY1, -MRGID), by = "iso_NA2") |> 
    mutate(iso_code = case_when(is.na(iso_code) == TRUE ~ iso_NA2,
                                TRUE ~ iso_code)) |> 
    distinct() 
  
    # --- All territitories emissions are the same as national emissions.
    GHGemi_country <- GHGemi_mrgid |> 
      group_by(iso_code) |> 
      summarise(cumulative_co2_including_luc = sum(cumulative_co2_including_luc, na.rm = TRUE)) |> 
      left_join(countries_territories_df, by = "iso_code") |> 
      filter(!is.na(MRGID))
    
    
  ## ---- FORMAT DATA: Bivariate color scale
    
    # --- Join mitigation ORO data with GHG emissions data
    GHGemi_mitPubs_country <- adap_miti_geoP_paper_mrgid |> 
      dplyr::select(Country, TERRITORY1, iso_code, Count_ORO_mit, Count_ORO_adap_miti) |>
      distinct() |>  # distinct to avoid duplicates with MRGID (e.g. USA)
      group_by(Country, iso_code) |> 
      summarise(Count_ORO_mit = sum(Count_ORO_mit, na.rm = TRUE),
                Count_ORO_adap_miti = sum(Count_ORO_adap_miti, na.rm = TRUE)) |> 
      mutate(perc_mit = Count_ORO_mit/Count_ORO_adap_miti) |> 
      ungroup() |> 
      left_join(countries_territories_df |> dplyr::select(-TERRITORY1, -Country), by = "iso_code") |>
      filter(!is.na(MRGID)) |>
      left_join(GHGemi_country |>  dplyr::select(MRGID, cumulative_co2_including_luc), by = "MRGID")
    
    
    # - Checks -----
      # Number of papers
      tmp <- GHGemi_mitPubs_country |> 
        dplyr::select(Country, Count_ORO_mit, Count_ORO_adap_miti) |> 
        distinct() 
      
      sum(tmp$Count_ORO_mit) # must be 4843
      sum(tmp$Count_ORO_adap_miti) # 7740
      
    # ---- 
        
    
    # --- Create the bivariate color scale
    bivariate_color_scale <- color_bivariate_map(nquantiles  = 10, 
                                                 upperleft   = "#0096EB", # rgb(130,0,80, maxColorValue = 255), 
                                                 upperright  = "#FFE60F", # rgb(255,230,15, maxColorValue = 255),
                                                 bottomleft  = "#e1edf7",
                                                 bottomright = "#820050", # rgb(0,150,235, maxColorValue = 255)  
                                                 ylab        = "CO2 emission (cum)",
                                                 xlab        = "n_weighted_papers") 
    
    # --- Adapt it to the data
    GHGemi_mitPubs_country_for_scale_stats <- GHGemi_mitPubs_country |> 
      dplyr::select(Country, iso_code, Count_ORO_mit, perc_mit, Count_ORO_mit, Count_ORO_adap_miti, cumulative_co2_including_luc) |>
      filter(Count_ORO_adap_miti != 0) |> 
      filter(is.na(cumulative_co2_including_luc) != TRUE) |> 
      distinct() 
    
    sum(GHGemi_mitPubs_country_for_scale_stats$Count_ORO_mit) # must be 4838 (instead of 4843) because remove of western sahara that don't have emissions data
    sum(GHGemi_mitPubs_country_for_scale_stats$Count_ORO_adap_miti) # 7735
    
      # --- Save the data for Devi
      save(GHGemi_mitPubs_country_for_scale_stats, file = here::here("data", "GHGemi_mitPubs_country_for_scale_stats.RData"))


    
    data_bivar_n_article_CO2em <- format_data_bivariate_map(data        = GHGemi_mitPubs_country_for_scale_stats,
                                                            data.y      = "perc_mit", # "Count_ORO_mit"
                                                            data.x      = "cumulative_co2_including_luc",
                                                            color_table = bivariate_color_scale,
                                                            nquantiles  = 10,
                                                            probs.quant.x = seq(0, 1, 0.1),
                                                            probs.quant.y = seq(0, 1, 0.1)) |> 
      left_join(countries_territories_df |> dplyr::select(-TERRITORY1, -Country), by = "iso_code") |>
      filter(!is.na(MRGID)) 
    
    # - Checks -----
    # Number of papers
    tmp <- data_bivar_n_article_CO2em |> 
      dplyr::select(Country, Count_ORO_mit, Count_ORO_adap_miti) |> 
      distinct() 
    
    sum(tmp$Count_ORO_mit) # must be 4838
    sum(tmp$Count_ORO_adap_miti) # 7735
    
    # ---- 
    
    # --- Format the shapefile of the world countries polygon and bind data
    world_shp_data <- format_shp_of_the_world(world_shp    = world_shp,
                                              data_to_bind = data_bivar_n_article_CO2em,
                                              PROJ         = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
    data_2_map <- format_data2map(data = world_shp_data,
                                  PROJ = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    

    # --- Format the shapefile of the eez countries polygon and bind data
    eez_shp_data <- eez_shp |>
      left_join(data_bivar_n_article_CO2em, by = "MRGID") |>
      filter(!is.na(MRGID)) |> 
      sf::st_transform(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
      # - Checks -----
      # Number of papers
      tmp <- eez_shp_data |> 
        sf::st_drop_geometry() |> 
        dplyr::select(Country.x, Count_ORO_mit, Count_ORO_adap_miti) |> 
        distinct() 
      
      sum(tmp$Count_ORO_mit, na.rm = TRUE) # must be 4838
      sum(tmp$Count_ORO_adap_miti, na.rm = TRUE) # 7735
      
      # ---- 
    
  ## ---- PLOT DATA
  panelA <- bivariate_map(data_map   = data_2_map,
                          eez        = eez_shp_data, 
                          data_world = NULL,
                          color      = bivariate_color_scale,
                          xlab       = "CO2eq. emissions",
                          ylab       = "% mit. paper (GeoP)",
                          name       = "main/final_version/bivar_map_GHGemi_mitPubs_PERC_Geop")
      
      
  ## ---- DEVI, here is the model section for emissions and mitigation papers
  ## ---- You can load this data file
  load(here::here("data", "GHGemi_mitPubs_country_for_scale_stats.RData"))
  
  ## ---- Test for correlation between co2 emissions and mitigation publications (by geoparsing)
  
    # --- Scale values by dividing by their sd and mean center
    y.count <- scale(GHGemi_mitPubs_country_for_scale_stats$Count_ORO_mit, center = TRUE, scale = TRUE)
    y.perc <- scale(GHGemi_mitPubs_country_for_scale_stats$perc_mit, center = TRUE, scale = TRUE)
    x <- scale(GHGemi_mitPubs_country_for_scale_stats$cumulative_co2_including_luc, center = TRUE, scale = TRUE)
    

    # --- Test for normality
    shapiro.test(x) # what is the p value? if >0.05, normal
    shapiro.test(y.count) # same -- report p value
    shapiro.test(y.perc) # same -- report p value
    
    # --- Test for linear relationship
    plot(y.count, x)
    plot(y.perc, x)
    
    
    # --- calculate Spearman's correlation coefficienct (not Pearson's coefficient due to the non-normality of the data
    mit_cor.count <- cor.test(x, y.count, method = "spearman") ; mit_cor.count
    mit_cor.perc <- cor.test(x, y.perc, method = "spearman") ; mit_cor.perc
    
    
  ## ---- GLM
    
    ggplot(data = GHGemi_mitPubs_country_for_scale_stats) +
      geom_point(mapping = aes(x = scales::rescale(cumulative_co2_including_luc, to = c(0, 1)),
                               y = log(perc_mit/(1-perc_mit)),
                               size = Count_ORO_adap_miti)) +
      theme_bw()
    
    
    # --- Model with all countries
    y.mit.perc <- GHGemi_mitPubs_country_for_scale_stats$perc_mit
    count_total.mit <- GHGemi_mitPubs_country_for_scale_stats$Count_ORO_adap_miti
    x.mit <- GHGemi_mitPubs_country_for_scale_stats$cumulative_co2_including_luc 
    x.mit.scaled <- scales::rescale(x.mit, to = c(0, 1)) # Scale X data between 0 and 1 to compare it with adaptation

    fit.mit <- glm(y.mit.perc ~ x.mit.scaled, family = binomial, weights = rep(10, length(x.mit.scaled))) ; summary(fit.mit)
    fit.mit <- glm(y.mit.perc ~ x.mit.scaled, family = binomial, weights = count_total.mit) ; summary(fit.mit)
    
    exp(fit.mit$coefficients[2])
    
    # --- Model without USA
    data.without.USA <- GHGemi_mitPubs_country_for_scale_stats |> 
      filter(Country != "United States")
    
    y.mit.perc.USA <- data.without.USA$perc_mit
    x.mit.USA <- data.without.USA$cumulative_co2_including_luc 
    count_total.mit.USA <- data.without.USA$Count_ORO_adap_miti
    x.mit.scaled.USA <- scales::rescale(x.mit.USA, to = c(0, 1))
    fit.mit.USA <- glm(y.mit.perc.USA ~ x.mit.scaled.USA, family = binomial, weights = count_total.mit.USA) ; summary(fit.mit.USA)
    exp(fit.mit.USA$coefficients[2])
    
### -----  
  
  
  
### ----- PANEL B ----- 
  
  ## ---- LOAD DATA (vulnerability)
  surges_rast <- raster::raster(here::here("data", "external", "climate-hazard-maps", "Coastal_surges_1974-2014_R.tif")) 
  area_rast <- raster::area(surges_rast)
  
  ## ---- LOAD DATA (population in low elevation coastal zones): https://sedac.ciesin.columbia.edu/data/set/lecz-urban-rural-population-estimates-v1/data-download
  pop_lecz <- readxl::read_excel(here::here("data", "external", "population_lecz", "10mlecz-grumpv1.xls"), sheet = "country_lecz")
  

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
    
    
  ## ---- FORMAT DATA: Bivariate color scale
    
    # --- Create the bivariate color scale
    bivariate_color_scale <- color_bivariate_map(nquantiles  = 10, 
                                                 upperleft   = "#0096EB", # rgb(130,0,80, maxColorValue = 255), 
                                                 upperright  = "#FFE60F", # rgb(255,230,15, maxColorValue = 255),
                                                 bottomleft  = "#e1edf7",
                                                 bottomright = "#820050", # rgb(0,150,235, maxColorValue = 255)  
                                                 ylab        = "CO2 emission (cum)",
                                                 xlab        = "n_weighted_papers")
    
    
    expo_adaPubs_mrgid <- adap_miti_geoP_paper_mrgid |> 
      dplyr::select(-TERRITORY1, -Country, -iso_code, -iso_NA2, -country_id) |> 
      full_join(exposure_mrgid, by = "MRGID") |> 
      filter(!is.na(exposure_perc)) |>
      ungroup()
    

    # - Checks -----
    # Number of papers
    tmp <- adap_miti_geoP_paper_mrgid |> 
      dplyr::select(TERRITORY1, Count_ORO_ada, Count_ORO_adap_miti) |> 
      distinct() ; sum(tmp$Count_ORO_ada) # must be must be 2897
    
    tmp2 <- expo_adaPubs_mrgid |> 
      dplyr::select(TERRITORY1,  Count_ORO_ada, Count_ORO_adap_miti) |> 
      distinct() ; sum(tmp2$Count_ORO_ada) 
    # Equal 2867 (instead of 2867) because 30 papers remove with the filter(!is.na(exposure_perc)).
    
    # Check of the 30 papers
    where_na <- adap_miti_geoP_paper_mrgid |> 
      dplyr::select(-TERRITORY1, -Country, -iso_code, -iso_NA2, -country_id, -perc_mit) |> 
      full_join(exposure_mrgid, by = "MRGID") |> 
      filter(is.na(exposure_perc)) |>
      ungroup() |>  
      dplyr::select(TERRITORY1,  Count_ORO_ada, Count_ORO_adap_miti) |> 
      distinct() |> 
      filter(! TERRITORY1 %in% expo_adaPubs_mrgid$TERRITORY1); sum(where_na$Count_ORO_ada)
    
    # ---- 
    
    # --- Adapt it to the data
    expo_adaPubs_mrgid_for_scale_stats <- expo_adaPubs_mrgid |>
      filter(Count_ORO_adap_miti != 0)
    
      # Save data for Devi
      save(expo_adaPubs_mrgid_for_scale_stats, file = here::here("data", "expo_adaPubs_mrgid_for_scale_stats.RData"))

    
    data_bivar_n_article_expo_mrgid <- format_data_bivariate_map(data        = expo_adaPubs_mrgid_for_scale_stats,
                                                                 data.y      = "perc_ada", # "Count_ORO_ada"
                                                                 data.x      = "exposure_perc", # "vulnerability",
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
      sf::st_transform(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") |> 
      rename(Country.x = TERRITORY1.x)
    
    # - Checks -----
    # Number of papers
    tmp <- eez_shp_mrgid |> 
      sf::st_drop_geometry() |> 
      dplyr::select(Country.x, Count_ORO_ada, Count_ORO_adap_miti) |> 
      distinct() 
    
    sum(tmp$Count_ORO_ada, na.rm = T) # must be 2867
    # ---- 
    
  ## ---- MAP DATA
  map <- bivariate_map(data_map   = data_2_map_mrgid,
                       eez        = eez_shp_mrgid, 
                       data_world = NULL,
                       color      = bivariate_color_scale,
                       xlab       = "Exposure",
                       ylab       = "% ada. paper (GeoP)",
                       name       = "main/final_version/AdaPaperGeop_PERC_expo_territory_Geop") 
  
  ## ---- DEVI, here is the model section for emissions and mitigation papers
  ## ---- You can load this data file
  load(here::here("data", "expo_adaPubs_mrgid_for_scale_stats.RData"))
  
  ## ---- Test for correlation between CC exposure and adaptation publications (by geoparsing)
  
    # --- Scale values by dividing by their sd and mean center
    x.ada <- scale(expo_adaPubs_mrgid_for_scale_stats$exposure_perc, center = TRUE, scale = TRUE)
    y.count.ada <- scale(expo_adaPubs_mrgid_for_scale_stats$Count_ORO_ada, center = TRUE, scale = TRUE)
    y.perc.ada <- scale(expo_adaPubs_mrgid_for_scale_stats$perc_ada, center = TRUE, scale = TRUE)

    
    # --- Test for linear relationship
    plot(y.count.ada, x.ada)
    plot(y.perc.ada, x.ada)
    
    
    # --- calculate Spearman's correlation coefficienct (not Pearson's coefficient due to the non-normality of the data
    ada_cor.count <- cor.test(x.ada, y.count.ada, method = "spearman") ; ada_cor.count
    ada_cor.perc <- cor.test(x.ada, y.perc.ada, method = "spearman") ; ada_cor.perc
    
  
  ## ---- GLM
    
    # --- Scale Y data between 0 and 1 to compare it with adaptation
    y.ada.count <- expo_adaPubs_mrgid_for_scale_stats$Count_ORO_ada
    y.ada.perc <- expo_adaPubs_mrgid_for_scale_stats$perc_ada
    count_total <- expo_adaPubs_mrgid_for_scale_stats$Count_ORO_adap_miti
    x.ada <- expo_adaPubs_mrgid_for_scale_stats$exposure_perc
    x.ada.scaled <- scales::rescale(x.ada, to = c(0, 1))
    
    # IMPORTANT (MAYBE): some y values = INF or -INF when the perc_mit value = 0 or 1.
    ggplot(data = data_bivar_n_article_expo_mrgid) +
      geom_point(mapping = aes(x = scales::rescale(exposure_perc, to = c(0, 1)),
                               y = log(perc_ada/(perc_mit)),
                               size = Count_ORO_adap_miti)) +
      scale_y_continuous(limits=c(-100,100)) +
      theme_bw()
    
    # --- Model
    fit.ada <- glm(y.ada.perc ~ x.ada.scaled, family = binomial, weights = rep(10, length(x.ada.scaled))) ; summary(fit.ada)
    exp(fit.ada$coefficients[2])
    
    fit.ada.weight <- glm(y.ada.perc ~ x.ada.scaled, family = binomial, weights = count_total) ; summary(fit.ada.weight)
    exp(fit.ada.weight$coefficients[2])
    

    

### -----
    
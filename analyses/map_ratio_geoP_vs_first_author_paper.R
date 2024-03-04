###################################################################################################
#                                                                                                 #
# FIGURE X                                                                                        #
# Panel A: Ratio number of publication per country using geopased data vs. 1st author affiliation #  
#                                                                                                 #
###################################################################################################
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
library(ggrepel)

### ----- LOAD FUNCTIONS -----
source(here::here("R", "functions_to_format.R")) # all functions needed to format data
source(here::here("R", "functions_to_plot.R")) # all functions needed to plot dataw


### ----- CONNECTION TO THE LATEST VERSION OF THE SQL DATABASE -----
sqliteDir <- here::here("data/sqlite-databases")
sqliteFiles <- dir(sqliteDir)
sqliteVersions <- as.numeric(gsub(".sqlite","",substring(sqliteFiles, regexpr("_v", sqliteFiles) + 2)))
latestVersion <- sqliteFiles[which.max(sqliteVersions)]
dbcon <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(sqliteDir, latestVersion), create=FALSE)


### ---- PANEL A -----

  ## ---- LOAD DATA

    # --- List of countries
    countries_ls <- read.csv(file = here::here("data", "external", "list_of_countries", "sql-pays.csv"), sep = ";") |>  # Countries names
      dplyr::mutate(country = countrycode(sourcevar   = name_en,
                                          origin      = "country.name",
                                          destination = "country.name"),
                    iso_code = countrycode(sourcevar   = country,
                                           origin      = "country.name",
                                           destination = "iso3c"))

    # --- Cleaned geoparsed data (see .Rmd script called 0_data-processing-cleaning.Rmd)
    geoparsed_data_clean <- get(load(here::here("data", "geoparsing", "tmp_clean.RData")))
    
    # --- 1st author data
    uniquerefs <- tbl(dbcon, "uniquerefs") # metadata on the unique references
    pred_relevance <- tbl(dbcon, "pred_relevance")  # which articles are relevant to OROs

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
    
    
  ## ---- FORMAT geoparsed data
    
    # --- Select relevant geoparsed data
    geop_data_all <- geoparsed_data_clean |>
      left_join(pred_relevance, by = "analysis_id", copy = TRUE) |> 
      filter(0.5 <= relevance_mean) |> 
      dplyr::select(analysis_id, shp_id, grid_df_id, TERRITORY1, country_id) |> 
      mutate(country_id = case_when(TERRITORY1 == "Greenland" ~ "Denmark",
                                    TRUE ~ country_id)) |> 
      distinct(analysis_id, TERRITORY1, country_id) |> 
      rename(territory1_geop = TERRITORY1, 
             country_geop    = country_id) |> 
      mutate(iso_code_geoP = countrycode(sourcevar   = country_geop,
                                         origin      = "country.name",
                                         destination = "iso3c"))
    
    # --- Geoparsed data per country
    # geop_data_per_country <- geop_data_all |> 
    #   group_by(country_id) |> 
    #   summarise(n_geop = n())
    
  ## ---- FORMAT 1st author affiliation data
    
    # --- Subset to relevant rows and get the affiliation
    oroAffiliations <- pred_relevance |> 
      left_join(uniquerefs, by = "analysis_id") |> 
      dplyr::select(analysis_id, affiliation) |> 
      collect() |> 
      filter(analysis_id %in% unique(geop_data_all$analysis_id)) |> 
      filter(!is.na(affiliation))
    
    # --- Extract the country of the first author for each relevant publications
    data_1stA_country <- extract_1stA_affiliation(data         = oroAffiliations, 
                                                  countries_ls = countries_ls)
    
    # -- Number of publication per country
    data_aff <- data_1stA_country$oroAff_1stA |> 
      dplyr::filter(!is.na(country_aff)) |> 
      dplyr::mutate(country_aff = stringr::str_replace_all(country_aff, c("Guam" = "United States",
                                                                          "New Caledonia" = "France",
                                                                          "Puerto Rico"   = "United States")),
                    country_aff = countrycode(sourcevar   = country_aff,
                                              origin      = "country.name",
                                              destination = "country.name"),
                    iso_code_aff = countrycode(sourcevar   = country_aff,
                                               origin      = "country.name",
                                               destination = "iso3c")) |> 
      dplyr::select(-affiliation) 

      # dplyr::group_by(country_aff, iso_code) 
      # dplyr::summarise(n_firstA = n()) 
    
  ## ---- FORMAT geoparsed and 1st author data together
    
    # --- Bind data
    data_1A_geop <- full_join(geop_data_all, data_aff, by = "analysis_id")
    
    # --- Data a the country scale for geoparsing
    data_per_country_geoP <- data_1A_geop |> 
      group_by(country_geop, iso_code_geoP) |> 
      summarise(n_geop = n()) 
    
    # --- Data a the country scale for 1 author affiliation
    data_per_country_aff <- data_1A_geop |> 
      group_by(country_aff, iso_code_aff) |> 
      summarise(n_aff = n()) 
    
    # --- Common data at the country scale
    ORO_per_country_common_Aff_Geop <- full_join(data_per_country_aff, data_per_country_geoP, by = c("iso_code_aff" = "iso_code_geoP")) |> 
      replace_na(list(n_geop = 0, n_aff = 0)) |> 
      mutate(layer = ((n_aff - n_geop)/n_geop)*100) |> 
      ungroup() |> 
      rename(iso_code = iso_code_aff,
             country  = country_aff) |> 
      dplyr::select(-country_geop)
    
    # --- Format data to map
    # --- Format the shapefile of the world countries polygon and bind data
    world_shp_boundaries <- format_shp_of_the_world(world_shp    = world_shp,
                                                    data_to_bind = ORO_per_country_common_Aff_Geop,
                                                    PROJ         = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
      
    data_2_map <- format_data2map(data = world_shp_boundaries,
                                  PROJ = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
    # --- Format the shapefile of the eez countries polygon and bind data
    eez_shp_SIDS <- eez_shp |>
      filter(group_land == "SIDS") |>
      left_join(ORO_per_country_common_Aff_Geop, by = c("ISO_SOV1" = "iso_code")) |>
      filter(!is.na(MRGID)) |> 
      sf::st_transform(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
    
    
  ## ---- PLOT PANEL A 
  data_2_map$data$layer[data_2_map$data$layer == Inf] <- 100
  data_2_map$data$layer[data_2_map$data$layer > 100] <- 100
  panelA <- univariate_map(data_map          = data_2_map,
                           eez               = eez_shp_SIDS,
                           color_scale       = c("darkred","white", "darkblue"),
                           midpoint          = 0,
                           second.var        = NULL,
                           # vals_colors_scale = NULL,
                           title_color       = "Change in \n #paper (%)",
                           title_size        = NULL,
                           show.legend       = TRUE,
                           name              = "main/map_geoP_vs_affiliation")
  
  
### ----

### ---- PANEL B -----
  
  ## --- Format data from the Sankey Diagram
  
    # -- Find the corresponding continent and sumamrise
    ccodes <- raster::ccodes() |>  
      select(NAME, continent, UNREGION1) |> 
      mutate(NAME = case_when(NAME == "Micronesia" ~ "Micronesia (Federated States of)",
                              TRUE ~ NAME)) |> 
      rename(region = UNREGION1) |> 
      mutate(NAME = countrycode(sourcevar   = NAME,
                                origin      = "country.name",
                                destination = "country.name"),
             region = case_when(str_detect(region, pattern = "Asia") == TRUE ~ "Asia",
                                str_detect(region, pattern = "Africa") == TRUE ~ "Africa",
                                str_detect(region, pattern = "Europe") == TRUE ~ "Europe",
                                TRUE ~ region))
      
  
    # -- Add the continent to the geoparsed data
    geop_data_conti <- geop_data_all |> 
        left_join(ccodes, by = c("country_geop" = "NAME")) |> 
        rename(continent_geoP = continent,
               region_geoP    = region) 
      
    # -- Add the continent to the 1st author affiliation data
    data_aff_conti <- data_aff |> 
      left_join(ccodes, by = c("country_aff" = "NAME")) |> 
      rename(continent_aff = continent,
             region_aff    = region)  

    
    # -- Bind Data
    data_aff_geoP <- left_join(geop_data_conti, data_aff_conti, by = "analysis_id") |> 
      replace_na(list(n_aff = 0,
                      country_aff   = "Unknown",
                      iso_code_aff  = "Unknown",
                      continent_aff = "Unknown",
                      region_aff    = "Unknown")) 
    
  ## --- Format data at the continent scale
  data_aff_geoP_conti <- data_aff_geoP |> 
    dplyr::select(analysis_id, continent_geoP, continent_aff) |> 
    group_by(continent_aff, continent_geoP) |> 
    summarise(value = n()) |> 
    rename(source = continent_aff,
           target = continent_geoP) |> 
    mutate(target = paste(target, " ", sep=""))
  
  nodes_conti <- data.frame(name=c(as.character(data_aff_geoP_conti$source), as.character(data_aff_geoP_conti$target)) %>% unique())
  
  data_aff_geoP_conti <- data_aff_geoP_conti |> 
    mutate(# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
      IDsource = match(source, nodes_conti$name)-1,
      IDtarget = match(target, nodes_conti$name)-1)
  
  ## --- Format data at the region scale
  data_aff_geoP_region <- data_aff_geoP |> 
    dplyr::select(analysis_id, region_geoP, region_aff) |> 
    group_by(region_aff, region_geoP) |> 
    summarise(value = n()) |> 
    rename(source = region_aff,
           target = region_geoP) |> 
    mutate(target = paste(target, " ", sep=""))
  
  nodes_region <- data.frame(name=c(as.character(data_aff_geoP_region$source), as.character(data_aff_geoP_region$target)) %>% unique())
  
  data_aff_geoP_region <- data_aff_geoP_region |> 
    mutate(# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
           IDsource = match(source, nodes_region$name)-1,
           IDtarget = match(target, nodes_region$name)-1)

      

### ---- Plot Sankey Diagramm
library(patchwork)
library(hrbrthemes)
library(circlize)
library(networkD3)
library(webshot)

  
  ## --- Continent scale
  sankey_continent <- sankeyNetwork(Links = data_aff_geoP_conti, Nodes = nodes_conti,
                                    Source = "IDsource", Target = "IDtarget",
                                    Value = "value", NodeID = "name",
                                    sinksRight = FALSE, nodeWidth = 40, fontSize = 30, nodePadding = 20) ; sankey_continent
  
  saveNetwork(sankey_continent, here::here("figures", "main", "sankey_continent.html"))
  # you convert it as png
  webshot(here::here("figures", "main", "sankey_continent.html"), 
          here::here("figures", "main", "sankey_continent.png"), 
          vwidth = 1000, vheight = 900)

  ## --- Region scale
  sankey_region <- sankeyNetwork(Links = data_aff_geoP_region, Nodes = nodes_region,
                Source = "IDsource", Target = "IDtarget",
                Value = "value", NodeID = "name", 
                sinksRight = FALSE, nodeWidth = 40, fontSize = 30, nodePadding = 20) ; sankey_region
  
  saveNetwork(sankey_region, here::here("figures", "main", "sankey_region.html"))
  # you convert it as png
  webshot(here::here("figures", "main", "sankey_region.html"), 
          here::here("figures", "main", "sankey_region.png"), 
          vwidth = 1000, vheight = 900)

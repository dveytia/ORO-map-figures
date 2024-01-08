#########################################################################################
#                                                                                       #
# FIGURE 2                                                                              #
# Panel A: Map of # ORO publications/# publications on ocean & climate  in each country #  
# Panel B: Map of ratio of mitigation/adaptation ORO publications                       #
# Panel C: Scatter plot (1 point = 1 country), # ORO publication = f(total #pub on O&C) #
#                                                                                       #
#########################################################################################
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
library(ggrepel)

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
  uniquerefs <- tbl(dbcon, "uniquerefs") # metadata on the unique references
  pred_relevance <- tbl(dbcon, "pred_relevance")  # which articles are relevant to OROs
  
  countries_ls <- read.csv(file = here::here("data", "external", "list_of_countries", "sql-pays.csv"), sep = ";") # Countries names
  
  numb_OandApub_per_country <- read.delim(here::here("data/external/ocean-and-climate-publications/WOS_ocean-and-climate_by-country_2023-11-21.txt"))
  world_shp <- sf::read_sf(here::here("data", "external", "world_shp")) # shape file of the world
  
  ## ---- FORMAT DATA 
  
    # --- Subset to relevant rows and get the affiliation
    oroAffiliations <- pred_relevance %>%
      filter(0.5 <= relevance_mean) %>%
      left_join(uniquerefs, by = "analysis_id") %>%
      select(analysis_id, affiliation) %>%
      collect()
    
    # --- Extract the country of the first author for each relevant publications
    data_1stA_country <- extract_1stA_affiliation(data         = oroAffiliations, 
                                                  countries_ls = countries_ls) 
    
    tmp <- data_1stA_country$oroAff_1stA |> sample_n(size = 50)
    lapply(data_1stA_country, dim)
    
      # -- Number of publication per country
      ORO_per_country <- data_1stA_country$oroAff_1stA |> 
        dplyr::filter(!is.na(country_aff)) |>
        dplyr::group_by(country_aff) |> 
        dplyr::summarise(Count_ORO = n())|> 
        dplyr::filter(!is.na(country_aff)) 
      
    # --- Format the file with the # of publications on ocean & climate by country as found on WOS
    oceanClimate_byCountry <- number_OandC_paper_formating(data         = numb_OandApub_per_country,
                                                           countries_ls = countries_ls)
    
    # --- Compute the ratio #ORO pub per country/#O&C pub per country
    ratio_ORO_totPub <- oceanClimate_byCountry |> 
      dplyr::left_join(ORO_per_country, by = c("Country" = "country_aff")) |> 
      dplyr::mutate(Count_ORO = ifelse(is.na(Count_ORO), 0 , Count_ORO),
                    layer     = (Count_ORO/Record.Count)*100) |> 
      dplyr::left_join(countries_ls[, c("name_en", "alpha3")], by = c("Country" = "name_en"))
    
    # --- Format the shapefile of the world countries polygon and bind data
    world_shp_boundaries <- format_shp_of_the_world(world_shp    = world_shp,
                                                    data_to_bind = ratio_ORO_totPub,
                                                    PROJ         = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
    # --- Format the data to produce the map
    data_2_map_panelA <- format_data2map(data = world_shp_boundaries,
                                         PROJ = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
  ## ---- PLOT PANEL A
  panelA <- univariate_map(data_map          = data_2_map_panelA,
                           color_scale       = viridis::magma(10, direction = -1),
                           vals_colors_scale = NULL,
                           legend            = "#ORO/#O&C (%)",
                           show.legend       = TRUE,
                           name              = "main/Fig2_panelA")
### -----

### ----- PANEL B -----

  ## ---- LOAD DATA
  uniquerefs <- tbl(dbcon, "uniquerefs") # metadata on the unique references
  pred_oro_branch <- tbl(dbcon, "pred_oro_branch") # predictions for ORO branch
  pred_relevance <- tbl(dbcon, "pred_relevance") # which articles are relevant to OROs
  countries_ls <- read.csv(file = here::here("data", "external", "list_of_countries", "sql-pays.csv"), sep = ";") # Countries names
  world_shp <- sf::read_sf(here::here("data", "external", "world_shp")) # shape file of the world
  
  
  ## ---- FORMAT DATA 
    
    # --- Just simplify and create a column that idenfies with 1 or 0 whether 
    # --- a publication is relevant for mitiagion or adaptation
    mitAdaptPubs <- pred_oro_branch %>% 
      left_join(pred_relevance, by = "analysis_id") |> 
      filter(0.5 <= relevance_mean) %>%
      mutate(adaptation = ifelse(0.5 <= `oro_branch.Nature - mean_prediction` |
                                 0.5 <= `oro_branch.Societal - mean_prediction`,
                                 1, 0),
             mitigation = ifelse(0.5 <= `oro_branch.Mitigation - mean_prediction`, 1 ,0)) %>% 
      select(analysis_id, adaptation, mitigation) %>%
      left_join(uniquerefs %>% select(analysis_id, affiliation), by = "analysis_id") %>%
      collect()
    
    # --- Extract the country of the first author for each relevant publications
    data_1stA_country_MA <- extract_1stA_affiliation(data         = mitAdaptPubs, 
                                                     countries_ls = countries_ls) 
    
    tmp <- data_1stA_country_MA$oroAff_1stA |> sample_n(size = 50)
    lapply(data_1stA_country_MA, dim)
    
    # --- Ratio # of mitigation publications over # of adaptation publications
    ratio_mitig_adapt <- data_1stA_country_MA$oroAff_1stA |> 
      group_by(country_aff) |> 
      summarise(adaptation = sum(adaptation, na.rm = TRUE),
                mitigation = sum(mitigation, na.rm = TRUE)) |> 
      mutate(ratio   = mitigation/adaptation,
             mit_ada = mitigation + adaptation,
             layer   = (mitigation/mit_ada)*100) |>  # % mitigation
      rename(Country = country_aff)
    
    # --- Format the shapefile of the world countries polygon and bind data
    world_shp_boundaries_MA <- format_shp_of_the_world(world_shp    = world_shp,
                                                       data_to_bind = ratio_mitig_adapt,
                                                       PROJ         = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
    # --- Format the data to produce the map
    data_2_map_panelB <- format_data2map(data = world_shp_boundaries_MA,
                                         PROJ = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
  ## ---- PLOT PANEL B
  panelB <- univariate_map(data_map          = data_2_map_panelB,
                           color_scale       = c("#4c4680", "#7670a8", "white", "#35a7d9", "#197da8"),
                           vals_colors_scale = c(0, 0.3, 0.5, 0.7, 1),
                           legend            = "% mit. ORO",
                           show.legend       = TRUE,
                           name              = "main/Fig2_panelB")
  
### -----
  
### ----- PANEL C -----
  
  ## ---- FORMAT DATA
  
    # --- Find the predominant ORO type (adaptation vs. mitigation)
    # --- And bind all data together
    data_panelC <- ratio_mitig_adapt |> 
      mutate(dominant_ORO = forcats::fct_relevel(case_when(layer >  50 ~ "Mitigation",
                                                           layer == 50 ~ "50/50",
                                                           layer <  50 ~ "Adaptation"),
                                                 "Adaptation", "50/50", "Mitigation")) |> 
      full_join(oceanClimate_byCountry, by = "Country") |> 
      full_join(ORO_per_country, by = c("Country" = "country_aff"))
  
    # --- Remove NA and Data transformation if needed
    # --- Identify the points that are the farthest from the OLS
    hist(data_panelC_noNA$Record.Count) # Data skewed to the right => log10 transformation
    hist(data_panelC_noNA$Count_ORO) # Data skewed to the right => log10 transformation
  
    # For some countries (e.g., Greenland), one ref identified in the mit_adap df.
    # But not selected in the oroAffiliations df because level of relevance too low.
    

  ## ---- PLOT PANEL C
  panelC <- biplot_fig2c(data        = data_panelC,
                         ylab        = "# ORO publication",
                         xlab        = "# O&C publication",
                         color_scale       = c("#4c4680", "#7670a8", "grey70", "#35a7d9", "#197da8"),
                         vals_colors_scale = c(0, 0.3, 0.5, 0.7, 1),
                         log.transf  = TRUE,
                         quant.prob  = 0.85, 
                         name        = "main/test") ; panelC  

  
### -----

### ----- DISCONNECT -----
DBI::dbDisconnect(dbcon)



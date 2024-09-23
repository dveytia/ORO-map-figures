#########################################################################################
#                                                                                       #
# FIGURE 2                                                                              #
# Panel A: Map of # ORO publications/# publications on ocean & climate  in each country #  
# Panel B: Map: temporal trends of the # of publications per country (slope value)      #
# Panel C: Scatter plot of the ratio of mitigation/adaptation ORO publications          #
# Panel D: Type of OROs studied in relation to GDP per country                          #
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
library(countrycode)
library(broom)
library(ggrepel)
library(nlme)
library(tibble)

### ----- LOAD FUNCTIONS -----
source(here::here("R", "functions_to_format.R")) # all functions needed to format data
source(here::here("R", "functions_to_plot.R")) # all functions needed to plot data


### ----- CONNECTION TO THE LATEST VERSION OF THE SQL DATABASE -----
sqliteDir <- here::here("data/sqlite-databases")
sqliteFiles <- dir(sqliteDir)
sqliteVersions <- as.numeric(gsub(".sqlite","",substring(sqliteFiles, regexpr("_v", sqliteFiles) + 2)))
latestVersion <- sqliteFiles[which.max(sqliteVersions)]
dbcon <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(sqliteDir, latestVersion), create=FALSE)


## ------- Other objects to load first --------
countries_ls <- read.csv(file = here::here("data", "external", "list_of_countries", "sql-pays.csv"), sep = ";") |>  # Countries names
  dplyr::mutate(country = countrycode(sourcevar   = name_en,
                                      origin      = "country.name",
                                      destination = "country.name"),
                iso_code = countrycode(sourcevar   = country,
                                       origin      = "country.name",
                                       destination = "iso3c"))

### ----- PANEL A (#ORO papers) -----

  ## ---- LOAD DATA
  uniquerefs <- tbl(dbcon, "uniquerefs") # metadata on the unique references
  pred_relevance <- tbl(dbcon, "pred_relevance")  # which articles are relevant to OROs
  numb_OandApub_per_country <- read.delim(here::here("data/external/ocean-and-climate-publications/WOS_ocean-and-climate_by-country_2023-11-21.txt"))
  world_shp <- sf::read_sf(here::here("data", "external", "world_shp"))  # shape file of the world
  countries_ls <- read.csv(file = here::here("data", "external", "list_of_countries", "sql-pays.csv"), sep = ";") |>  # Countries names
    dplyr::mutate(country = countrycode(sourcevar   = name_en,
                                        origin      = "country.name",
                                        destination = "country.name"),
                  iso_code = countrycode(sourcevar   = country,
                                         origin      = "country.name",
                                         destination = "iso3c"))
  
  # --- LDC = Least Developed Countries
  # --- LLCD = Land Locked Developing Countries
  # --- SIDS = Small Island Developing States
  
    # --- Land locked country. Use countrycode::countrycode() to give the same name to countries for all different databasess
    landlocked <- read.csv(file = here::here("data", "external", "special_country_groups", "landlocked-countries-2024.csv"), sep = ",") |> 
      dplyr::select(country, LandlockedCountries) |> 
      rename(Country    = country, 
             group_land = LandlockedCountries) |> 
      mutate(iso_code = countrycode(sourcevar   = Country,
                                    origin      = "country.name",
                                    destination = "iso3c"),
             group_land = case_when(group_land == "yes" ~ "Land-locked",
                                    TRUE ~ "NA"))
  
    # --- AMUNRC: Associate Members of United Nations Regional Commissions
    AMUNRC <- c("American Samoa", "Anguilla", "Aruba", "Bermuda", "British Virgin Islands", "Cayman Islands", "Commonwealth of Northern Marianas",
                "Curacao", "French Polynesia", "Guadeloupe", "Guam", "Martinique", "Montserrat", "New Caledonia", "Puerto Rico", "Sint Maarten",
                "Turks and Caicos Islands", "United States Virgin Islands")
  
    # --- Category of each country (SIDS, land-locked, coastal)
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
    
    # --- Shape file of countrie's EEZ
    eez_shp <- sf::st_read(here::here("data", "external", "eez_shp", "eez_v12.shp")) |>  
      dplyr::select(MRGID, POL_TYPE, TERRITORY1, SOVEREIGN1, ISO_SOV1) |> 
      dplyr::rename(Country = SOVEREIGN1) |> 
      dplyr::mutate(Country = countrycode(sourcevar   = ISO_SOV1,
                                          origin      = "iso3c",
                                          destination = "country.name"),
                    iso_code = countrycode(sourcevar   = Country,
                                           origin      = "country.name",
                                           destination = "iso3c")) |> 
      full_join(country_grp |>  dplyr::select(-iso_code), by = c("TERRITORY1" = "Country")) |> 
      mutate(group_land = case_when(Country == "Cape Verde" ~ "Island", TRUE ~ group_land))
  
  
  ## ---- FORMAT DATA 
  
    # --- Subset to relevant rows and get the affiliation
    oroAffiliations <- pred_relevance %>%
      filter(0.5 <= relevance_mean) %>%
      left_join(uniquerefs, by = "analysis_id") %>%
      select(analysis_id, affiliation, year) %>%
      collect()
    
    # --- Extract the country of the first author for each relevant publications
    data_1stA_country <- extract_1stA_affiliation(data         = oroAffiliations, 
                                                  countries_ls = countries_ls)
    
    # -- Number of publication per country
    ORO_per_country <- data_1stA_country$oroAff_1stA |> 
      dplyr::filter(!is.na(country_aff)) |> 
      dplyr::mutate(country_aff = countrycode(sourcevar   = country_aff,
                                              origin      = "country.name",
                                              destination = "country.name"),
                    iso_code = countrycode(sourcevar   = country_aff,
                                           origin      = "country.name",
                                           destination = "iso3c")) |>
      dplyr::group_by(country_aff, iso_code) |> 
      dplyr::summarise(Count_ORO = n()) 
    
    # --- Format the file with the # of publications on ocean & climate by country as found on WOS
    oceanClimate_byCountry <- number_OandC_paper_formating(data         = numb_OandApub_per_country,
                                                           countries_ls = countries_ls)
    
    # --- Compute the ratio #ORO pub per country/#O&C pub per country
    ratio_ORO_totPub <- oceanClimate_byCountry |> 
      full_join(ORO_per_country |> ungroup() |> select(-country_aff), by = "iso_code") |> 
      replace_na(list(Count_ORO = 0))|> 
      mutate(Count_ORO = ifelse(is.na(Count_ORO), 0 , Count_ORO),
             layer     = (Count_ORO/Record.Count)*100) 
    
      # Save for Devi
      # save(ratio_ORO_totPub, file = here::here("data", "ratio_ORO_totPub.RData"))
    
    
    
    
    # ---------- DEVI MODEL ratio of ORO to ocean and climate by country type
    
    # No significant difference in proportion if SIDS or not
    load(here::here("data", "ratio_ORO_totPub.RData"))
    ratio_ORO_totPub_modDf <- ratio_ORO_totPub %>%
      mutate(propORO = layer/100) %>%
      left_join(country_grp, by = "iso_code") %>%
      mutate(SIDS = ifelse(group_land == "SIDS", "SIDS","Other")) %>%
      mutate(SIDS = ifelse(is.na(SIDS), "Other", SIDS)) %>%
      mutate(SIDS = factor(SIDS, levels = c("Other","SIDS")))
    
    summary(ratio_ORO_totPub_modDf)
    
    ggplot(ratio_ORO_totPub_glm, aes(x = SIDS, y = propORO))+
      geom_boxplot()
    
    ratio_ORO_totPub_glm <- glm(propORO ~ SIDS, weights = Record.Count, family = "binomial", data = ratio_ORO_totPub_modDf)
    summary(ratio_ORO_totPub_glm)
    
    
    # Instead try by climate zone
    # download ascii file from http://koeppen-geiger.vu-wien.ac.at/
    # Or maybe current and future https://www.nature.com/articles/sdata2018214
    # Used National Aggregates of Geospatial Data Collection: Population, Landscape and Climate Estimaes v2 (1990-2000)
    # https://sedac.ciesin.columbia.edu/data/set/nagdc-population-landscape-climate-estimates-v2/data-download
    
    ## read in climate zone data
    climZoneDf <- readxl::read_excel(here::here("data/external/nagdc-population-landscape-climate-estimates-v2.xls"), sheet = "AREA2000", col_names = TRUE, trim_ws = TRUE)
    # Subset to columns of interest
    climZoneDf <- climZoneDf[,c(1,grep("PARCZ", colnames(climZoneDf)),186)] %>%
      select(-PARCZ00) %>%
      rename(iso_code = ISO3V10, total_sq_km = T_ARCZ)
    # Mutate to long format
    climZoneDf <- reshape2::melt(climZoneDf, id.vars = c("iso_code", "total_sq_km"), variable.name = "climate_zone_S", value.name = "perc_land_area")
    climZoneDf$prop_land_area = climZoneDf$perc_land_area/100
    # Classify columns by larger zone
    zoneLookup <- list(
      paste0("PARCZ", 1:6),
      paste0("PARCZ", 7:10),
      paste0("PARCZ", 11:26),
      paste0("PARCZ", 27:37),
      paste0("PARCZ", 38:41),
      paste0("PARCZ", 42:45)
    )
    names(zoneLookup) <- c("Tropical", "Polar","Temperate","Cold","Dry","Desert")
    zoneLookup <- lapply(zoneLookup, function(x) {data.frame(climate_zone_S = x)})
    zoneLookup <- mapply(cbind, zoneLookup, "climate_zone_L"=c("Tropical", "Polar","Temperate","Cold","Dry","Desert"),SIMPLIFY = F)
    zoneLookup <- do.call(rbind.data.frame, zoneLookup)
    climZoneDf <- climZoneDf %>%
      left_join(zoneLookup, by = "climate_zone_S")
    
    ## Join climate zone data to ratio data
    ratio_ORO_totPub_modDf <- ratio_ORO_totPub %>%
      mutate(propORO = layer/100) %>%
      left_join(country_grp, by = "iso_code") %>%
      mutate(SIDS = ifelse(group_land == "SIDS", "SIDS","Other")) %>%
      mutate(SIDS = ifelse(is.na(SIDS), "Other", SIDS)) %>%
      mutate(SIDS = factor(SIDS, levels = c("Other","SIDS"))) %>%
      left_join(climZoneDf, by = "iso_code") %>%
      mutate(climate_tropical = ifelse(climate_zone_L == "Tropical","Tropical","Other")) %>%
      mutate(climate_zone_L_f = factor(climate_zone_L, levels = c("Polar", "Cold","Temperate","Dry","Desert","Tropical")),
             climate_tropical_f = factor(climate_tropical, levels = c("Other","Tropical")))

    summary(ratio_ORO_totPub_modDf)
    
    
    ## Fit models
    
    # The likelihood of publishing an ORO as a function of % land area classified as tropical
    ratio_ORO_totPub_percTropical_glm <- glm(propORO ~ perc_land_area, weights = Record.Count, family = "binomial", data = ratio_ORO_totPub_modDf, subset = climate_zone_L == "Tropical")
    summary(ratio_ORO_totPub_percTropical_glm)
    
    ggplot()+
      geom_point(data = ratio_ORO_totPub_modDf %>%
                   filter(climate_zone_L == "Tropical"),
                 aes(x = perc_land_area, y = propORO)) # try log odds ratio instead
    
    # # Extract the summary statistics for just the slope (year)
    # summaryTable <- do.call(rbind, lapply(modFits, function(x) summary(x)$tTable['year_num',]))
    # summaryTable <- summaryTable |> 
    #   as.data.frame() |> 
    #   arrange(desc(Value)) |> 
    #   mutate(Coefficient = "Year") |> 
    #   # country_aff = names(modFits)) |>
    #   tibble::rownames_to_column(var = "country") |>
    #   select(country, Coefficient, Value, Std.Error, `t-value`, `p-value`) |> 
    #   mutate(iso_code = countrycode(sourcevar   = country,
    #                                 origin      = "country.name",
    #                                 destination = "iso3c"))
    # # layer    = cut()) 
    # # filter(`p-value` <= 0.05) |> 
    # # rename(layer = Value) 
    # 
    # ## Save to outputs
    # write.csv(summaryTable, here::here("outputs/glsCountryPublicationTrendsSummaryTable.csv"))
    
    
    # ----- End DEVI MODELLING -----------------------------
    
    
    # --- Format the shapefile of the world countries polygon and bind data
    world_shp_boundaries <- format_shp_of_the_world(world_shp    = world_shp,
                                                    data_to_bind = ratio_ORO_totPub,
                                                    PROJ         = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") |> 
      left_join(country_grp |>  select(-Country), by = "iso_code") |> 
      select(-country.y) |> 
      rename(country = country.x) |> 
      mutate(group_land = case_when(group_land %in% c("Land-locked", "SIDS", "Coastal") ~ group_land,
                                    !is.na(group_land) & NA2_DESCRI != country ~ "Island",
                                    is.na(group_land)  & NA2_DESCRI != country ~ "Island",
                                    is.na(group_land)  & NA2_DESCRI == country ~ "Coastal"))
    
    # --- Format the shapefile of the eez countries polygon and bind data
    eez_shp_islands <- full_join(eez_shp, ratio_ORO_totPub, by = "iso_code") |> 
      # left_join(country_grp |>  select(-Country), by = "iso_code") |> 
      mutate(country = str_replace_all(country, c("Côte d’Ivoire" = "Ivory Coast",
                                                  "Congo - Brazzaville" = "Republic of the Congo",
                                                  "Congo - Kinshasa"    = "Democratic Republic of the Congo",
                                                  "Somalia"             = "Federal Republic of Somalia")),
             group_land = case_when(group_land %in% c("Island", "Land-locked", "SIDS", "Coastal") ~ group_land,
                                    !is.na(group_land) & TERRITORY1 != country ~ "Island",
                                    is.na(group_land)  & TERRITORY1 != country ~ "Island",
                                    is.na(group_land)  & TERRITORY1 == country ~ "Coastal")) |> 
      filter(group_land %in% c("Island", "SIDS", "AMUNRC") & !is.na(MRGID) & !is.na(layer)) |>
      filter(group_land == "SIDS" & !is.na(MRGID) & !is.na(layer)) |>
      filter(! TERRITORY1 %in% c("French Guiana", "Greenland")) |> 
      sf::st_transform(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
    # --- Format the data to produce the map
    data_2_map_panelA <- format_data2map(data = world_shp_boundaries,
                                         PROJ = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    

  ## ---- PLOT PANEL A 
  panelA <- univariate_map(data_map          = data_2_map_panelA,
                           eez               = eez_shp_islands,
                           color_scale       = viridis::magma(10, direction = -1),
                           midpoint          = NULL,
                           second.var        = "Count_ORO",
                           # vals_colors_scale = NULL,
                           title_color       = "#ORO/#O&C (%)",
                           title_size        = "#ORO paper",
                           show.legend       = TRUE,
                           name              = "main/map_ORO_O&Cpapers")
  
### -----
    
### ----- Panel B (Trends)-----
  
  ## ---- LOAD DATA
  uniquerefs <- tbl(dbcon, "uniquerefs") # metadata on the unique references
  pred_relevance <- tbl(dbcon, "pred_relevance")  # which articles are relevant to OROs
  
  ## ---- FORMAT DATA
  
    # --- Subset to relevant rows and get the affiliation
    oroAffiliations <- pred_relevance %>%
      filter(0.5 <= relevance_mean) %>%
      left_join(uniquerefs, by = "analysis_id") %>%
      select(analysis_id, affiliation, year) %>%
      collect()
    
    # --- Extract the country of the first author for each relevant publications
    data_1stA_country <- extract_1stA_affiliation(data         = oroAffiliations, 
                                                  countries_ls = countries_ls)
  
    # -- Number of publication per country and per year
    ORO_per_country_year <- data_1stA_country$oroAff_1stA |> 
      dplyr::filter(!is.na(country_aff)) |> 
      dplyr::mutate(country_aff = countrycode(sourcevar   = country_aff,
                                              origin      = "country.name",
                                              destination = "country.name"),
                    iso_code = countrycode(sourcevar   = country_aff,
                                           origin      = "country.name",
                                           destination = "iso3c")) |>
      dplyr::filter(!is.na(year)) |> 
      dplyr::group_by(country_aff, iso_code, year) |> 
      dplyr::summarise(Count_ORO = n()) 
    
    
  ## ---- DEVI, YOU CAN PUT THE CODE TO MODEL #OROpub ~ f(year) HERE
  ## ---- You can use the dataframe called ORO_per_country_year
  ORO_per_country_year_trends_df <- ORO_per_country_year %>%
    mutate(year_num = as.numeric(year),
           log_articles = log(Count_ORO)) %>%
    filter(1980 <= year & year <= 2022 & is.finite(log_articles))
  
    # Function to fit simple overall exponential trend with corAR1 autocorrelation
    corAR1_fit <- function(dat){
      tryCatch(
        {
          suppressWarnings(gls(log_articles ~ year_num, data = dat, correlation = corAR1()))
        },
        error = function(cond){
          paste("Error message:", message(conditionMessage(cond)), collapse = " ")
        },
        warning = function(cond){
          paste("Warning message:", message(conditionMessage(cond)), collapse = " ")
        },
        finally = {
          message(paste("Processed", dat$country_aff[1]))
        }
      )
    }
  
    # Split data by country group to fit models separately
    splitDat <- split(ORO_per_country_year_trends_df, 
                      ORO_per_country_year_trends_df$country_aff)
    
    # Fit model to each country
    modFits <- lapply(splitDat, corAR1_fit)
    
    # subset to only the model fits that worked
    modFits <- modFits[which(unlist(lapply(modFits, class)) == "gls")] 
    
    # Extract the summary statistics for just the slope (year)
    summaryTable <- do.call(rbind, lapply(modFits, function(x) summary(x)$tTable['year_num',]))
    summaryTable <- summaryTable |> 
      as.data.frame() |> 
      arrange(desc(Value)) |> 
      mutate(Coefficient = "Year") |> 
             # country_aff = names(modFits)) |>
      tibble::rownames_to_column(var = "country") |>
      select(country, Coefficient, Value, Std.Error, `t-value`, `p-value`) |> 
      mutate(iso_code = countrycode(sourcevar   = country,
                                    origin      = "country.name",
                                    destination = "iso3c"))
             # layer    = cut()) 
      # filter(`p-value` <= 0.05) |> 
      # rename(layer = Value) 
    
    ## Save to outputs
    write.csv(summaryTable, here::here("outputs/glsCountryPublicationTrendsSummaryTable.csv"))
    
    
    
    
    ##### DEVI -- SUB-ANALYSIS OF TRENDS BY COUNTRY CHARACTERISTICS #--------------------------
    summaryTable <- read.csv(here::here("outputs/glsCountryPublicationTrendsSummaryTable.csv"))
    
    # join with column to indicate country type: land locked, coastal, SIDS
    landlocked <- read.csv(file = here::here("data", "external", "special_country_groups", "landlocked-countries-2024.csv"), sep = ",") |> 
      select(country, LandlockedCountries) |> 
      rename(Country    = country, 
             group_land = LandlockedCountries) |> 
      mutate(iso_code = countrycode::countrycode(sourcevar   = Country,
                                    origin      = "country.name",
                                    destination = "iso3c"),
             group_land = case_when(group_land == "yes" ~ "Land-locked",
                                    TRUE ~ "NA")) 
    
    AMUNRC <- c("American Samoa", "Anguilla", "Aruba", "Bermuda", "British", "Virgin Islands", "Cayman Islands", "Commonwealth of Northern Marianas",
                "Curacao", "French Polynesia", "Guadeloupe", "Guam", "Martinique", "Montserrat", "New Caledonia", "Puerto Rico", "Sint Maarten",
                "Turks and Caicos Islands", "United States Virgin Islands")
    SIDS <- read.csv(file = here::here("data", "external", "special_country_groups", "special-country-groups.csv"), sep = ",")
    SIDS <- SIDS %>%
      mutate(iso_code = countrycode::countrycode(sourcevar   = Country,
                                                 origin      = "country.name",
                                                 destination = "iso3c"))
    
    country_grp <-  SIDS |> 
      select(Country, LLDC, SIDS) |> 
      mutate(iso_code = countrycode::countrycode(sourcevar   = Country,
                                    origin      = "country.name",
                                    destination = "iso3c"),
             group_land = case_when(LLDC == "No" & SIDS == "Yes" ~ "SIDS",
                                    LLDC == "Yes" & SIDS == "No" ~ "Land-locked",
                                    TRUE ~ "Coastal")) |> 
    
      select(-LLDC, -SIDS) |> 
      rbind(landlocked) |> 
      distinct() |> 
      mutate(group_land = case_when(Country %in% AMUNRC ~ "AMUNRC",
                                    TRUE ~ group_land)) |> 
      filter(Country != "Bolivia (Plurinational State of)") |> 
      rename(country = Country)
    
    unique(country_grp$group_land)
    
    trendCountryTypeDf <- summaryTable %>%
      #filter(p.value <= 0.05) %>%
      left_join(SIDS[,c("SIDS","iso_code")], by = "iso_code") %>% 
      mutate(SIDS = factor(SIDS, levels = c("Yes","No"))) %>%
      left_join(country_grp[,c("iso_code","group_land")], by = c("iso_code")) %>%
      #mutate(group_land = ifelse(SIDS == "Yes", "SIDS", group_land)) %>%
      mutate(group_land = factor(group_land, levels = c("Land-locked","Coastal","SIDS"))) %>%
      mutate(significant = ifelse(p.value <= 0.05, TRUE, FALSE))
    
    # Number of SIDS is 13, but number of sids in the group_land column is 9...ffor some reason they are NA in the group_land column
    summary(trendCountryTypeDf)
    trendCountryTypeDf %>%
      filter(SIDS == "Yes" & is.na(group_land)) %>%
      print()
    
    # Other countries are also NA in the group_land classification
    View(trendCountryTypeDf[is.na(trendCountryTypeDf$group_land),])
    
    # So actually SIDS have lower and insignificant trend in ORO publication
    ggplot(trendCountryTypeDf, aes(x=SIDS, y = Value))+
      geom_boxplot()+
      geom_jitter(aes(col = significant))
    

    
    
    
    
    
  ## ---- Format map data
    
    # --- Format the shapefile of the world countries polygon and bind data
    world_shp_boundaries <- format_shp_of_the_world(world_shp    = world_shp,
                                                    data_to_bind = data_trends,
                                                    PROJ         = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") |> 
      left_join(country_grp |>  select(-Country), by = "iso_code") |> 
      select(-country.y) |> 
      rename(country = country.x) |> 
      mutate(group_land = case_when(group_land %in% c("Land-locked", "SIDS", "Coastal") ~ group_land,
                                    !is.na(group_land) & NA2_DESCRI != country ~ "Island",
                                    is.na(group_land)  & NA2_DESCRI != country ~ "Island",
                                    is.na(group_land)  & NA2_DESCRI == country ~ "Coastal"))
    
    # --- Format the shapefile of the eez countries polygon and bind data
    eez_shp_islands <- full_join(eez_shp, data_trends, by = "iso_code") |> 
      # left_join(country_grp |>  select(-Country), by = "iso_code") |> 
      mutate(country = str_replace_all(country, c("Côte d’Ivoire" = "Ivory Coast",
                                                  "Congo - Brazzaville" = "Republic of the Congo",
                                                  "Congo - Kinshasa"    = "Democratic Republic of the Congo",
                                                  "Somalia"             = "Federal Republic of Somalia")),
             group_land = case_when(group_land %in% c("Island", "Land-locked", "SIDS", "Coastal") ~ group_land,
                                    !is.na(group_land) & TERRITORY1 != country ~ "Island",
                                    is.na(group_land)  & TERRITORY1 != country ~ "Island",
                                    is.na(group_land)  & TERRITORY1 == country ~ "Coastal")) |> 
      filter(group_land %in% c("Island", "SIDS", "AMUNRC") & !is.na(MRGID) & !is.na(layer)) |>
      filter(group_land == "SIDS" & !is.na(MRGID) & !is.na(layer)) |>
      filter(! TERRITORY1 %in% c("French Guiana", "Greenland")) |> 
      sf::st_transform(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
    # --- Format the data to produce the map
    data_2_map_panelB <- format_data2map(data = world_shp_boundaries,
                                         PROJ = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
  ## ---- PLOT PANEL B 
  panelB <- univariate_map(data_map          = data_2_map_panelB,
                           eez               = eez_shp_islands,
                           color_scale       = "trends",
                           midpoint          = NULL,
                           second.var        = NULL,
                           # vals_colors_scale = NULL,
                           title_color       = "Slope",
                           title_size        = NULL,
                           show.legend       = TRUE,
                           name              = "main/map_temporal_trends_test")
  
### -----
    
  
### ----- Panel C -----
  
  ## ---- LOAD DATA
  pred_oro_branch <- tbl(dbcon, "pred_oro_branch") # predictions for ORO branch
  load(here::here("data", "ratio_ORO_totPub.RData"))
    
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
    
    # --- Ratio # of mitigation publications over # of adaptation publications
    ratio_mitig_adapt <- data_1stA_country_MA$oroAff_1stA |> 
      mutate(country_aff = countrycode(sourcevar   = country_aff,
                                       origin      = "country.name",
                                       destination = "country.name"),
             iso_code = countrycode(sourcevar   = country_aff,
                                    origin      = "country.name",
                                    destination = "iso3c")) |>
      group_by(country_aff, iso_code) |> 
      summarise(adaptation = sum(adaptation, na.rm = TRUE),
                mitigation = sum(mitigation, na.rm = TRUE)) |> 
      mutate(ratio   = mitigation/adaptation,
             mit_ada = mitigation + adaptation,
             layer   = (mitigation/mit_ada)*100) |>  # % mitigation
      rename(Country = country_aff) 
    
  
    # --- Find the predominant ORO type (adaptation vs. mitigation)
    # --- And bind all data together
    data_panelC <- ratio_ORO_totPub |> 
      rename(ORO_OC = layer) |> 
      full_join(ratio_mitig_adapt |>  rename(perc_mit = layer), by = "iso_code") |> 
      select(-Country, -ratio, -mit_ada) |> 
      # left_join(income_grp |>  select(-country), by = "iso_code") |>
      left_join(country_grp |> select(iso_code, group_land), by = "iso_code") |>
      replace_na(list(group_land = "Coastal")) |>
      filter(!is.na(country) & group_land != "AMUNRC") |> 
      filter(!is.na(perc_mit)) |> 
      mutate(adaptation_log = log(adaptation + 1),
             mitigation_log = log(mitigation + 1),
             labels         = case_when(adaptation >= mitigation ~ "Adaptation",
                                        TRUE ~ "Mitigation")) |> 
      ungroup()
    
    # --- Data for donuts
    n_LL = sum(data_panelC$group_land == "Land-locked")
    n_C = sum(data_panelC$group_land == "Coastal")
    n_SIDS = sum(data_panelC$group_land == "SIDS")
    
    data_donut = data_panelC |> 
      group_by(group_land, labels) |> 
      summarise(count = n()) |> 
      mutate(total_count = case_when(group_land == "Coastal" ~ n_C,
                                     group_land == "SIDS" ~ n_SIDS,
                                     TRUE ~ n_LL),
             perc = (count/total_count)*100) |> 
      group_split(group_land)
    
  ## ---- DEVI, YOU CAN PUT THE CODE FOR THE MODEL HERE
  ## Model the odds ratio of Mitigation vs Adaptation for each country type
  panelC_fit_Df <- data_panelC %>%
    select(group_land, mitigation, adaptation) %>%
    mutate(group_land = factor(group_land, levels = c("Land-locked","Coastal","SIDS")))
  
  panelC_fit <- glm(cbind(mitigation, adaptation) ~ group_land, family = binomial, data = panelC_fit_Df)
  panelC_fit_sum <- summary(panelC_fit) 
  print(panelC_fit_sum)
  
  # ^These results report the odds ratio (on the link scale) relative to the land group.
  # To interpret, put back on response scale by taking the exponent
  exp(panelC_fit_sum$coefficients[,"Estimate"]) # odds ratio on response scale
  (exp(panelC_fit_sum$coefficients[,"Estimate"])-1)*100 # expressed as a percentage
  
  # Interpretation:
  # Land-locked countries are 5.2 times (420 %) more likely to favor mitigation measures (p << 0.01)
  # Compared to land-locked, Coastal countries are 0.85 times the odds (-15 %) to favor mitigation (not sig, p = 0.4)
  # Compared to land-locked, SIDS have 0.41 times the odds (-58%) of using a mitigation measure (p << 0.01)
  
  # Does country group as a whole predict ratio?
  # Yes, country group has a statistically significant effect on mitigation/adaptation ratio (p << 0.01)
  drop1(panelC_fit, .~., test="Chisq")
  
  # Export model results to a text file
  sink(here::here("outputs/mitigationVsAdaptationByCountryTypeBinomialModel.txt"))
  print("## MODEL SUMMARY")
  print(panelC_fit_sum)
  print("## COEFFICIENT TRANSFORMATIONS")
  print("Odds ratio on the response scale (i.e. exp(B))")
  exp(panelC_fit_sum$coefficients[,"Estimate"]) # odds ratio on response scale
  print("Expressed as a percentage (i.e. (exp(B)-1)*100)")
  (exp(panelC_fit_sum$coefficients[,"Estimate"])-1)*100 # as a percentage
  print("## GLOBAL SIGNIFICANCE OF COUNTRY TYPE TERM")
  drop1(panelC_fit, .~., test="Chisq")
  sink()
  
    
  ## ---- PLOT PANEL C
    
    # --- Biplot adaptation ~ f(mitigation)
    biplot <- biplot_fig2c(data        = data_panelC,
                           var.y       = "adaptation",
                           var.x       = "mitigation",
                           var.col     = "group_land",
                           # var.shape   = "group_land",
                           ylab        = "# ada. pubs",
                           xlab        = "# mit. pubs",
                           one_one_line = TRUE,
                           # color_scale = c("#4c4680","white", "#197da8"),
                           color_title = "#ORO/#O&C (%)",
                           log.transf  = TRUE,
                           quant.prob  = 0.90, 
                           name        = "main/biplot_adapt_mitig") 
    
    # --- Donuts % of mitigation per country type (LL, Coastal, SIDS)
    plot_donuts_ls <- lapply(data_donut, function(x){
      
      x_plot <- x |> 
        select(-group_land) |> 
        mutate(Type     = "MitAda",
               cat = factor(labels, levels = c("Mitigation", "Adaptation"))) |> 
        arrange(cat) |> 
        mutate(pos = round(cumsum(perc) - (0.5 * perc), 2))  
      
      ggplot(x_plot, aes(x = Type, y = perc, fill = labels)) +
        geom_col(show.legend = F) +
        geom_text(aes(label = paste0(round(perc, 1), "%"), x = Type, y = pos), size = 6, color = "white") +
        # Colors
        scale_fill_manual(name   = NULL,
                          values = c("Adaptation"  = "#4c4680",
                                     "Mitigation" = "#197da8")) +
        scale_x_discrete(limits = c(" ", "MitAda")) +
        coord_polar("y") +
        theme_void()
      
    })
    
    # --- Get plot legend
    legend = ggpubr::get_legend(
      ggplot(data_donut[[1]], aes(x = labels, y = perc, fill = labels)) +
        geom_col() +
        scale_fill_manual(name = NULL, 
                          values = c("Adaptation"  = "#4c4680",
                                     "Mitigation" = "#197da8"),
                          labels = c("Adaptation" = "Above (Adaptation)",
                                     "Mitigation" = "Below (Mitigation)")) +
        theme(legend.direction = "horizontal",
              legend.text     = element_text(size = 16, face  = "bold"),
              legend.background = element_blank()))
    
    
    # --- Arrange plot
    plot_final <- cowplot::ggdraw() +
      cowplot::draw_plot(biplot, x = 0.0, y = 0.0, width = 0.60, height = 1.0) +
      cowplot::draw_plot(plot_donuts_ls[[1]], x = 0.50, y = 0.45, width = 0.4, height = 0.4) +
      cowplot::draw_plot(plot_donuts_ls[[2]], x = 0.70, y = 0.45, width = 0.4, height = 0.4) +
      cowplot::draw_plot(plot_donuts_ls[[3]], x = 0.60, y = 0.05, width = 0.4, height = 0.4) +
      cowplot::draw_text(text = c("Coastal", "Land-locked", "SIDS"),
                         fontface = rep("bold", 3),
                         color = c("#3f47e8", "#b36705", "#0fbcd6"),
                         x = c(0.70, 0.90, 0.80), 
                         y = c(0.47, 0.47, 0.06), size = 18) +
      # y = c(0.52, 0.52, 0.12), size = 16) +
      cowplot::draw_text(text = "Percentage above the 1:1 line", x = 0.8, y = 0.95, size = 19, fontface = "bold") +
      cowplot::draw_plot(legend, x = 0.65, y = 0.82, width = 0.3, height = 0.1) ; plot_final
    
    # ggplot2::ggsave(plot = plot_final, here::here("figures", "main", "donuts_test.jpeg"), width = 15, height = 5, device = "jpeg")
    # ggplot2::ggsave(plot = plot_final, here::here("figures", "main", "figure2_panelC_donuts4.jpeg"), width = 13, height = 7, device = "jpeg")
    ggplot2::ggsave(plot = plot_final, here::here("figures", "main", "biplot_donuts_mitigation_vs_adaptation.pdf"), width = 15, height = 7, device = "pdf")
    
    
### -----
    
  
### ----- Panel D -----
    
  ## ---- LOAD DATA
  pred_oro_type_long <- tbl(dbcon, "pred_oro_type_long") %>% collect()
    
    
    # --- GDP per capita
    gdp_per_capita <- readr::read_csv(here::here("data/external/gdp-per-capita/gdp-per-capita-worldbank-2021.csv"), 
                                      show_col_types = FALSE) |>
      filter(Year > 1980) |> 
      rename(country = Entity,
             GDP_per_capita = `GDP per capita, PPP (constant 2017 international $)`) |> 
      group_by(country, Code) |> 
      summarise(GDP_per_capita = median(GDP_per_capita, na.rm = T)) |> 
      mutate(country = countrycode(sourcevar   = country,
                                   origin      = "country.name",
                                   destination = "country.name"),
             iso_code = countrycode(sourcevar   = country,
                                    origin      = "country.name",
                                    destination = "iso3c")) |> 
      filter(!is.na(iso_code) & !is.na(GDP_per_capita))
    
    ccodes <- raster::ccodes() |>  
      select(NAME, continent) |> 
      mutate(country = countrycode(sourcevar   = NAME,
                                   origin      = "country.name",
                                   destination = "country.name"),
             iso_code = countrycode(sourcevar   = country,
                                    origin      = "country.name",
                                    destination = "iso3c")) |> 
      select(iso_code, continent) |> 
      filter(!is.na(iso_code)) |> 
      distinct()
    
    
  ## ---- FORMAT DATA
  country_grp <- country_grp |> 
    filter(Country != "Bolivia (Plurinational State of)") |> 
    select(-Country)
  
    # --- Join with author affiliation data from uniquerefs
    pred_oro_type <- uniquerefs %>%
      # left_join(pred_relevance, by = "analysis_id") |> 
      # filter(0.5 <= relevance_mean) %>% 
      dplyr::select(analysis_id, affiliation) %>%
      collect() %>%
      right_join(pred_oro_type_long, by = "analysis_id")
    
    # --- Find 1st author affiliation
    pred_oro_type_1A <- extract_1stA_affiliation(data         = pred_oro_type, 
                                                 countries_ls = countries_ls)
    
    # --- Select the most relevant oro_type predicted and estimate the # paper per country & oro_branch 
    npaper_per_country_branch <- pred_oro_type_1A$oroAff_1stA |> 
      mutate(country_aff = str_replace_all(country_aff, c("United Kingdoms"  = "United Kingdom",
                                                          "Falkland Island"  = "United Kingdom",
                                                          "Faroe Islands"    = "Denmark",
                                                          "Greenland"        = "Denmark",
                                                          "French Polynesia" = "France",
                                                          "Guadeloupe"       = "France",
                                                          "Monaco"           = "France",
                                                          "New Caledonia"    = "France",
                                                          "Reunion"          = "France",
                                                          "Guam"             = "United States")),
             country_aff = countrycode(sourcevar   = country_aff,
                                       origin      = "country.name",
                                       destination = "country.name"),
             iso_code = countrycode(sourcevar   = country_aff,
                                    origin      = "country.name",
                                    destination = "iso3c")) |> 
      
      group_by(country_aff, iso_code, oro_type) |> # oro_branch
      summarise(n_mean = sum(0.5 <= mean),
                n_lower = sum(0.5 <= lower),
                n_upper = sum(0.5 <= upper)) |> 
      # arrange(country_aff, oro_branch, -n_mean) |>
      rename(oro_branch = oro_type) |> 
      ungroup()
    
    
    # Total data per country
    total <- npaper_per_country_branch |> 
      group_by(country_aff, iso_code) |> 
      summarise(n_mean  = sum(n_mean, na.rm = T) ,
                n_lower = sum(n_lower, na.rm = T) ,
                n_upper = sum(n_upper, na.rm = T)) |> 
      mutate(oro_branch = "# ORO pubs") |> 
      dplyr::select(country_aff, iso_code, oro_branch, n_mean, n_lower, n_upper)
    
    total_perc <- npaper_per_country_branch |> 
      group_by(iso_code) |> 
      summarise(n_mean_sum  = sum(n_mean, na.rm = T) ,
                n_lower_sum = sum(n_lower, na.rm = T) ,
                n_upper_sum = sum(n_upper, na.rm = T))
    
    # Compute percentage per ORO type
    npaper_per_country_type_perc <- npaper_per_country_branch |>
      full_join(total_perc, by = "iso_code") |>
      mutate(n_mean  = (n_mean/n_mean_sum)*100,
             n_lower = (n_lower/n_lower_sum)*100,
             n_upper = (n_upper/n_upper_sum)*100) |>
      select(-n_mean_sum, -n_lower_sum, -n_upper_sum) |>
      rbind(total) |>
      mutate(panel = case_when(oro_branch == "# ORO pubs" ~ "Total number of articles",
                               TRUE ~ "% of total"),
             color = oro_branch) |>
      left_join(country_grp, by = "iso_code") |>
      replace_na(list(group_land = "Coastal")) |>
      left_join(ccodes, by = "iso_code") |>
      mutate(group_land = factor(group_land, levels = c("Coastal", "Land-locked", "SIDS", "AMUNRC")),
             continent  = factor(continent, levels  = c("North America", "Europe", "Asia", "Oceania", "Africa", "South America")))
    
    # Compute percentage per ORO type
    npaper_per_country_type <- pred_oro_type_1A$oroAff_1stA |>
      mutate(country_aff = str_replace_all(country_aff, c("United Kingdoms"  = "United Kingdom",
                                                          "Falkland Island"  = "United Kingdom",
                                                          "Faroe Islands"    = "Denmark",
                                                          "Greenland"        = "Denmark",
                                                          "French Polynesia" = "France",
                                                          "Guadeloupe"       = "France",
                                                          "Monaco"           = "France",
                                                          "New Caledonia"    = "France",
                                                          "Reunion"          = "France",
                                                          "Guam"             = "United States")),
             country_aff = countrycode(sourcevar   = country_aff,
                                       origin      = "country.name",
                                       destination = "country.name"),
             iso_code = countrycode(sourcevar   = country_aff,
                                    origin      = "country.name",
                                    destination = "iso3c")) |>
      group_by(country_aff, iso_code, oro_type) |> # oro_type
      summarise(n_mean = sum(0.5 <= mean),
                n_lower = sum(0.5 <= lower),
                n_upper = sum(0.5 <= upper)) |> 
      ungroup() |> 
      # rename(oro_type = oro_branch) |> 
      left_join(country_grp, by = "iso_code") |> 
      replace_na(list(group_land = "Coastal")) |> 
      left_join(ccodes, by = "iso_code") |> 
      mutate(group_land = factor(group_land, levels = c("Coastal", "Land-locked", "SIDS", "AMUNRC")),
             continent  = factor(continent, levels  = c("North America", "Europe", "Asia", "Oceania", "Africa", "South America"))) 
    
    # ---- Merge GDP and ORO data
    data_plot_bars <- left_join(npaper_per_country_type_perc, gdp_per_capita, by = "iso_code") |> filter(!is.na(GDP_per_capita)) |> ungroup()
    data_plot_donuts <- left_join(npaper_per_country_type, gdp_per_capita, by = "iso_code") |> filter(!is.na(GDP_per_capita))
  
  
    ## ---------- Devi modelling ------------###
    # Try to model probability of publishing adaptation vs mitigation OROs as a function of GDP grouped by country type
    # Using a logistic regression (glm and gam)
    # However, because there are high numbers of 0 adaptation publications at low GDP countries, the model for
    # coastal does not capture the 'shoulders' of the distribution where there are high adaptation publications at low gdp
    
    ## Format data
    panelD_fit_Df <- data_plot_bars %>%
      filter(oro_branch != "# ORO pubs") 
      mutate(oro_aim = case_when(
        oro_branch %in% c("Marine renewable energy", "CO2 removal or storage", "Increase efficiency") ~ "Mitigation",
        !(oro_branch %in% c("Marine renewable energy", "CO2 removal or storage", "Increase efficiency")) ~ "Adaptation"
      )) %>% 
      select(iso_code, oro_aim, group_land, n_mean, GDP_per_capita) %>%
      mutate(group_land = factor(group_land, levels = c("Land-locked","Coastal","SIDS"))) %>%
      group_by(iso_code, oro_aim, group_land, GDP_per_capita) %>%
      summarise(percent = sum(n_mean)) 
    
    n_total <- data_plot_bars %>%
      filter(oro_branch == "# ORO pubs") %>%
      group_by(iso_code) %>%
      summarise(total = mean(n_mean))
    
    panelD_fit_Df <- reshape2::dcast(panelD_fit_Df, iso_code + group_land + GDP_per_capita ~ oro_aim) 
    panelD_fit_Df <- panelD_fit_Df %>%
      mutate(Adaptation = ifelse(is.na(Adaptation), 0 , Adaptation),
             Mitigation = ifelse(is.na(Mitigation), 0 , Mitigation)) 
    panelD_fit_Df <- merge(panelD_fit_Df, n_total, all.x = "TRUE")
    panelD_fit_Df <- panelD_fit_Df %>%
      mutate(Adaptation_total = Adaptation*total/100,
             Mitigation_total = Mitigation*total/100)
    panelD_fit_Df <- na.omit(panelD_fit_Df)
    summary(panelD_fit_Df)
    
    
    # Plot the probability of adaptation ~ GDP -- Data is pretty noisy at low GDP values
    # x axis country to match barplot to check with Gael's results
    panelD_fit_Df_sub %>%
      arrange(group_land, desc(GDP_per_capita)) %>%
      group_by(group_land) %>%
      mutate(x_order = row_number()) %>% 
    ggplot(aes(x=x_order, y = Adaptation/100))+
      geom_point()+
      facet_wrap(vars(group_land))
    
    # x axis GDP for more accuracy
    ggplot(panelD_fit_Df, aes(x=GDP_per_capita, y = Adaptation/100))+
      geom_point()+
      scale_x_reverse()+
      facet_wrap(vars(group_land))
    
    # Plot the log odds ratio as well 
    ggplot(panelD_fit_Df, aes(x=GDP_per_capita, y = log(Adaptation/Mitigation)))+
      geom_point(aes(size = total), alpha = 0.5)+
      scale_x_reverse()+
      facet_wrap(vars(group_land))
    
    # Remove outliers with high numbers of total publications that bias coastal results 
    # by pulling the weight of data towards high adaptation at mid-GDP?
    outliers_ggp <- panelD_fit_Df %>%
      mutate(high_pubs = ifelse(quantile(total, 0.95) <= total, "Above 95%","Below 95%")) %>%
      ggplot(aes(x=GDP_per_capita, y = log(Adaptation/Mitigation)))+
      geom_point(aes(size = total, col = high_pubs), alpha = 0.5)+
      scale_x_reverse()+
      facet_wrap(vars(group_land)) +
      labs(x = "GDP per capita", y = "Log odds ratio of adaptation vs mitigation",
           col = "N publications\npercentile", size = "Total #\n publications")+
      theme_bw()
    
    
    panelD_fit_Df_sub <- panelD_fit_Df %>%
      #group_by(group_land) %>%
      filter(quantile(total, 0.95) > total) %>%
      mutate(fitted = log(Adaptation/Mitigation)) %>%
      filter(is.finite(fitted))
    
    # Model adaptation as success
    # I chose to give each country equal weight, because otherwise small proportion of coastal countries 
    # with really high numbers would bias the data
    panelD_fit <- glm(Adaptation/100 ~ GDP_per_capita:group_land + group_land, 
                      family = binomial, 
                      #weights = rep(10, nrow(panelD_fit_Df_sub)),
                      weights = panelD_fit_Df_sub$total,
                      data = panelD_fit_Df_sub)
    
    panelD_fit_sum <- summary(panelD_fit) 
    print(panelD_fit_sum)
    
    # ^These results report the odds ratio (on the link scale) relative to the land group.
    # To interpret, put back on response scale by taking the exponent
    panelD_fit_sum$call
    exp(panelD_fit_sum$coefficients[,"Estimate"]) # odds ratio on response scale
    formatC((exp(panelD_fit_sum$coefficients[,"Estimate"])-1)*100, digits = 2) # expressed as a percentage
    
    # Interpretation:
    # For every dollar increase in GDP per capita, the likleihood of favoring adaptation measures changes by:
    # Land-locked countries: -0.00077%  (not significant)
    # Coastal countries: -0.0012 %  (p < 2e-16***). 
    # Or for every decrease in GDP of 833.3 USD, likelihood of favoring adaptation increases by 1%
    # SIDS: -0.0024 % (1.35e-14***) 
    
    # Plot model fit
    x <- seq(min(panelD_fit_Df_sub$GDP_per_capita), max(panelD_fit_Df_sub$GDP_per_capita), length.out = 100)
    pred_dat <- data.frame(
      GDP_per_capita = c(x,x,x),
      group_land = sort(rep(unique(panelD_fit_Df_sub$group_land), 100))
    )
    y <- predict(panelD_fit, pred_dat, type = "link", se.fit = TRUE)
    pred_dat$y <- y$fit
    pred_dat$se.fit <- y$se.fit
    
    model_fit_ggp <- ggplot(pred_dat, aes(GDP_per_capita,y,col=group_land))+
      geom_line()+
      geom_ribbon(aes(ymin = y-se.fit, ymax = y+se.fit, fill = group_land), alpha = 0.25)+
      geom_point(data = panelD_fit_Df_sub, aes(x=GDP_per_capita, y = fitted, size = total), alpha = 0.5)+
      scale_x_reverse()+
      facet_wrap(vars(group_land))+
      labs(x = "GDP per capita", y = "Log odds ratio of adaptation vs mitigation",
           col = "Country type", fill = "Country type", size = "Total #\n publications")+
      theme_bw()
    
    # Plot together
    model_ggp <- cowplot::plot_grid(outliers_ggp, model_fit_ggp, labels = c("a.","b."), nrow=2)
    ggsave(here::here("figures/supplemental/mitigationVsAdaptationByGDPAndCountryTypeBinomialModel.pdf"),
           width = 7, height = 7, plot = model_ggp)
    
    # # # Try a gam
    # # require(mgcv)
    # # require(tidymv)
    # #   
    # panelD_fit_gam <- gam(cbind(Adaptation_total, Mitigation_total) ~ s(GDP_per_capita, by=group_land, k=3),
    #                   family = binomial,
    #                   data = panelD_fit_Df)
    # AIC(panelD_fit)
    # AIC(panelD_fit_gam)
    # pred_dat_gam <- predict_gam(panelD_fit_gam, type = "link")
    # pred_dat_gam$fitresponse <- exp(pred_dat_gam$fit)
    # pred_dat_gam$lower <- exp(pred_dat_gam$fit-pred_dat_gam$se.fit)
    # pred_dat_gam$upper <- exp(pred_dat_gam$fit+pred_dat_gam$se.fit)
    # 
    # ggplot(pred_dat_gam, aes(x=GDP_per_capita, y=fit))+
    #   geom_smooth_ci(group_land)+
    #   theme_bw()
    # 
    # ggplot(pred_dat_gam, aes(x=GDP_per_capita, col = group_land, fill = group_land))+
    #   geom_line(aes(y=fit))+
    #   geom_ribbon(aes(ymin = fit-se.fit, ymax = fit+se.fit), alpha = 0.3)+
    #   geom_point(data = panelD_fit_Df, aes(x=GDP_per_capita, y = log(Adaptation/Mitigation)))+
    #   #geom_hline(yintercept = 1)+
    #   facet_wrap(vars(group_land))+
    #   #ylim(c(0, 1.5))+
    #   labs(y = "Log odds ratio of adaptation publication")+
    #   theme_bw()
    #   
    # ggplot(pred_dat, aes(GDP_per_capita,y,col=group_land))+
    #   geom_line()+
    #   geom_hline(yintercept = 1)+
    #   geom_point(data = panelD_fit_Df, aes(x=GDP_per_capita, y = Adaptation/Mitigation))+
    #   geom_smooth(data = panelD_fit_Df, aes(x=GDP_per_capita, y = Adaptation/Mitigation, group = group_land))+
    #   facet_wrap(vars(group_land))
    
  
    
    # Export model results to a text file
    sink(here::here("outputs/mitigationVsAdaptationByGDPAndCountryTypeBinomialModel.txt"))
    print("## MODEL SUMMARY")
    print(panelD_fit_sum)
    print("## COEFFICIENT TRANSFORMATIONS")
    print("Odds ratio on the response scale (i.e. exp(B))")
    exp(panelD_fit_sum$coefficients[,"Estimate"]) # odds ratio on response scale
    print("Expressed as a percentage (i.e. (exp(B)-1)*100)")
    formatC((exp(panelD_fit_sum$coefficients[,"Estimate"])-1)*100, digits = 2) # as a percentage
    print("## Can the model be simplified??")
    drop1(panelD_fit)
    sink()
    
    
    
    ## ---- PLOT
    
    # --- Donuts 
    donuts_sids <- donuts_plots(data = data_plot_donuts, group = "sids") ; cowplot::plot_grid(plotlist = donuts_sids, ncol = 2)

    # --- Barplots
    barplots_sids <- barplots_gdp(data = data_plot_bars, group = "sids") ; barplots_sids
    
    # --- Arrange plot
    plot_final_sids <- cowplot::ggdraw() +
      cowplot::draw_plot(barplots_sids,    x = 0.00,  y = 0.00, width = 1.0, height = 0.98) +
      cowplot::draw_plot(donuts_sids[[1]], x = 0.30,  y = 0.78, width = 0.2, height = 0.2) +
      cowplot::draw_plot(donuts_sids[[2]], x = 0.68,  y = 0.78, width = 0.2, height = 0.2) +
      cowplot::draw_plot(donuts_sids[[3]], x = 0.825, y = 0.78, width = 0.2, height = 0.2) +
      cowplot::draw_text(text = c("Coastal", "Land-locked", "SIDS"), x = c(0.4, 0.78, 0.925), y = rep(0.985, 3), size = 19) ; plot_final_sids
    
    # ggsave(here::here("figures", "main", "Figure6_test2.jpeg"), width = 15, height = 7, device = "jpeg")
    ggsave(here::here("figures", "main", "stacked_barplot_OROtype_per_country.pdf"), width = 20, height = 10, device = "pdf")
    

### ----- Arrange the figure A to D -----

figure2 <- cowplot::ggdraw() +
  cowplot::draw_plot(panelA,          x = 0.0, y = 0.66, width = 0.5, height = 0.43) +
  cowplot::draw_plot(panelB,          x = 0.5, y = 0.66, width = 0.5, height = 0.43) +
  cowplot::draw_plot(plot_final,      x = .05, y = 0.46, width = 0.9, height = 0.30) +
  cowplot::draw_plot(plot_final_sids, x = 0.0, y = 0.11, width = 1.0, height = 0.33) +
  cowplot::draw_plot_label(label = c("(a)", "(b)", "(c)", "(d)"),
                           size = 15,
                           x = c(0, 0.5, 0, 0),
                           y = c(0.96, 0.96, 0.76, 0.44)) 

ggplot2::ggsave(plot = figure2, here::here("figures", "main", "Figure2_with_PanelD.pdf"), width = 20, height = 25, device = "pdf")


### -----


### ----- Arrange the figure A to C -----

figure2 <- cowplot::ggdraw() +
  cowplot::draw_plot(panelA,          x = 0.0, y = 0.45, width = 0.5, height = 0.5) +
  cowplot::draw_plot(panelB,          x = 0.5, y = 0.45, width = 0.5, height = 0.5) +
  cowplot::draw_plot(plot_final,      x = .05, y = 0.05, width = 0.9, height = 0.45) +
  cowplot::draw_plot_label(label = c("(a)", "(b)", "(c)"),
                           size = 15,
                           x = c(0, 0.5, 0),
                           y = c(0.83, 0.83, 0.51)) 

ggplot2::ggsave(plot = figure2, here::here("figures", "main", "Figure2.pdf"), width = 15, height = 15, device = "pdf")


### -----


    
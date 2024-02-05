#################################################################################################
#                                                                                               #
# FIGURE 6                                                                                      #
# Panel A:          #
# Panel B:          #
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
library(countrycode)
library(ggrepel)
library(cartogram)

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

  ## ---- LOAD & FORMAT GDP DATA
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

    # --- Quartile of GDP data
    # GDP_quartiles <- quantile(gdp_per_capita$GDP_per_capita, probs = seq(0, 1, 0.25), na.rm = TRUE)
    # gdp_per_capita <- gdp_per_capita |> 
    #   mutate(gdp_quartile = cut(GDP_per_capita, breaks = unique(GDP_quartiles), include.lowest = TRUE),
    #          group        = ifelse(!is.na(gdp_quartile), as.numeric(gdp_quartile), NA))
  
  ## ---- LOAD & FORMAT ORO data
  pred_oro_type_long <- tbl(dbcon, "pred_oro_type_long") %>% collect()
  uniquerefs <- tbl(dbcon, "uniquerefs") # metadata on the unique references
  countries_ls <- read.csv(file = here::here("data", "external", "list_of_countries", "sql-pays.csv"), sep = ";") # Countries names
  pred_relevance <- tbl(dbcon, "pred_relevance")  # which articles are relevant to OROs

  landlocked <- read.csv(file = here::here("data", "external", "special_country_groups", "landlocked-countries-2024.csv"), sep = ",") |> 
    select(country, LandlockedCountries) |> 
    rename(Country    = country, 
           group_land = LandlockedCountries) |> 
    mutate(iso_code = countrycode(sourcevar   = Country,
                                  origin      = "country.name",
                                  destination = "iso3c"),
           group_land = case_when(group_land == "yes" ~ "Land-locked",
                                  TRUE ~ "NA")) 
  
  AMUNRC <- c("American Samoa", "Anguilla", "Aruba", "Bermuda", "British", "Virgin Islands", "Cayman Islands", "Commonwealth of Northern Marianas",
              "Curacao", "French Polynesia", "Guadeloupe", "Guam", "Martinique", "Montserrat", "New Caledonia", "Puerto Rico", "Sint Maarten",
              "Turks and Caicos Islands", "United States Virgin Islands")
  country_grp <- read.csv(file = here::here("data", "external", "special_country_groups", "special-country-groups.csv"), sep = ",") |> 
    select(Country, LLDC, SIDS) |> 
    mutate(iso_code = countrycode(sourcevar   = Country,
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
    select(-Country)
  
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
  
    # --- Join with author affiliation data from uniquerefs
    pred_oro_type <- uniquerefs %>%
      # left_join(pred_relevance, by = "analysis_id") |> 
      # filter(0.5 <= relevance_mean) %>% 
      dplyr::select(analysis_id, affiliation) %>%
      collect()%>%
      right_join(pred_oro_type_long, by = "analysis_id")
    
    # --- Find 1st author affiliation
    pred_oro_type_1A <- extract_1stA_affiliation(data         = pred_oro_type, 
                                                 countries_ls = countries_ls)
    
    # tmp <- pred_oro_type_1A$oroAff_1stA
    # tmp2 <- pred_oro_type_1A2$oroAff_1stA
    # tmp3 <- pred_oro_type_1A3$oroAff_1stA
    
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
    
      # Total data
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

      # Compute percentage
      npaper_per_country_branch_perc <- npaper_per_country_branch |> 
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
        
      
      

    # --- Select the most relevant oro_type predicted and estimate the # paper per country & oro_type 
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
      

  
  ## ---- MERGE GDP & ORO DATA
  data_plot_bars <- left_join(npaper_per_country_branch_perc, gdp_per_capita, by = "iso_code") |> filter(!is.na(GDP_per_capita)) |> ungroup()
  data_plot_donuts <- left_join(npaper_per_country_type, gdp_per_capita, by = "iso_code") |> filter(!is.na(GDP_per_capita))
  
    
  ## ---- PLOT DATA
  
  # !!!!! CHECK NA !!!!!  
  # !!!!! ALL data for barplots with the same number of countries !!!!!  
  
    # --- Donuts 
    donuts_Q <- donuts_plots(data = data_plot_donuts, group = "gdp") ; cowplot::plot_grid(plotlist = donuts_Q, ncol = 2)
    donuts_sids <- donuts_plots(data = data_plot_donuts, group = "sids") ; cowplot::plot_grid(plotlist = donuts_sids, ncol = 2)
    donuts_conti <- donuts_plots(data = data_plot_donuts, group = "continent") ; cowplot::plot_grid(plotlist = donuts_conti, ncol = 2)
    
    
    # --- Barplots
    barplots_Q <- barplots_gdp(data = data_plot_bars, group = "gdp") ; barplots_Q
    barplots_sids <- barplots_gdp(data = data_plot_bars, group = "sids") ; barplots_sids
    barplots_conti <- barplots_gdp(data = data_plot_bars, group = "continent") ; barplots_conti
    
    barplots <- barplots_gdp(data = data_plot_bars, group = FALSE) ; barplots
    
    ggsave(here::here("figures", "main", "Figure6_type.jpeg"), width = 20, height = 10, device = "jpeg")
    

    # --- Arrange plot
    plot_final_sids <- cowplot::ggdraw() +
      cowplot::draw_plot(barplots_sids,    x = 0.00, y = 0.0, width = 1.0, height = 0.8) +
      cowplot::draw_plot(donuts_sids[[1]], x = 0.30, y = 0.8, width = 0.2, height = 0.2) +
      cowplot::draw_plot(donuts_sids[[2]], x = 0.675, y = 0.8, width = 0.2, height = 0.2) +
      cowplot::draw_plot(donuts_sids[[3]], x = 0.82, y = 0.8, width = 0.2, height = 0.2) +
      cowplot::draw_text(text = c("Coastal", "Land-locked", "SIDS"), x = c(0.4, 0.775, 0.92), y = rep(0.805, 3), size = 17) ; plot_final_sids
    
    # ggsave(here::here("figures", "main", "Figure6_test2.jpeg"), width = 15, height = 7, device = "jpeg")
    ggsave(here::here("figures", "main", "Figure6_sids_type.jpeg"), width = 20, height = 10, device = "jpeg")
    
    
    plot_final_gdp <- cowplot::ggdraw() +
      cowplot::draw_plot(barplots_Q, x = 0.00, y = 0.0, width = 1.0, height = 0.8) +
      cowplot::draw_plot(donuts_Q[[4]], x = 0.07, y = 0.8, width = 0.2, height = 0.2) +
      cowplot::draw_plot(donuts_Q[[3]], x = 0.30, y = 0.8, width = 0.2, height = 0.2) +
      cowplot::draw_plot(donuts_Q[[2]], x = 0.54, y = 0.8, width = 0.2, height = 0.2) +
      cowplot::draw_plot(donuts_Q[[1]], x = 0.775, y = 0.8, width = 0.2, height = 0.2) +
      cowplot::draw_text(text = c("1st quartile", "2nd quartile", "3rd quartile", "4th quartile"), x = c(0.17, 0.4, 0.64, 0.875), y = rep(0.805, 4), size = 17) ; plot_final_gdp
    ggsave(here::here("figures", "main", "Figure6_gdp_type.jpeg"), width = 20, height = 10, device = "jpeg")
    
    
    plot_final_conti <- cowplot::ggdraw() +
      cowplot::draw_plot(barplots_conti, x = 0.00, y = 0.0, width = 1.0, height = 0.8) +
      cowplot::draw_plot(donuts_conti[[1]],  x = -0.005, y = 0.8, width = 0.2, height = 0.2) +
      cowplot::draw_plot(donuts_conti[[2]],  x = 0.17,   y = 0.8, width = 0.2, height = 0.2) +
      cowplot::draw_plot(donuts_conti[[3]],  x = 0.42,   y = 0.8, width = 0.2, height = 0.2) +
      cowplot::draw_plot(donuts_conti[[4]],  x = 0.58,   y = 0.8, width = 0.2, height = 0.2) +
      cowplot::draw_plot(donuts_conti[[5]],  x = 0.70,   y = 0.8, width = 0.2, height = 0.2) +
      cowplot::draw_plot(donuts_conti[[6]],  x = 0.85,   y = 0.8, width = 0.2, height = 0.2) +
      cowplot::draw_text(text = c("North America", "Europe", "Asia", "Oceania", "Africa", "South America"),
                         x = c(0.095, 0.27, 0.52, 0.68, 0.8, 0.955), y = rep(0.805, 6), size = 17) ; plot_final_conti
    ggsave(here::here("figures", "main", "Figure6_conti_type.jpeg"), width = 20, height = 10, device = "jpeg")
    

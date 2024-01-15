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
    filter(!is.na(Code) & !is.na(GDP_per_capita))

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

    # --- Join with author affiliation data from uniquerefs
    pred_oro_type <- uniquerefs %>%
      # left_join(pred_relevance, by = "analysis_id") |> 
      # filter(0.5 <= relevance_mean) %>% 
      select(analysis_id, affiliation) %>%
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
                                                          "Guam"             = "United States"))) |> 
      group_by(country_aff, oro_branch) |> 
      summarise(n_mean = sum(0.5 <= mean),
                n_lower = sum(0.5 <= lower),
                n_upper = sum(0.5 <= upper)) |> 
      # arrange(country_aff, oro_branch, -n_mean) |>
      ungroup() |> 
      mutate(code = countrycode(sourcevar   = country_aff,
                                origin      = "country.name",
                                destination = "iso3c"))

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
                                                          "Guam"             = "United States"))) |>
      group_by(country_aff, oro_type) |> 
      summarise(n_mean = sum(0.5 <= mean),
                n_lower = sum(0.5 <= lower),
                n_upper = sum(0.5 <= upper)) |> 
      ungroup() |> 
      mutate(code = countrycode(sourcevar   = country_aff,
                                origin      = "country.name",
                                destination = "iso3c"))

  
  ## ---- MERGE GDP & ORO DATA
  data_plot_bars <- left_join(npaper_per_country_branch, gdp_per_capita, by = c("code" = "Code")) |> filter(!is.na(GDP_per_capita)) 
  data_plot_donuts <- left_join(npaper_per_country_type, gdp_per_capita, by = c("code" = "Code")) |> filter(!is.na(GDP_per_capita))
  
    
    
  ## ---- PLOT DATA
  
  # !!!!! CHECK NA !!!!!  
  # !!!!! ALL data for barplots with the same number of countries !!!!!  
  
    # --- Donuts 
    donuts <- donuts_plots(data = data_plot_donuts)
    cowplot::plot_grid(plotlist = donuts, ncol = 2)
    
    # --- Barplots
    barplots <- barplots_gdp(data = data_plot_bars) 

    # --- Arrange plot
    plot_final <- cowplot::ggdraw() +
      cowplot::draw_plot(barplots,    x = 0.00, y = 0.0, width = 1.0, height = 0.8) +
      cowplot::draw_plot(donuts[[4]], x = 0.08, y = 0.8, width = 0.2, height = 0.2) +
      cowplot::draw_plot(donuts[[3]], x = 0.34, y = 0.8, width = 0.2, height = 0.2) +
      cowplot::draw_plot(donuts[[2]], x = 0.58, y = 0.8, width = 0.2, height = 0.2) +
      cowplot::draw_plot(donuts[[1]], x = 0.80, y = 0.8, width = 0.2, height = 0.2) ; plot_final
    
    ggsave(here::here("figures", "main", "Figure62.jpeg"), width = 15, height = 7, device = "jpeg")
    
  
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
library(countrycode)
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
  numb_OandApub_per_country <- read.delim(here::here("data/external/ocean-and-climate-publications/WOS_ocean-and-climate_by-country_2023-11-21.txt"))
  world_shp <- sf::read_sf(here::here("data", "external", "world_shp"))  # shape file of the world
  countries_ls <- read.csv(file = here::here("data", "external", "list_of_countries", "sql-pays.csv"), sep = ";") |>  # Countries names
    dplyr::mutate(country = countrycode(sourcevar   = name_en,
                                        origin      = "country.name",
                                        destination = "country.name"),
                  iso_code = countrycode(sourcevar   = country,
                                         origin      = "country.name",
                                         destination = "iso3c"))
  

                  # iso_code = case_when(is.na(iso_code) ~ iso_code_country, TRUE ~ iso_code))
  # --- LDC = Least Developed Countries
  # --- LLCD = Land Locked Developing Countries
  # --- SIDS = Small Island Developing States
  landlocked <- read.csv(file = here::here("data", "external", "special_country_groups", "landlocked-countries-2024.csv"), sep = ",") |> 
    dplyr::select(country, LandlockedCountries) |> 
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
  
  eez_shp <- sf::st_read(here::here("data", "external", "eez_shp", "eez_v12.shp")) |>  # shape file of countrie's EEZ
    dplyr::select(MRGID, POL_TYPE, TERRITORY1, SOVEREIGN1, ISO_SOV1) |> 
    dplyr::rename(Country = SOVEREIGN1) |> 
    dplyr::mutate(Country = countrycode(sourcevar   = ISO_SOV1,
                                        origin      = "iso3c",
                                        destination = "country.name"),
                  # iso_code_country = countrycode(sourcevar   = Country,
                  #                                origin      = "country.name",
                  #                                destination = "iso3c"),
                  iso_code = countrycode(sourcevar   = Country,
                                         origin      = "country.name",
                                         destination = "iso3c")) |> 
    full_join(country_grp |>  dplyr::select(-iso_code), by = c("TERRITORY1" = "Country")) |> 
    mutate(group_land = case_when(Country == "Cape Verde" ~"Island", TRUE ~ group_land))
  
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
    
    tmp <- data_1stA_country$oroAff_1stA |> filter(country_aff == "Malta")
    
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
      # filter(group_land == "SIDS" & !is.na(MRGID) & !is.na(layer)) |>
      filter(! TERRITORY1 %in% c("French Guiana", "Greenland")) |> 
      sf::st_transform(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

    tmp2 <- sf::st_drop_geometry(world_shp_boundaries) |>filter(group_land == "SIDS")
    tmp2 <- sf::st_drop_geometry(eez_shp_islands)
    tmp2 <- sf::st_drop_geometry(eez_shp)
    length(unique(tmp2$TERRITORY1))
    
    # --- Format the data to produce the map
    data_2_map_panelA <- format_data2map(data = world_shp_boundaries,
                                         PROJ = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

    
    
  ## ---- PLOT PANEL A without points
  panelA <- univariate_map(data_map          = data_2_map_panelA,
                           eez               = eez_shp_islands,
                           color_scale       = viridis::magma(10, direction = -1),
                           midpoint          = NULL,
                           second.var        = NULL,
                           # vals_colors_scale = NULL,
                           title_color       = "#ORO/#O&C (%)",
                           title_size        = NULL,
                           show.legend       = TRUE,
                           name              = "main/Fig2_panelA")
  
  ## ---- PLOT PANEL A with points
  panelA <- univariate_map(data_map          = data_2_map_panelA,
                           eez               = eez_shp_islands,
                           color_scale       = viridis::magma(10, direction = -1),
                           midpoint          = NULL,
                           second.var        = "Count_ORO",
                           # vals_colors_scale = NULL,
                           title_color       = "#ORO/#O&C (%)",
                           title_size        = "#ORO paper",
                           show.legend       = TRUE,
                           name              = "main/Fig2_panelA_points_eez")
  
  test <- data_map$data |> filter(! group_land %in% c("Island", "AMUNRC")) |>  sf::st_drop_geometry()

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
    
    # --- Format the shapefile of the world countries polygon and bind data
    world_shp_boundaries_MA <- format_shp_of_the_world(world_shp    = world_shp,
                                                       data_to_bind = ratio_mitig_adapt,
                                                       PROJ         = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") |> 
      # replace_na(list(adaptation = 0, mitigation = 0, mit_ada = 0, ratio = 0)) |> 
      left_join(country_grp |>  select(-Country), by = "iso_code") |> 
      mutate(group_land = case_when(group_land %in% c("Land-locked", "SIDS", "Coastal") ~ group_land,
                                    !is.na(group_land) & NA2_DESCRI != country ~ "Island",
                                    is.na(group_land)  & NA2_DESCRI != country ~ "Island",
                                    is.na(group_land)  & NA2_DESCRI == country ~ "Coastal"))
    
    eez_shp_islands_MA <- full_join(eez_shp |>  select(-Country), ratio_mitig_adapt, by = "iso_code") |> 
      # left_join(country_grp |>  select(-Country), by = "iso_code") |> 
      mutate(country = str_replace_all(Country, c("Côte d’Ivoire" = "Ivory Coast",
                                                  "Congo - Brazzaville" = "Republic of the Congo",
                                                  "Congo - Kinshasa"    = "Democratic Republic of the Congo",
                                                  "Somalia"             = "Federal Republic of Somalia")),
             group_land = case_when(group_land %in% c("Island", "Land-locked", "SIDS", "Coastal") ~ group_land,
                                    !is.na(group_land) & TERRITORY1 != Country ~ "Island",
                                    is.na(group_land)  & TERRITORY1 != Country ~ "Island",
                                    is.na(group_land)  & TERRITORY1 == Country ~ "Coastal")) |> 
      filter(group_land %in% c("Island", "SIDS", "AMUNRC") & !is.na(MRGID)) |>
      # filter(group_land == "SIDS" & !is.na(MRGID) & !is.na(layer)) |>
      filter(! TERRITORY1 %in% c("French Guiana", "Greenland")) |> 
      sf::st_transform(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
    tmp <- sf::st_drop_geometry(world_shp_boundaries_MA)
    tmp2 <- sf::st_drop_geometry(eez_shp_islands_MA)
    
    # --- Format the data to produce the map
    data_2_map_panelB <- format_data2map(data = world_shp_boundaries_MA,
                                         PROJ = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
  ## ---- PLOT PANEL B
  panelB <- univariate_map(data_map          = data_2_map_panelB,
                           eez               = eez_shp_islands_MA,
                           color_scale       = c("#4c4680","white", "#197da8"),
                           second.var        = NULL,
                           midpoint          = 0.5, 
                           title_color       = "% mit. ORO",
                           title_size        = NULL, 
                           show.legend       = TRUE,
                           name              = "main/Fig2_panelB_eez")
  
### -----
  
### ----- PANEL C -----
  
  ## ---- LOAD DATA
  income_grp <- readxl::read_xlsx(path = here::here("data", "external", "special_country_groups", "income_group.xlsx")) |> 
    select(Economy, Code, `Income group`) |> 
    rename(country      = Economy, 
           iso_code     = Code,
           group_income = `Income group`)
  
  
  ## ---- FORMAT DATA
  
    # --- Find the predominant ORO type (adaptation vs. mitigation)
    # --- And bind all data together
    data_panelC <- ratio_ORO_totPub |> 
      rename(ORO_OC = layer) |> 
      full_join(ratio_mitig_adapt |>  rename(perc_mit = layer), by = "iso_code") |> 
      select(-Country, -ratio, -mit_ada) |> 
      # left_join(income_grp |>  select(-country), by = "iso_code") |>
      left_join(country_grp |> select(iso_code, group_land), by = "iso_code") |>
      replace_na(list(group_land = "Coastal")) |>
      filter(!is.na(country) & group_land != "AMUNRC" )
      
    # data_panelC <- world_shp_boundaries |> 
    #   sf::st_drop_geometry() |> 
    #   select(iso_code, Record.Count, Count_ORO, NA2_DESCRI, admin_iso, country, layer, group_land) |> 
    #   rename(ORO_OC = layer) |> 
    #   full_join(world_shp_boundaries_MA |> sf::st_drop_geometry() |>  
    #               select(iso_code, mitigation, adaptation, admin_iso, layer) |> rename(perc_mit = layer), by = "admin_iso") |> 
    #   filter(!is.na(country) & !is.na(group_land)) |> 
    #   distinct(country, .keep_all = TRUE)
    # # data_panelC_no0 <- filter(data_panelC, Record.Count > 0 & Count_ORO > 0)
    
    # --- Remove NA and Data transformation if needed
    # --- Identify the points that are the farthest from the OLS
    hist(data_panelC_noNA$Record.Count) # Data skewed to the right => log10 transformation
    hist(data_panelC_noNA$Count_ORO) # Data skewed to the right => log10 transformation
    
  
    # For some countries (e.g., Greenland), one ref identified in the mit_adap df.
    # But not selected in the oroAffiliations df because level of relevance too low.
    

  ## ---- PLOT PANEL C
  panelC <- biplot_fig2c(data        = data_panelC,
                         var.y       = "Count_ORO",
                         var.x       = "Record.Count",
                         var.col     = "ORO_OC",
                         var.shape   = "group_land",
                         ylab        = "# ORO publication",
                         xlab        = "# O&C publication",
                         one_one_line = FALSE,
                         color_scale = viridis::magma(10, direction = -1),
                         color_title = "#ORO/#O&C (%)",
                         log.transf  = TRUE,
                         quant.prob  = 0.90, 
                         name        = "main/Fig2_PanelC2") ; panelC 
  
 

  
### -----

  
### ----- PANEL D -----
  
  data_panelD <- filter(data_panelC, !is.na(perc_mit))
  data_panelD2 <- data_panelD |> 
    mutate(adaptation_log = log(adaptation + 1),
           mitigation_log = log(mitigation + 1),
           labels    = case_when(adaptation >= mitigation ~ "Adaptation",
                                 TRUE ~ "Mitigation")) |> 
    # group_income = factor(group_income, levels = c("High income", "Upper middle income", "Lower middle income", "Low income"))) |> 
    ungroup()
  
  ## --- Donuts plots
  n_LL = sum(data_panelD2$group_land == "Land-locked")
  n_C = sum(data_panelD2$group_land == "Coastal")
  n_SIDS = sum(data_panelD2$group_land == "SIDS")
  
  data_donut = data_panelD2 |> 
    group_by(group_land, labels) |> 
    summarise(count = n()) |> 
    mutate(total_count = case_when(group_land == "Coastal" ~ n_C,
                                   group_land == "SIDS" ~ n_SIDS,
                                   TRUE ~ n_LL),
           perc = (count/total_count)*100) |> 
    group_split(group_land)
  
  panelD <- biplot_fig2c(data        = data_panelD,
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
                         name        = "main/Fig2_PanelD_France") ; panelD 
  
  ggplot2::ggsave(here::here("figures", "main", "Fig2_PanelD_France2.jpeg"), width = 9, height = 7, device = "jpeg")
  
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
    cowplot::draw_plot(panelD, x = 0.0, y = 0.0, width = 0.60, height = 1.0) +
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
  ggplot2::ggsave(plot = plot_final, here::here("figures", "main", "figure2_panelC_donuts.jpeg"), width = 15, height = 7, device = "jpeg")
  
  
### -----
  
### ----- PANEL Supp -----
  
  ## ---- FORMAT DATA
  ratio_mitig_adapt2 <- ratio_mitig_adapt |> 
    select(-ratio) |> 
    rename(n_MitPlusAda = mit_ada,
           perc_mit     = layer) |> 
    # left_join(income_grp |>  select(-country), by = "iso_code") |> 
    left_join(country_grp |> select(iso_code, group_land), by = "iso_code") |> 
    replace_na(list(group_land = "Coastal")) |> 
    filter(group_land != "AMUNRC") 

    ## --- Residuals
    data_panelD2 <- data_panelD |> 
      mutate(adaptation_log = log(adaptation + 1),
             mitigation_log = log(mitigation + 1),
             labels    = case_when(adaptation >= mitigation ~ "Adaptation",
                                   TRUE ~ "Mitigation")) |> 
             # group_income = factor(group_income, levels = c("High income", "Upper middle income", "Lower middle income", "Low income"))) |> 
      ungroup()
    
    ## --- Donuts plots
    n_LL = sum(data_panelD2$group_land == "Land-locked")
    n_C = sum(data_panelD2$group_land == "Coastal")
    n_SIDS = sum(data_panelD2$group_land == "SIDS")
    
    data_donut = data_panelD2 |> 
      group_by(group_land, labels) |> 
      summarise(count = n()) |> 
      mutate(total_count = case_when(group_land == "Coastal" ~ n_C,
                                     group_land == "SIDS" ~ n_SIDS,
                                     TRUE ~ n_LL),
             perc = (count/total_count)*100) |> 
      group_split(group_land)
    
  
  ## ---- PLOT PANEL D
  plot <- ggplot(data    = data_panelD2, 
         mapping = aes(x = mitigation_log, 
                       y = adaptation_log,
                       color = group_land)) +
    
    geom_abline(slope = 1, linetype='dotted') +
    
    scale_color_manual(values = c("SIDS" = "#0fbcd6",
                                  "Land-locked" = "#b36705",
                                  "Coastal" = "#3f47e8"),
                       name = NULL) +
    
    geom_text_repel(data         = filter(data_panelD2, labels == TRUE), 
                    mapping      = aes(label = country), 
                    max.overlaps = 100,
                    show.legend  = FALSE,
                    min.segment.length = 0.1) +
    
    geom_point(show.legend = FALSE) + # mapping = aes(color = group_income)
    
    facet_wrap(group_land ~ ., ncol = 3) +
    
    # geom_smooth(method  = lm, 
    #             col     = "grey10") +
    
    ylim(c(0, 9)) +
    xlim(c(0, 9)) +
    
    xlab(label = "# Mit. papers") +
    ylab(label = "# Ada. papers") +
    
    # scale_color_manual(values = c("High income"   = "#de6040",
    #                               "Upper middle income" = "#40c28a",
    #                               "Lower middle income" = "#9e63ad",
    #                               'Low income' = "#0fbcd6"),
    #                    name   = "Income",
    #                    labels = c("High income"         = "High",
    #                               "Upper middle income" = "Upper middle",
    #                               "Lower middle income" = "Lower middle",
    #                               "Low income"          = "Low")) +
    
    theme_bw() +
    # guides(size = "none", color = guide_colourbar(title.position = "top", barwidth = 8, barheight = 0.7)) +
    theme(legend.position = "right",
          axis.text.x     = element_text(size = 11),
          axis.text.y     = element_text(size = 11),
          axis.title.x    = element_text(size = 13),
          axis.title.y    = element_text(size = 13),
          legend.text     = element_text(size = 13),
          legend.title    = element_text(size  = 14, 
                                         face  = "bold", 
                                         vjust = 0.5),
          strip.text.x = element_text(size = 14)) ; plot
  
  ### Change strips backgrounds
  fill_colors = c("#3f47e8", "#b36705", "#0fbcd6")
  # plot2 <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(plot))
  # strips <- which(startsWith(plot2$layout$name,'strip'))
  # 
  # for (s in seq_along(strips)) {
  #   plot2$grobs[[strips[s]]]$grobs[[1]]$children[[1]]$gp$fill <- fill_colors[s]
  # }
  # 
  # plot(plot2)
  # ggplot2::ggsave(plot = plot2, here::here("figures", "main", "Ada_vs_Mit_country_types_v2.jpeg"), width = 15, height = 5, device = "jpeg")

  plot_donuts_ls <- lapply(data_donut, function(x){
    
    x_plot <- x |> 
      select(-group_land) |> 
      mutate(Type     = "MitAda",
             cat = factor(labels, levels = c("Mitigation", "Adaptation"))) |> 
      arrange(cat) |> 
      mutate(pos = round(cumsum(perc) - (0.5 * perc), 2))  

    ggplot(x_plot, aes(x = Type, y = perc, fill = labels)) +
      geom_col(show.legend = F) +
      geom_text(aes(label = paste0(round(perc, 1), "%"), x = Type, y = pos), size = 4, color = "white") +
      # Colors
      scale_fill_manual(name   = NULL,
                        values = c("Adaptation"  = "#4c4680",
                                   "Mitigation" = "#197da8")) +
      scale_x_discrete(limits = c(" ", "MitAda")) +
      coord_polar("y") +
      theme_void()
    
  })
  
  cowplot::plot_grid(plotlist = plot_donuts_ls, ncol = 3)
  
  # --- Arrange plot
  plot_final <- cowplot::ggdraw() +
    cowplot::draw_plot(plot, x = 0.00, y = 0.0, width = 1.0, height = 1.0) +
    cowplot::draw_plot(plot_donuts_ls[[1]], x = -0.09, y = 0.55, width = 0.4, height = 0.4) +
    cowplot::draw_plot(plot_donuts_ls[[2]], x = 0.23,  y = 0.55, width = 0.4, height = 0.4) +
    cowplot::draw_plot(plot_donuts_ls[[3]], x = 0.55,  y = 0.55, width = 0.4, height = 0.4) ; plot_final
    
  ggplot2::ggsave(plot = plot_final, here::here("figures", "main", "adap_mit_SIDS5.jpeg"), width = 15, height = 5, device = "jpeg")
  
### -----

  
### ---- PANEL Supp -----
  
  ## --- LOAD DATA
  grid_df <- tbl(dbcon, "grid_df_res2.5") 
  shp_df_matches <- tbl(dbcon, "geoparsed-text_shp_df_matches")
  pred_oro_branch <- tbl(dbcon, "pred_oro_branch") # predictions for ORO branch
  eez_shp <- sf::st_read(here::here("data", "external", "eez_shp", "eez_v12.shp")) |>  # shape file of countrie's EEZ
    dplyr::select(MRGID, POL_TYPE, TERRITORY1, SOVEREIGN1, ISO_SOV1) |> 
    dplyr::rename(Country = SOVEREIGN1)
  
  test <- tbl(dbcon, "shp_df_natural-earth-shapes") |> select(shpfile_id, sovereignt, sov_a3, admin, adm0_a3) |>  collect()
  test2 <- tbl(dbcon, "shp_grid_df") |>  collect()
  
  length(unique(test$shpfile_id))
  
    # -- # ORO publication per country based on 1st author affiliation
    ORO_per_country_aff <- ORO_per_country |> 
      rename(Count_ORO_1aff = Count_ORO) |> 
      group_by(iso_code) |> 
      summarise(Count_ORO_1aff = sum(Count_ORO_1aff))
      
  
    # -- # ORO publication per country based on geoparsong results
    ORO_geoP <- oroAffiliations |> 
      left_join(shp_df_matches, by = "analysis_id", copy = TRUE) |> 
      distinct(analysis_id, affiliation, place, .keep_all = TRUE) |> 
      right_join(grid_df, by = "grid_df_id", copy = TRUE) |>
      filter(!is.na(analysis_id)) |> 
      collect()
    
  ## --- FORMAT DATA
    
    # -- Identify the country of the geoparsed data (in terrestrial land)
    ORO_geoP_sf_land <- ORO_geoP |> 
      filter(is_land == 1) |> 
      sf::st_as_sf(coords = c("LON", "LAT"), crs = 4326) 
    
    sf::sf_use_s2(FALSE)
    ORO_per_country_geoP_sf_land <- sf::st_join(ORO_geoP_sf_land, world_shp) |>  
      filter(!is.na(affiliation))
    
    # ORO_land_test <- ORO_geoP_sf_land |> 
    #   sf::st_drop_geometry() |> 
    #   select(analysis_id, shp_id) |> 
    #   left_join(test, by = c("shp_id" = "shpfile_id")) |> 
    #   filter(!is.na(shp_id)) |> 
    #   distinct() ; length(unique(ORO_land_test$analysis_id))
    
    # y <- sf::st_join(ORO_geoP_sf_land, world_shp)
    # z <- sf::st_join(world_shp, ORO_geoP_sf_land)
    
    ORO_per_country_geoP_df_land <- sf::st_drop_geometry(ORO_per_country_geoP_sf_land) |>
      select(analysis_id, affiliation, grid_df_id, shp_id, place, NA2_DESCRI) |> 
      left_join(test, by = c("shp_id" = "shpfile_id")) |> 
      filter(!is.na(analysis_id)) |>
      distinct(analysis_id, NA2_DESCRI, .keep_all = TRUE) |> 
      select(-grid_df_id, -place) |> 
      filter(!is.na(NA2_DESCRI) | !is.na(sovereignt)) |> 
      select(analysis_id, NA2_DESCRI, sovereignt, sov_a3, admin, adm0_a3) |> 
      rename(TERRITORY1 = NA2_DESCRI)
      
    x <- filter(ORO_per_country_geoP_df_land, analysis_id == 378349)
    y2 <- filter(y, analysis_id == 378349)
    
    # -- Identify the country of the geoparsed data (in marine realm)
    ORO_geoP_sf_sea <- ORO_geoP |> 
      filter(is_land == 0) |> 
      sf::st_as_sf(coords = c("LON", "LAT"), crs = 4326) 
    
    ORO_per_country_geoP_sf_eez <- sf::st_join(ORO_geoP_sf_sea, eez_shp) |> 
      filter(!is.na(affiliation))
    
    # !!! is.na(affiliation) why ? !!! sf::st_join(ORO_geoP_sf_sea, eez_shp)
    
    ORO_per_country_geoP_df_eez <- sf::st_drop_geometry(ORO_per_country_geoP_sf_eez) |>
      select(analysis_id, affiliation, grid_df_id, shp_id, place, TERRITORY1, Country, ISO_SOV1) |> 
      left_join(test, by = c("shp_id" = "shpfile_id")) |> 
      filter(!is.na(analysis_id)) |>
      select(analysis_id, TERRITORY1, sovereignt, sov_a3, admin, adm0_a3)
    
    
    # -- Bind eez data with land data
    ORO_per_country_geoP_df_all <- rbind(ORO_per_country_geoP_df_land, ORO_per_country_geoP_df_eez) |> 
      mutate(same = case_when(TERRITORY1   == admin ~ "same",
                              is.na(admin) == TRUE ~ "NAadmin",
                              TRUE ~ "diff"),
             final_country = case_when(TERRITORY1 == admin ~ admin,
                                       is.na(admin) == TRUE ~ TERRITORY1,
                                       is.na(TERRITORY1) == TRUE ~ admin,
                                       TRUE ~ admin),
             final_admin   = case_when(is.na(admin) == TRUE ~ TERRITORY1,
                                       TRUE ~ admin)) |> 
      filter(!is.na(final_country))
    
    length(unique(ORO_per_country_geoP_df_all$analysis_id)) # n = 17191 from geoparsing
    sum(ORO_per_country$Count_ORO) # n = 40644 from 1st author affiliation
    
    sum(ORO_per_country_geoP_df_all$same == "same") # 10991
    sum(ORO_per_country_geoP_df_all$same == "NAadmin") # 10815
    sum(ORO_per_country_geoP_df_all$same == "diff") # 2126
    
      # Select data with 1st author affiliation that are identified in the geoparsing results
      ORO_aff_selecGEOP <- data_1stA_country$oroAff_1stA |> 
        filter(!is.na(country_aff)) |>
        filter(analysis_id %in% unique(ORO_per_country_geoP_df_all$analysis_id)) 
      
      ORO_geoP_select_1staff <- ORO_per_country_geoP_df_all |> 
        select(analysis_id, final_country) |> 
        filter(analysis_id %in% unique(ORO_aff_selecGEOP$analysis_id)) |> 
        mutate(final_country = stringr::str_replace_all(final_country, c("Chagos Archipelago" = "United Kingdom",
                                                                         "Alaska"             = "United States",
                                                                         "United States of America" = "United States",
                                                                         "Hawaii"             = "United States",
                                                                         "Azores"             = "Portugal",
                                                                         "Madeira"            = "Portugal",
                                                                         "Gilbert Islands"    = "Kiribati",
                                                                         "Tristan da Cunha"   = "United Kingdom",
                                                                         "Canary Islands"     = "Spain",
                                                                         "Palmyra Atoll"      = "United States",
                                                                         "Johnston Atoll"      = "United States",
                                                                         "Andaman and Nicobar" = "India",
                                                                         "Jan Mayen"           = "Norway",
                                                                         "Crozet Islands"      = "France",
                                                                         "Macquarie Island"    = "Australia",
                                                                         "Prince Edward Islands" = "South Africa",
                                                                         "Micronesia" = "Federated States of Micronesia"))) |> 
        rename(country_geoP = final_country)
      
      ORO_allselect_aff_geop <- full_join(ORO_aff_selecGEOP, ORO_geoP_select_1staff, by = "analysis_id") |> 
        mutate(country_aff = countrycode(sourcevar   = country_aff,
                                         origin      = "country.name",
                                         destination = "country.name"),
               country_geoP = countrycode(sourcevar   = country_geoP,
                                          origin      = "country.name",
                                          destination = "country.name"))
      
      ORO_per_country_allselect_aff <- ORO_allselect_aff_geop |> 
        group_by(country_aff) |> 
        summarise(Count_ORO_1aff = n())
      
      ORO_per_country_allselect_geoP <- ORO_allselect_aff_geop |> 
        group_by(country_geoP) |> 
        summarise(Count_ORO_geoP = n())
      
      ORO_per_country_common_Aff_Geop <- full_join(ORO_per_country_allselect_aff, ORO_per_country_allselect_geoP, by = c("country_aff" = "country_geoP")) |> 
        replace_na(list(Count_ORO_1aff = 0, Count_ORO_geoP = 0)) |> 
        mutate(iso_code = countrycode(sourcevar   = country_aff,
                                      origin      = "country.name",
                                      destination = "iso3c"),
               layer = ((Count_ORO_1aff - Count_ORO_geoP)/Count_ORO_geoP)*100) |> 
        ungroup()
      
      sum(ORO_per_country_common_Aff_Geop$Count_ORO_geoP)
      sum(ORO_per_country_common_Aff_Geop$Count_ORO_1aff)


    # -- Group by countries
    ORO_per_country_geoP <- ORO_per_country_geoP_df_all |> 
      mutate(final_country = stringr::str_replace_all(final_country, c("Chagos Archipelago" = "United Kingdom",
                                                                       "Alaska"             = "United States",
                                                                       "United States of America" = "United States",
                                                                       "Hawaii"             = "United States",
                                                                       "Azores"             = "Portugal",
                                                                       "Madeira"            = "Portugal",
                                                                       "Gilbert Islands"    = "Kiribati",
                                                                       "Tristan da Cunha"   = "United Kingdom",
                                                                       "Canary Islands"     = "Spain",
                                                                       "Palmyra Atoll"      = "United States",
                                                                       "Johnston Atoll"      = "United States",
                                                                       "Andaman and Nicobar" = "India",
                                                                       "Jan Mayen"           = "Norway",
                                                                       "Crozet Islands"      = "France",
                                                                       "Macquarie Island"    = "Australia",
                                                                       "Prince Edward Islands" = "South Africa",
                                                                       "Micronesia" = "Federated States of Micronesia")),
             # Give the same name to countries with varying names (Congo (Republic democratic of) & Republic democratic of)
             final_country = countrycode(sourcevar   = final_country,
                                         origin      = "country.name",
                                         destination = "country.name"),
             iso_code = countrycode(sourcevar   = final_country,
                                    origin      = "country.name",
                                    destination = "iso3c")) |> 
      group_by(iso_code, final_country) |> 
      summarise(Count_ORO_geoP_country = n()) |> 
      filter(!is.na(iso_code))
    
    # ORO_per_admin_geoP <- ORO_per_country_geoP_df_all |> 
    #   group_by(final_admin) |> 
    #   summarise(Count_ORO_geoP_admin = n()) |> 
    #   mutate(iso_code = countrycode(sourcevar   = final_admin,
    #                                 origin      = "country.name",
    #                                 destination = "iso3c"))
    
    # ORO_per_geoP <- full_join(ORO_per_country_geoP |> select(-final_country), ORO_per_admin_geoP |> select(-final_admin), by = "iso_code")
    
    # -- Merge geoparsing and 1st author result
    ORO_per_country_all <- full_join(ORO_per_country_geoP, ORO_per_country_aff, by = "iso_code") |> 
      replace_na(list(Count_ORO_1aff = 0, Count_ORO_geoP_country = 0)) |> 
      select(-final_country) |> 
      mutate(layer = ((Count_ORO_1aff - Count_ORO_geoP_country)/Count_ORO_geoP_country)*100,
             Country = countrycode(sourcevar   = iso_code,
                                   origin      = "iso3c",
                                   destination = "country.name")) |> 
      ungroup()
      
    
  ## --- PLOT DATA
    
    # -- Create the bivariate color scale
    bivariate_color_scale <- color_bivariate_map(nquantiles  = 10, 
                                                 upperleft   = "#be64ac", 
                                                 upperright  = "#434e87",
                                                 bottomleft  = "white",
                                                 bottomright = "#5ac8c8",   
                                                 ylab        = "vulnerability",
                                                 xlab        = "n_weighted_papers")
    
    data_bivar_ORO_per_country_all <- format_data_bivariate_map(data        = ORO_per_country_all,
                                                                data.x      = "Count_ORO_1aff",
                                                                data.y      = "Count_ORO_geoP_country",
                                                                color_table = bivariate_color_scale,
                                                                probs.quant = seq(0,1,0.1))
    
    # --- Format the shapefile of the world countries polygon and bind data
    world_shp_data_OROall <- format_shp_of_the_world(world_shp    = world_shp,
                                                     data_to_bind = data_bivar_ORO_per_country_all,
                                                     PROJ         = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
    tmp <- sf::st_drop_geometry(world_shp_data_OROall)
    data_2_map_OROall <- format_data2map(data = world_shp_data_OROall,
                                         PROJ = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
    
    
    bivariate_map(data_map   = data_2_map_OROall,
                  data_world = NULL,
                  color      = bivariate_color_scale,
                  ylab       = "# ORO geoP",
                  xlab       = "# ORO 1st aff",
                  name       = "main/Figure2_PanelE")
  
    
    data_2_map_OROall$data$layer[data_2_map_OROall$data$layer == Inf] <- 100
    data_2_map_OROall$data$layer[data_2_map_OROall$data$layer > 100] <- 100
    
    # univariate_map(data_map          = data_2_map_OROall,
    #                color_scale       = NULL,
    #                midpoint          = 0,
    #                legend            = "Change",
    #                show.legend       = TRUE,
    #                name              = "main/Fig2_panelE")
    
    ## --- PLOT COMMON DATA
    world_shp_data_OROcommon <- format_shp_of_the_world(world_shp    = world_shp,
                                                        data_to_bind = ORO_per_country_common_Aff_Geop,
                                                        PROJ         = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
    data_2_map_OROcommon <- format_data2map(data = world_shp_data_OROcommon,
                                            PROJ = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
    data_2_map_OROcommon$data$layer[data_2_map_OROcommon$data$layer == Inf] <- 100
    data_2_map_OROcommon$data$layer[data_2_map_OROcommon$data$layer > 100] <- 100
    
    univariate_map(data_map          = data_2_map_OROcommon,
                   color_scale       = c("darkred","white", "darkblue"),
                   second.var        = NULL,
                   midpoint          = 0, 
                   title_color       = "Change in \n #paper (%)",
                   title_size        = NULL,
                   show.legend       = TRUE,
                   name              = "main/Fig2_panelE_common2")
    
    panelB <- univariate_map(data_map          = data_2_map_panelB,
                             color_scale       = c("#4c4680","white", "#197da8"),
                             second.var        = NULL,
                             midpoint          = 0, 
                             title_color       = "% mit. ORO",
                             title_size        = NULL, 
                             show.legend       = TRUE,
                             name              = "main/Fig2_panelB2")
    
    
### ----
  
### ----- DISCONNECT -----
DBI::dbDisconnect(dbcon)



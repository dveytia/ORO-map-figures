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
                                                    PROJ         = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    tmp <- sf::st_drop_geometry(world_shp_boundaries)
    
    # --- Format the data to produce the map
    data_2_map_panelA <- format_data2map(data = world_shp_boundaries,
                                         PROJ = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
    
  ## ---- PLOT PANEL A
  panelA <- univariate_map(data_map          = data_2_map_panelA,
                           color_scale       = viridis::magma(10, direction = -1),
                           midpoint          = NULL,
                           second.var        = NULL,
                           # vals_colors_scale = NULL,
                           title_color       = "#ORO/#O&C (%)",
                           title_size        = NULL,
                           show.legend       = TRUE,
                           name              = "main/Fig2_panelA")
  
  panelA <- univariate_map(data_map          = data_2_map_panelA,
                           color_scale       = viridis::magma(10, direction = -1),
                           midpoint          = NULL,
                           second.var        = "Count_ORO",
                           # vals_colors_scale = NULL,
                           title_color       = "#ORO/#O&C (%)",
                           title_size        = "#ORO paper",
                           show.legend       = TRUE,
                           name              = "main/Fig2_panelA_points")

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
      rename(Country = country_aff) |> 
      mutate(iso_code = countrycode(sourcevar   = Country,
                                    origin      = "country.name",
                                    destination = "iso3c")) 
    
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
                         name        = "main/Fig2_PanelC") ; panelC  

  
### -----
  
  
### ----- PANEL D -----
  
  ## ---- LOAD DATA
    
    # --- LDC = Least Developed Countries
    # --- LLCD = Land Locked Developing Countries
    # --- SIDS = Small Island Developing States
    landlocked <- read.csv(file = here::here("data", "external", "special_country_groups", "landlocked-countries-2024.csv"), sep = ",") |> 
      select(country, LandlockedCountries) |> 
      rename(Country    = country, 
             group_land = LandlockedCountries) |> 
      mutate(iso_code = countrycode(sourcevar   = Country,
                                    origin      = "country.name",
                                    destination = "iso3c"),
             group_land = case_when(group_land == "yes" ~ "Land-locked",
                                    TRUE ~ "NA"))
  
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
      distinct()
      
    
    income_grp <- readxl::read_xlsx(path = here::here("data", "external", "special_country_groups", "income_group.xlsx")) |> 
      select(Economy, Code, `Income group`) |> 
      rename(country      = Economy, 
             iso_code     = Code,
             group_income = `Income group`)
    
    "Czech Republic"
  
  ## ---- FORMAT DATA
  ratio_mitig_adapt2 <- ratio_mitig_adapt |> 
    select(-ratio) |> 
    rename(n_MitPlusAda = mit_ada,
           perc_mit     = layer) |> 
    left_join(income_grp |>  select(-country), by = "iso_code") |> 
    left_join(country_grp |> select(iso_code, group_land), by = "iso_code") |> 
    replace_na(list(group_land = "Coastal"))
  
    ## --- Residuals
    ratio_mitig_adapt2 <- ratio_mitig_adapt2 |> 
      mutate(adaptation_log = log(adaptation + 1),
             mitigation_log = log(mitigation + 1),
             residuals = resid(lm(adaptation_log ~ mitigation_log, data = cur_data())),
             labels    = abs(residuals) >= quantile(abs(residuals), prob = 0.85))
  
  ## ---- PLOT PANEL D
  plot <- ggplot(data    = ratio_mitig_adapt2, 
         mapping = aes(x = mitigation_log, 
                       y = adaptation_log)) +
    
    geom_abline(slope = 1, linetype='dotted') +
    
    geom_point(mapping = aes(color = group_land), show.legend = FALSE) +
    
    facet_grid(group_land ~ .) +
    
    # geom_smooth(method  = lm, 
    #             col     = "grey10") +
    
    ylim(c(0, 9)) +
    xlim(c(0, 9)) +
    
    xlab(label = "# Mit. papers") +
    ylab(label = "# Ada. papers") +
    
    # geom_text_repel(data         = filter(ratio_mitig_adapt2, group_land %in% c("SIDS", "LLC")), 
    #                 mapping      = aes(label = Country, color = group_land),
    #                 max.overlaps = 100,
    #                 show.legend  = FALSE,
    #                 min.segment.length = 0.1) +
    
    scale_color_manual(values = c("Coastal"   = "#3f47e8",
                                  "Land-locked"  = "#b36705",
                                  "SIDS" = "#0fbcd6"),
                       name   = NULL,
                       labels = c("CC"   = "Coastal",
                                  "LDC"  = "Land-locked",
                                  "SIDS" = "SIDS")) +
    
    # scale_shape_manual(values = c("High income"         = 15,
    #                               "Upper middle income" = 16,
    #                               "Lower middle income" = 17,
    #                               "Low income"          = 18),
    #                    name   = "Income",
    #                    labels = c("High income"         = "High",
    #                               "Upper middle income" = "Upper middle",
    #                               "Lower middle income" = "Lower middle",
    #                               "Low income"          = "Low")) +

    theme_bw() +
    # guides(size = "none", color = guide_colourbar(title.position = "top", barwidth = 8, barheight = 0.7)) +
    theme(legend.position = c(0.2,0.9),
          axis.text.x     = element_text(size = 11),
          axis.text.y     = element_text(size = 11),
          axis.title.x    = element_text(size = 13),
          axis.title.y    = element_text(size = 13),
          legend.text     = element_text(size = 12),
          legend.title    = element_text(size  = 13, 
                                         face  = "bold", 
                                         hjust = 0.5, 
                                         vjust = 0.5)) 
  
  ### Change strips backgrounds
  fill_colors = c("#3f47e8", "#b36705", "#0fbcd6")
  plot2 <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(plot))
  strips <- which(startsWith(plot2$layout$name,'strip'))
  
  for (s in seq_along(strips)) {
    plot2$grobs[[strips[s]]]$grobs[[1]]$children[[1]]$gp$fill <- fill_colors[s]
  }
  
  plot(plot2)
  ggplot2::ggsave(plot = plot2, here::here("figures", "main", "Ada_vs_Mit_country_types_v22.jpeg"), width = 5, height = 7, device = "jpeg")

### -----

  
### ---- PANEL E -----
  
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
    
    ORO_land_test <- ORO_geoP_sf_land |> 
      sf::st_drop_geometry() |> 
      select(analysis_id, shp_id) |> 
      left_join(test, by = c("shp_id" = "shpfile_id")) |> 
      filter(!is.na(shp_id)) |> 
      distinct() ; length(unique(ORO_land_test$analysis_id))
    
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
    
    length(unique(ORO_per_country_geoP_df_all$analysis_id)) # n = 17201 from geoparsing
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
                  data_world = world_shp,
                  color      = bivariate_color_scale,
                  ylab       = "# ORO geoP",
                  xlab       = "# ORO 1st aff",
                  name       = "main/Figure2_PanelE3")
  
    
    data_2_map_OROall$data$layer[data_2_map_OROall$data$layer == Inf] <- 100
    data_2_map_OROall$data$layer[data_2_map_OROall$data$layer > 100] <- 100
    
    univariate_map(data_map          = data_2_map_OROall,
                   color_scale       = NULL,
                   midpoint          = 0,
                   legend            = "Change",
                   show.legend       = TRUE,
                   name              = "main/Fig2_panelE")
    
    ## --- PLOT COMMON DATA
    world_shp_data_OROcommon <- format_shp_of_the_world(world_shp    = world_shp,
                                                        data_to_bind = ORO_per_country_common_Aff_Geop,
                                                        PROJ         = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
    data_2_map_OROcommon <- format_data2map(data = world_shp_data_OROcommon,
                                            PROJ = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    
    data_2_map_OROcommon$data$layer[data_2_map_OROcommon$data$layer == Inf] <- 100
    data_2_map_OROcommon$data$layer[data_2_map_OROcommon$data$layer > 100] <- 100
    
    univariate_map(data_map          = data_2_map_OROcommon,
                   color_scale       = NULL,
                   midpoint          = 0,
                   legend            = "Change",
                   show.legend       = TRUE,
                   name              = "main/Fig2_panelE_common")
    
### ----
  
### ----- DISCONNECT -----
DBI::dbDisconnect(dbcon)



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

  ## ---- LOAD & FORMAT DATA
  world_conti_shp <- sf::read_sf(here::here("data", "external", "world_continents_shp")) # shape file of the world
  ccodes <- raster::ccodes() |>  
    select(NAME, continent) |> 
    rename(ccode_continent = continent)

  gdp_per_capita <- readr::read_csv(here::here("data/external/gdp-per-capita/gdp-per-capita-worldbank-2021.csv"), 
                                    show_col_types = FALSE) |>
    filter(Year > 2015) |> 
    rename(country = Entity,
           GDP_per_capita = `GDP per capita, PPP (constant 2017 international $)`) |> 
    group_by(country, Code) |> 
    summarise(GDP_per_capita = median(GDP_per_capita, na.rm = T)) |> 
    left_join(ccodes, by = c("country" = "NAME")) |> 
    mutate(continent = countrycode(sourcevar   = country,
                                   origin      = "country.name",
                                   destination = "continent"),
           continent2 = case_when(continent == "Americas" ~ ccode_continent,
                                  TRUE ~ continent))|>
    filter(!is.na(continent2)) |> 
    select(-ccode_continent, -continent)
  
    
  energy_demand <- readr::read_csv(here::here("data/external/ghg-emissions/owid-co2-data.csv"),
                                   show_col_types = FALSE) |> 
    select(country, year, iso_code, energy_per_capita, energy_per_gdp) |>
    filter(year > 2015) |> 
    group_by(country, iso_code) |> 
    summarise(energy_per_capita = median(energy_per_capita, na.rm = T),
              energy_per_gdp    = median(energy_per_gdp,    na.rm = T)) |> 
    left_join(ccodes, by = c("country" = "NAME")) |> 
    mutate(continent = countrycode(sourcevar   = country,
                                   origin      = "country.name",
                                   destination = "continent"),
           continent2 = case_when(continent == "Americas" ~ ccode_continent,
                                  TRUE ~ continent))|>
    filter(!is.na(continent2)) |> 
    select(-ccode_continent, -continent)
    
  
  
  ## ---- CHECK IF ANY CORRELATION BETWEEN BOTH VARIABLES
  energy_gdp <- full_join(energy_demand, gdp_per_capita |> ungroup() |> select(-continent2, -country), by = c("iso_code" = "Code")) |> drop_na()

  correlation_btw_var(data       = energy_gdp,
                      log.transf = FALSE, 
                      quant.prob = 0.9,
                      name       = "supplemental/energy_vs_GFD")
  
  correlation_btw_var(data       = energy_gdp,
                      log.transf = TRUE, 
                      quant.prob = 0.9,
                      name       = "supplemental/energy_vs_GFD_log")
  # https://ourworldindata.org/grapher/energy-use-per-person-vs-gdp-per-capita
  
  ## ==> Strong correlation, not necessary to use both variables. Cartogram with GDP only
  
  ## ---- Format GDP data
  gdp_per_capita <- readr::read_csv(here::here("data/external/gdp-per-capita/gdp-per-capita-worldbank-2021.csv"), 
                                    show_col_types = FALSE) |>
    filter(Year > 2015 & Year < 2020) |> 
    rename(country = Entity,
           GDP_per_capita = `GDP per capita, PPP (constant 2017 international $)`) |> 
    group_by(country, Code) |> 
    summarise(GDP_per_capita = median(GDP_per_capita, na.rm = T)) |> 
    left_join(ccodes, by = c("country" = "NAME")) |> 
    mutate(continent = countrycode(sourcevar   = country,
                                   origin      = "country.name",
                                   destination = "continent"),
           continent2 = case_when(continent == "Americas" ~ ccode_continent,
                                  country   == "Russia"   ~ "Asia",
                                  TRUE ~ continent)) |>
    filter(!is.na(continent2)) |> 
    group_by(continent2) |> 
    summarise(GDP_per_capita = median(GDP_per_capita, na.rm = T)) 

  
  ## --- World shapefile, group Australia and Oceania
  world_conti_shp2 <- world_conti_shp |> 
    mutate(CONTINENT = case_when(CONTINENT == "Australia" ~ "Oceania",
                                 TRUE ~ CONTINENT)) |> 
    group_by(CONTINENT) |> 
    summarise(geometry = sf::st_union(geometry))
  
  plot(world_conti_shp["CONTINENT"])
  plot(world_conti_shp2["CONTINENT"])
  
  gdp_per_capita_sf <- left_join(world_conti_shp2 |> select(CONTINENT), gdp_per_capita, by = c("CONTINENT" = "continent2")) |> 
    sf::st_transform(crs = 3857) |> # 53030
    filter(CONTINENT != "Antarctica")
  

  ## ---- PLOT DATA
  plot(gdp_per_capita_sf["GDP_per_capita"])
  ## !!! AUSTRALIA low because median value with oceanian's countries with low GDP per capita.
  
    # --- Format data to obtain a cartogram
    gdp_conti_carto <- cartogram_cont(gdp_per_capita_sf, "GDP_per_capita", itermax = 5)
    plot(gdp_conti_carto["GDP_per_capita"])
    
    Figure6_PanelA <- ggplot() +
      geom_sf(data = gdp_conti_carto, aes(color = CONTINENT, fill  = GDP_per_capita), size = 1, show.legend = TRUE) +
      scale_fill_viridis_c(option = "viridis", name = "GDP per capita") +
      scale_color_manual(name = NULL,
                         values = c("Africa" = "#9c40b8",
                                    "Asia"   = "#1f7819",
                                    "Europe" = "#3456ad",
                                    "North America" = "#e89338",
                                    "Oceania"       = "#7d431f",
                                    "South America" = "#eb4e49"),
                         guide = "none") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank()) ; Figure6_PanelA
    
    ggsave(here::here("figures", "main", "Figure6_PanelA.jpeg"), width = 7, height = 5, device = "jpeg")

  
  
### -----


### ----- PANEL B -----

  ## ---- LOAD DATA
  pred_oro_type_long <- tbl(dbcon, "pred_oro_type_long") %>% collect()
  uniquerefs <- tbl(dbcon, "uniquerefs") # metadata on the unique references
  countries_ls <- read.csv(file = here::here("data", "external", "list_of_countries", "sql-pays.csv"), sep = ";") # Countries names
  world_shp <- sf::read_sf(here::here("data", "external", "world_shp")) # shape file of the world
  
  ## ---- FORMAT DATA
  
    # --- Join with author affiliation data from uniquerefs
    pred_oro_type_continent <- uniquerefs %>%
      select(analysis_id, affiliation) %>%
      collect()%>%
      right_join(pred_oro_type_long, by = "analysis_id")
    
    # --- Find 1st author affiliation
    pred_oro_type_continent1A <- extract_1stA_affiliation(data         = pred_oro_type_continent, 
                                                          countries_ls = countries_ls)
    
    # --- Find the corresponding continent and sumamrise
    ccodes <- raster::ccodes() |>  
      select(NAME, continent) |> 
      rename(ccode_continent = continent)
    
    pred_oro_type_continent_1Aaff <- pred_oro_type_continent1A$oroAff_1stA |> 
      left_join(ccodes, by = c("country_aff" = "NAME")) |> 
      mutate(continent = countrycode(sourcevar   = country_aff,
                                     origin      = "country.name",
                                     destination = "continent"),
             continent2 = case_when(continent == "Americas" ~ ccode_continent,
                                    TRUE ~ continent)) |>
      filter(!is.na(continent2)) 
    
    # --- Change Russia in the Asian continent
    pred_oro_type_continent_1Aaff_RussiaAsia = pred_oro_type_continent_1Aaff
    pred_oro_type_continent_1Aaff_RussiaAsia$continent2[pred_oro_type_continent_1Aaff_RussiaAsia$country_aff == "Russian Federation"] <- "Asia"
    
    # !!!! CHECK WITH DEVI !!!!
    # --- Select the most relevant oro_type predicted and estimate the # paper per continent, oro_branch and oro_type
    npaper_per_conti <- pred_oro_type_continent_1Aaff |> 
      group_by(analysis_id) |>
      # slice_max(order_by = mean) |> 
      group_by(continent2, oro_branch, oro_type) |> 
      # summarise(n_paper = n())
      summarise(n_mean = sum(0.5 <= mean),
                n_lower = sum(0.5 <= lower),
                n_upper = sum(0.5 <= upper)) |> 
      arrange(continent2, oro_branch, -n_mean) |>
      ungroup() |> 
      mutate(valueOrder = as.factor(row_number()))
    
      
    # tmp1 = filter(pred_oro_type_continent_1Aaff, continent == "Africa" & oro_branch == "Mitigation" & oro_type == "CO2 removal or storage") ;  nrow(tmp)
    # tmp2 = filter(pred_oro_type_continent_1Aaff, continent == "Africa" & oro_branch == "Mitigation" & oro_type == "Increase efficiency") ;  nrow(tmp)
    # 
    #!!! Pb with Mali & Reunion & Guadeloupe !!! 
    # sum(is.na(pred_oro_type_continent_1Aaff$continent))
    # tmp_NA <- filter(pred_oro_type_continent_1Aaff, is.na(continent)) |> select(-country_aff)
    # tmp_NA <- extract_1stA_affiliation(data         = tmp_NA, 
    #                                    countries_ls = countries_ls)
    
    # --- Format data for the plot
    # Set a number of 'empty bar' to add at the end of each group
    empty_bar <- 2
    to_add <- data.frame(matrix(NA, empty_bar*nlevels(as.factor(npaper_per_conti$continent2)), ncol(npaper_per_conti)))
    colnames(to_add) <- colnames(npaper_per_conti)
    to_add$continent2 <- rep(levels(as.factor(npaper_per_conti$continent2)), each = empty_bar)
    data <- rbind(npaper_per_conti, to_add)
    data <- data %>% 
      arrange(continent2, oro_branch, -n_mean)
    data$id <- seq(1, nrow(data))
    data <- data[-c(53:54),]
    
    # prepare a data frame for base lines
    base_data <- data %>% 
      group_by(continent2) %>% 
      summarize(start=min(id), end=max(id) - empty_bar) %>% 
      rowwise() %>% 
      mutate(title=mean(c(start, end)))
    
  ## ---- PLOT DATA
  Figure6_PanelB <- ggplot(data, aes(x = id, y = n_mean, fill = oro_type)) +
      
      # Bars
      geom_col(position = position_dodge(), show.legend = T) +
      geom_errorbar(aes(ymin = n_lower, ymax = n_upper), 
                    position = position_dodge(0.9),
                    width = .2) +

    # Colors
    scale_fill_manual(name   = "ORO type",
                      values = c("Marine renewable energy"            = "#026996",
                                 "CO2 removal or storage"             = "#0688c2",
                                 "Increase efficiency"                = "#9ed7f0",                               
                                 "Conservation"                       = "#078257",
                                 "Human assisted evolution"           = "#43b08a",
                                 "Built infrastructure & technology"  = "#600787",
                                 "Socio-institutional"                = "#ad5ad1")) +

      labs(y = "Articles predicted relevant (n)") +
      theme_bw() +
      theme(legend.position = c(0.85,0.75),
            axis.title.x    = element_blank(),
            axis.text.x     = element_blank(),
            axis.ticks.x    = element_blank(),
            axis.title.y    = element_text(size = 16),
            axis.text.y     = element_text(size = 14),
            legend.text     = element_text(size = 14),
            legend.title    = element_text(size = 16)) +
    
      # geom_text(data = base_data, aes(x = title, y = -500, label=continent2), hjust=c(rep(0.5,5), 0.37), colour = "black", alpha=0.8, size=5.5, fontface="bold", inherit.aes = FALSE)
      geom_text(data = base_data, aes(x = title, y = -500, label=continent2), hjust=c(rep(0.5,5), 0.37), 
                colour = c("#9c40b8","#1f7819","#3456ad","#e89338","#7d431f","#eb4e49"), 
                size=5.5, fontface="bold", inherit.aes = FALSE)
  
    ggsave(here::here("figures", "main", "Figure6_PanelB_RussiaInAsia.jpeg"), width = 13, height = 5, device = "jpeg")

    c("#9c40b8","#1f7819","#3456ad","#e89338","#7d431f","#eb4e49")
    
    
### ----- 
    
### ----- ARRANGE PANEL
library(cowplot)
Figure6 <- ggdraw() +
  draw_plot(Figure6_PanelA, x = 0.00, y = 0.50, width = 1, height = 0.5) +
  draw_plot(Figure6_PanelB, x = 0.00, y = 0.00, width = 1, height = 0.5)

ggsave(here::here("figures", "main", "Figure6.jpeg"), width = 13, height = 5, device = "jpeg")

    
### ----- DISCONNECT -----
DBI::dbDisconnect(dbcon)
    
    
    
    
    
    
    
        
  branchPal <- c("Mitigation" = "#35a7d9", 
                 "Nature" = "forestgreen", 
                 "Societal"="#7670a8")

  ggplot(npaper_per_conti, aes(x = continent2, y = n_mean, fill = oro_type)) +
    geom_col(position = position_dodge()) +
    geom_errorbar(aes(ymin = n_lower, ymax = n_upper), 
                  position = position_dodge(0.9),
                  width = .2) +
    # geom_text(aes(label = n_mean), position = position_dodge(0.9), size = 3, col="red") + #nudge_x=0.2,
    scale_fill_manual(values = c("Marine renewable energy"            = "#026996",
                                 "CO2 removal or storage"             = "#0688c2",
                                 "Increase efficiency"                = "#9ed7f0",
                                 "Human assisted evolution"           = "#078257",
                                 "Conservation"                       = "#43b08a",
                                 "Built infrastructure & technology"  = "#ad5ad1",
                                 "Socio-institutional"                = "#600787")) +
    # scale_x_discrete(limits = levels(temp$valueOrder), 
    #                  labels = gsub("_"," ", temp$oro_type[order(temp$valueOrder)]))+
    #ylim(c(0, max(temp$n_upper)+2000))+
    labs(y="Articles predicted relevant (n)") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle=45, hjust=1),
      axis.title.x = element_blank()
    )
    


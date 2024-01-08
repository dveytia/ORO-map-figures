#################################################################################################
#                                                                                               #
# FIGURE 5                                                                                      #
# Panel A: Map of mitigation options and CO2 country emisssions (two color scales)              #  
#          (1) Bivariate CS (EEZ's color) ==> CO2 em. per country ~ # ORO pub on BC and MRE)    # 
#          (2) Univariate color scale (countrie's color) ==> # pub mitigations ORO (1st author) #
# Panel B:                               #
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

### ----- LOAD FUNCTIONS -----
source(here::here("R", "functions_to_format.R")) # all functions needed to format data
source(here::here("R", "functions_to_plot.R")) # all functions needed to plot data


### ----- CONNECTION TO THE LATEST VERSION OF THE SQL DATABASE -----
sqliteDir <- here::here("data/sqlite-databases")
sqliteFiles <- dir(sqliteDir)
sqliteVersions <- as.numeric(gsub(".sqlite","",substring(sqliteFiles, regexpr("_v", sqliteFiles) + 2)))
latestVersion <- sqliteFiles[which.max(sqliteVersions)]
dbcon <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(sqliteDir, latestVersion), create=FALSE)

### ----- LOAD DATA
pred_blue_carbon <- tbl(dbcon, "pred_blue_carbon")
shp_df_matches <- tbl(dbcon, "geoparsed-text_shp_df_matches") 
grid_df <- tbl(dbcon, "grid_df_res2.5") 
world_shp <- sf::read_sf(here::here("data", "external", "world_shp")) # shape file of the world
pred_oro_any_mitigation <- tbl(dbcon, "pred_oro_any_mitigation")


### ----- Perform calculations to give you the weighted number of articles relevant to blue carbon or mitigation
### ----- found within each grid cell
mitigation_grid_df <- pred_blue_carbon %>%
  # Get predictions for blue carbon and MRE for each article
  full_join(pred_oro_any_mitigation, by = "analysis_id") %>%
  rename(blue_carbon = `0 - relevance - mean_prediction`,
         marine_renewable_energy = `oro_any.M_Renewables - mean_prediction`) %>%
  select(analysis_id, blue_carbon, marine_renewable_energy) %>%
  # Filter to only the articles that are relevant for either
  mutate(blue_carbon = ifelse(0.5 <= blue_carbon, 1,0),
         marine_renewable_energy = ifelse(0.5 <= marine_renewable_energy, 1, 0)) %>%
  filter(blue_carbon == 1 | marine_renewable_energy == 1) %>% 
  # Join with info about which grid cells they match to
  left_join(shp_df_matches, by = "analysis_id") %>%
  # for each unique grid cell, take the weighted sum
  group_by(grid_df_id)%>%
  summarise(n_articles_weighted = sum(cell_weight)) %>% 
  # Join with information about where to plot the grid cells
  right_join(grid_df, by = "grid_df_id") %>%
  # This last step collects all the relevant data from the database 
  collect() 
  
### ----- PLOT DATA
  
  # - Trasform into a spatial object
  PROJ = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

  mitigation_grid_sf <- sf::st_as_sf(mitigation_grid_df, coords = c("LON", "LAT"), crs = 4326) |> 
    sf::st_transform(crs = PROJ)
  
  ggplot(data = mitigation_grid_sf) +
    geom_sf(data = world_shp, fill = "grey95") +
    geom_sf(aes(color = n_articles_weighted)) +
    scale_colour_viridis_c(na.value = "green") +
    theme_bw()
  
### REMARK 1: many cell in the terrestrial realm are colored, I don't understand why...?
### REMARK 2: why all the Indian ocean is filled with NAs?
  
  # SELECT LAND DATA
  mitigation_grid_sf_LAND <- filter(mitigation_grid_sf, is_land == 1)
  ggplot(data = mitigation_grid_sf_LAND) +
    geom_sf(data = world_shp, fill = "grey95") +
    geom_sf(aes(color = n_articles_weighted)) +
    scale_colour_viridis_c(na.value = "white") +
    theme_bw()
  
  
### I tried to understand why terrestrial cells are not filled with NA  
### For that, I keep informations about analysis_id and place (i.e., did not compute the weighted sum for each grid cell)
tmp <- pred_blue_carbon %>%
  # Get predictions for blue carbon and MRE for each article
  full_join(pred_oro_any_mitigation, by = "analysis_id") %>%
  rename(blue_carbon = `0 - relevance - mean_prediction`,
         marine_renewable_energy = `oro_any.M_Renewables - mean_prediction`) %>%
  select(analysis_id, blue_carbon, marine_renewable_energy) %>%
  # Filter to only the articles that are relevant for either
  mutate(blue_carbon = ifelse(0.5 <= blue_carbon, 1,0),
         marine_renewable_energy = ifelse(0.5 <= marine_renewable_energy, 1, 0)) %>%
  filter(blue_carbon == 1 | marine_renewable_energy == 1) %>% 
  # Join with info about which grid cells they match to
  left_join(shp_df_matches, by = "analysis_id") %>% 
  # for each unique grid cell, take the weighted sum
  # group_by(grid_df_id)%>%
  # summarise(n_articles_weighted = sum(cell_weight)) %>%
  # Join with information about where to plot the grid cells
  right_join(grid_df, by = "grid_df_id") %>%
  # This last step collects all the relevant data from the database 
  collect() 
  
  # DATA ON LAND
  tmp_land <- filter(tmp, is_land == 1)


### ----------------------
### TEST for "place" identified as "bolivarian republic of venezuela"
### ----------------------
plot_geoparsing(data         = tmp_land,
                world        = world_shp, 
                land         = TRUE, 
                Place2Filter = "bolivarian republic of venezuela")
  
tmp_land_VNZ <- sf::st_as_sf(tmp_land |> filter(place == "bolivarian republic of venezuela"), coords = c("LON", "LAT"), crs = 4326)

### REMARK 3: Several points are outside Venezualian borders (i.e., in other countries)
### There is a mismatch between the LAT LON data and the indication found in the column PLACE ...
### I don't understand why?
  

### ----------------------
### TEST for "place" identified as "republic of france" 
### ----------------------
plot_geoparsing(data         = tmp_land,
                world        = world_shp, 
                land         = TRUE, 
                Place2Filter = "republic of france")

tmp_land_FR <- sf::st_as_sf(tmp_land |> filter(place == "republic of france"), coords = c("LON", "LAT"), crs = 4326)

### SAME QUESTION AS THE ONE FOLLOWING REMARK 3.
### I tried to understand by plotting one paper (I randomly took the analysis_id = 31942)

# Analysis_id = 31942 with 15 lines (15 different LAT LON)
FR_31942 <- sf::st_as_sf(tmp_land_FR |> filter(analysis_id == 31942), coords = c("LON", "LAT"), crs = 4326) # try with 2249
ggplot(data = FR_31942) +
  geom_sf(data = world_shp, fill = "grey95") +
  geom_sf(color = "red") +
  theme_bw()

### REMARK 4: I found that for each analysis_id, there are 15 different LAT & LON values. 
### and each analysis_id have the same 15 LAT&LON values
length(unique(tmp_land_FR$analysis_id))
nrow(tmp_land_FR)/15

### I extracted the title of the paper and downloaded the PDF
### You can download it here: https://www-sciencedirect-com.ezpum.scdi-montpellier.fr/science/article/pii/S0924796320301305?ref=cra_js_challenge&fr=RR-1
### I don't understand why there are several LAT&LON values outside France since the study site corresponds to the Bay of Seine (North of France)
uniquerefs <- tbl(dbcon, "uniquerefs") |> 
  filter(analysis_id == 31942) |> 
  collect() ; uniquerefs$title

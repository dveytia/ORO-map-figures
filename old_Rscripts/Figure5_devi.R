
## FIGURE 5: MATCH/MISMATCH BETWEEN RESEARCH EFFORTS AND CHALLANGES/NEEDS


# here I have started you off by showing how to access the data relevant for each of the panels, 
# and which calculations need to come next (if any)


# Path to latest version of sqlite database
sqliteDir <- here::here("data/sqlite-databases")
sqliteFiles <- dir(sqliteDir)
sqliteVersions <- as.numeric(gsub(".sqlite","",substring(sqliteFiles, regexpr("_v", sqliteFiles) + 2)))
latestVersion <- sqliteFiles[which.max(sqliteVersions)]

# Connect to latest version of the database
dbcon <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(sqliteDir, latestVersion), create=FALSE)
src_dbi(dbcon) # just to explore prints out all the tables



## PANEL A: Map – Mitigation (bivariate fill in grid cells in EEZ 
# x axis = # blue carbon + MRE in grid cell, y axis = CO2 emissions of country in EEZ; 
# fill in countries = #pub of Mitigation OROs by 1st author affiliation) 
#[+ GLOBAL inset of global Mitigation OROs?]


# For the bivariate plot of blue carbon vs MRE
# get data tables
grid_df <- tbl(dbcon, "grid_df_res2.5")
shp_df_matches <- tbl(dbcon, "geoparsed-text_shp_df_matches")
pred_oro_any_mitigation <- tbl(dbcon, "pred_oro_any_mitigation")
pred_blue_carbon <- tbl(dbcon, "pred_blue_carbon")
# Perform calculations to give you the weighted number of articles relevant to blue carbon or mitigation
# found within each grid cell
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
  summarise(n_articles_weighted = sum(cell_weight))%>% 
  # Join with information about where to plot the grid cells
  right_join(grid_df, by = "grid_df_id") %>%
  # This last step collects all the relevant data from the database 
  collect() 


# For filling in the countries for #pub of Mitigation OROs by 1st author affiliation)
uniquerefs <- tbl(dbcon, "uniquerefs") # metadata on the unique references
pred_oro_branch <- tbl(dbcon, "pred_oro_branch") # predictions for ORO branch
# Just simplify and create a column that idenfies with 1 or 0 whether a publication is relevant 
# for mitiagion 
mitPubs <- pred_oro_branch %>%
  mutate(mitigation = ifelse(0.5 <= `oro_branch.Mitigation - mean_prediction`, 1,0))%>%
  select(analysis_id, mitigation) %>%
  # Join with affiliation information
  left_join(uniquerefs %>% select(analysis_id, affiliation), by = "analysis_id") %>%
  collect()
# Now what you need to do is condense the affiliation to just the country name, 
# and then count the number of mitigation articles in each country






## PANEL B: Map – Adaptation (bivariate fill in grid cells in EEZ - 
# x axis = # Adaptation ORO sites in grid cell, 
# y axis = vulnerability of country in EEZ (e.g. low elevation coastal zone); 
# fill in countries = #pub of Adaptation OROs by 1st author affiliation)


# For bivariate plot of adaptation vs vulnerabiity
grid_df <- tbl(dbcon, "grid_df_res2.5")
shp_df_matches <- tbl(dbcon, "geoparsed-text_shp_df_matches")
pred_oro_branch <- tbl(dbcon, "pred_oro_branch") # predictions for ORO branch

adaptation_grid_df <- pred_oro_branch %>%
  # Get predictions for adaptation relevance each article and filter
  # to only relevant articles
  mutate(adaptation = ifelse(0.5 <= `oro_branch.Nature - mean_prediction` |
                               0.5 <= `oro_branch.Societal - mean_prediction`,
                             1,0))%>%
  select(analysis_id, adaptation) %>%
  filter(adaptation == 1) %>%
  # Join with info about which grid cells they match to
  left_join(shp_df_matches, by = "analysis_id") %>%
  # for each unique grid cell, take the weighted sum
  group_by(grid_df_id)%>%
  summarise(n_articles_weighted = sum(cell_weight))%>% 
  # Join with information about where to plot the grid cells
  right_join(grid_df, by = "grid_df_id") %>%
  # This last step collects all the relevant data from the database 
  collect() 
# From here you will need to find which grid cells belong in which country's eez
# so that you can assign a country to each row, and then join this with the 
# vulnerability index from the data files in data/external/climate-hazard-maps
# This folder contains the raster files from https://doi.org/10.1038/s43247-022-00577-5. 
# I believe we want a combined map of hazards, so the next steps would be to normalize the 
# 4 maps (1-10 scores), add them together and take averages for each EEZ.


# For fill in countries = #pub of Adaptation OROs by 1st author affiliation)
uniquerefs <- tbl(dbcon, "uniquerefs") # metadata on the unique references
pred_oro_branch <- tbl(dbcon, "pred_oro_branch") # predictions for ORO branch
# Just simplify and create a column that idenfies with 1 or 0 whether a publication is relevant 
# for mitiagion 
adaptPubs <- pred_oro_branch %>%
  mutate(adaptation = ifelse(0.5 <= `oro_branch.Nature - mean_prediction` |
                               0.5 <= `oro_branch.Societal - mean_prediction`,
                             1,0))%>%
  select(analysis_id, adaptation) %>%
  # Join with affiliation information
  left_join(uniquerefs %>% select(analysis_id, affiliation), by = "analysis_id") %>%
  collect()
# Now what you need to do is condense the affiliation to just the country name, 
# and then count the number of adaptation articles in each country

  
  
  
  
  



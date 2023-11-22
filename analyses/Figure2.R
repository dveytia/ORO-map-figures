#TestPush


## I'm including some example code of how sqlite is useful. 
# Mainly it can be used to join and filter across large datasets without having to import
# all of them into your workspace. 

# for example, say I wanted to filter the geoparsing results to only the articles that
# are predicted relevant for blue carbon. I could do:

# load libraries
library(dplyr)
library(dbplyr)
library(R.utils)
library(RSQLite)

# Path to latest version of sqlite database
sqliteDir <- here::here("data/sqlite-databases")
sqliteFiles <- dir(sqliteDir)
sqliteVersions <- as.numeric(gsub(".sqlite","",substring(sqliteFiles, regexpr("_v", sqliteFiles) + 2)))
latestVersion <- sqliteFiles[which.max(sqliteVersions)]

# Connect to latest version of the database
dbcon <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(sqliteDir, latestVersion), create=FALSE)


# Point to relevant tables (does not actually load them)
grid_df <- tbl(dbcon, "grid_df_res2.5")
shp_df_matches <- tbl(dbcon, "geoparsed-text_shp_df_matches")
pred_blue_carbon <- tbl(dbcon, "pred_blue_carbon")


# Perform calculations to give you the weighted number of articles relevant to blue carbon
# found within each grid cell
df <- pred_blue_carbon %>%
  # only look at relevant articles 
  filter(`0 - relevance - mean_prediction` > 0.5) %>% 
  # Join with info about which grid cells they match to
  left_join(shp_df_matches, by = "analysis_id") %>%
  # for each unique grid cell, take the weighted sum
  group_by(grid_df_id)%>%
  summarise(n_articles_weighted = sum(cell_weight))%>% 
  # Join with information about where to plot the grid cells
  right_join(grid_df, by = "grid_df_id") %>%
  # This last step collects all the relevant data from the database 
  collect() 


## IMPORTANT -- when done with the database (i.e. after collect)
# disconnect
DBI::dbDisconnect(dbcon)


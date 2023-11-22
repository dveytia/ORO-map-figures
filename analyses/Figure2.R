#TestPush


################# EXAMPLES OF USING SQLITE ########################


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
src_dbi(dbcon) # just to explore prints out all the tables

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




################# STARTING DATA CALCULATIONS ########################



# Path to latest version of sqlite database
sqliteDir <- here::here("data/sqlite-databases")
sqliteFiles <- dir(sqliteDir)
sqliteVersions <- as.numeric(gsub(".sqlite","",substring(sqliteFiles, regexpr("_v", sqliteFiles) + 2)))
latestVersion <- sqliteFiles[which.max(sqliteVersions)]

# Connect to latest version of the database
dbcon <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(sqliteDir, latestVersion), create=FALSE)
src_dbi(dbcon) # just to explore prints out all the tables



## Panel A - Map of # ORO publications/# publications on ocean & climate  in each country

# Number of ORO publications by country
# Get relevant tables
uniquerefs <- tbl(dbcon, "uniquerefs") # metadata on the unique references
pred_relevance <- tbl(dbcon, "pred_relevance") # which articles are relevant to OROs
# Subset to relevant rows and get the affiliation
oroAffiliations <- pred_relevance %>%
  filter(0.5 <= relevance_mean) %>%
  left_join(uniquerefs, by = "analysis_id") %>%
  select(analysis_id, affiliation) %>%
  collect()
# Now what you need to do is condense the affiliation to just the country name


# Number of publications on ocean & climate by country as found on WOS
oceanClimate_byCountry <- read.delim(
  here::here("data/external/ocean-and-climate-publications/WOS_ocean-and-climate_by-country_2023-11-21.txt")
)



## Panel B - Map of ratio of mitigation/adaptation ORO publications
uniquerefs <- tbl(dbcon, "uniquerefs") # metadata on the unique references
pred_oro_branch <- tbl(dbcon, "pred_oro_branch") # predictions for ORO branch

# Just simplify and create a column that idenfies with 1 or 0 whether a publication is relevant 
# for mitiagion or adaptation
mitAdaptPubs <- pred_oro_branch %>%
  mutate(adaptation = ifelse(0.5 <= `oro_branch.Nature - mean_prediction` |
                          0.5 <= `oro_branch.Societal - mean_prediction`,
                        1,0),
         mitigation = ifelse(0.5 <= `oro_branch.Mitigation - mean_prediction`, 1,0))%>%
  select(analysis_id, adaptation, mitigation) %>%
  # Join with affiliation information
  left_join(uniquerefs %>% select(analysis_id, affiliation), by = "analysis_id") %>%
  collect()

# Now what you need to do is condense the affiliation to just the country name, 
# and then count the number of mitigation and adaptation articles in each country, 
# and then get the ratio



## Panel C - Scatter plot (1 point = 1 country) : y = # ORO publication, x = total #pub on ocean & climate,
# point colour = predominant ORO (i.e. mitigation vs adaptation)

# I think for this, you have all the data you need already loaded from above, 
# it just involves different manipulations



## Disconnect database when done loading data
DBI::dbDisconnect(dbcon)




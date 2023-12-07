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
sqliteVersions <- as.numeric(gsub(".sqlite", "", substring(sqliteFiles, regexpr("_v", sqliteFiles) + 2)))
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
rm(list = ls(), envir = .GlobalEnv)


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
# Countries names
countries_ls <- read.csv(file = here::here("data", "external", "list_of_countries", "sql-pays.csv"), sep = ";")


# Identify the nationality of the 1st author
NA_affiliation <- filter(oroAffiliations, is.na(affiliation))

oroAffiliations_country1stA <- oroAffiliations |> 
  dplyr::filter(!is.na(affiliation)) |> 
  dplyr::mutate(country_aff = stringr::str_extract(affiliation, paste(countries_ls$name_en, collapse = "|"))) 



# Correct names to match with countries_ls
NA_country <- filter(oroAffiliations_country1stA, is.na(country_aff)) |> 
  filter(!is.na(affiliation)) |> 
  mutate(affiliation = stringr::str_replace_all(affiliation, c("England"       = "United Kingdom",
                                                               "ENGLAND"       = "United Kingdom",
                                                               "Engl"          = "United Kingdom",
                                                               "Wales"         = "United Kingdom",
                                                               "WALES"         = "United Kingdom",
                                                               "Scotland"      = "United Kingdom",
                                                               "Irel"          = "United Kingdom",
                                                               "North Irel"    = "United Kingdom",
                                                               "UK"            = "United Kingdom",
                                                               "U.K"           = "United Kingdom",
                                                               "British"       = "United Kingdom",
                                                               "Great Britain" = "United Kingdom",
                                                               "Scotl"         = "United Kingdom",
                                                               "USA"           = "United States",
                                                               "U.S.A"         = "United States",
                                                               "Russia"        = "Russian Federation",
                                                               "USSR"          = "Russian Federation",
                                                               "Iran"          = "Islamic Republic of Iran",
                                                               "ITALY"         = "Italy",
                                                               "Norw"          = "Norway",
                                                               "NORWAY"        = "Norway",
                                                               "Tanzania"      = "United Republic Of Tanzania",
                                                               "Macedonia"     = "The Former Yugoslav Republic of Macedonia",
                                                               "AUSTRALIA"     = "Australia",
                                                               "Macau"         = "Macao",
                                                               "Can,"          = "Canada,",
                                                               "CANADA"        = "Canada",
                                                               "Viet Nam"      = "Vietnam",
                                                               "VIETNAM"       = "Vietnam",
                                                               "JAPAN"         = "Japan",
                                                               "Jpn"           = "Japan",
                                                               "JPN"           = "Japan",
                                                               "Finl"          = "Finland",
                                                               "PHILIPPINES"   = "Philippines",
                                                               "U Arab Emirates"       = "United Arab Emirates",
                                                               "Belg"                    = "Belgium",
                                                               "BELGIUM"                 = "Belgium",
                                                               "Switz"                   = "Switzerland",
                                                               "SWITZERLAND"             = "Switzerland",
                                                               "NORWAY"                  = "Norway",
                                                               "Trinidad Tobago"         = "Trinidad and Tobago",
                                                               "West Indies"             = "West Indies, Trinidad and Tobago",
                                                               "INDIA"                   = "India",
                                                               "SOUTH AFRICA"            = "South Africa",
                                                               "FRANCE"                  = "France",
                                                               "Fr"                      = "France",
                                                               "Swed"                    = "Sweden",
                                                               "GERMANY"                 = "Germany",
                                                               "West Ger"                = "Germany",
                                                               "NETHERLANDS"             = "Netherlands",
                                                               "UKRAINE"                 = "Ukraine",
                                                               "Isr"                     = "Israel",
                                                               "ISRAEL"                  = "Israel",
                                                               "EGYPT"                   = "Egypt",
                                                               "POLAND"                  = "Poland",
                                                               "HONG KONG"               = "Hong Kong",
                                                               "Port"                    = "Portugal",
                                                               "Korea (the Republic of)" = "Republic of Korea",
                                                               "DENAMARK"                = "Denmark",
                                                               "INDONESIA"               = "INDONESIA",
                                                               "GAMBIA"                  = "Gambia",
                                                               "GREECE"                  = "Greece",
                                                               "S Afr"                   = "South Africa",
                                                               "MALAYSIA"                = "Malaysia",
                                                               "Syria"                   = "Syrian Arab Republic",
                                                               "Marshall Island"         = "Marshall Islands",
                                                               "Micronesia"              = "Federated States of Micronesia",
                                                               "Moldova"                 = "Republic of Moldova",
                                                               "Neth Antilles"           = "Netherlands Antilles",
                                                               "North Korea"             = "Democratic People's Republic of Korea"))) |> 
  dplyr::mutate(country_aff = stringr::str_extract(affiliation, paste(countries_ls$name_en, collapse = "|"))) 

oroAffiliations_country1stA2 <- dplyr::filter(NA_country, !is.na(country_aff)) |> 
  rbind(oroAffiliations_country1stA)

ORO_per_country <- dplyr::filter(NA_country, !is.na(country_aff)) |> 
  rbind(oroAffiliations_country1stA) |> 
  dplyr::filter(!is.na(country_aff)) |> 
  dplyr::group_by(country_aff) |> 
  dplyr::summarise(Count_ORO = n())

NA_country <- filter(NA_country, is.na(country_aff))
save(ORO_per_country, file = here::here("R", "ORO_per_country.RData"))


# test4 <- oroAffiliations |> 
#   dplyr::mutate(as.data.frame(stringr::str_extract_all(oroAffiliations$affiliation, paste(countries_ls$name_en, collapse = "|"), simplify = TRUE))) |> 
#   tidyr::unite("all_countries", starts_with("V"), sep = ",") |> 
#   tidyr::separate_rows(all_countries, sep = ",") 
# 
# test4.2 <- test4 |> 
#   distinct() |> 
#   group_by(analysis_id) |> 
#   
#   drop_na()
#   summarise(country_aff = paste(sort(all_countries, na.last = FALSE), collapse = "&"))


# Number of publications on ocean & climate by country as found on WOS
oceanClimate_byCountry <- read.delim(here::here("data/external/ocean-and-climate-publications/WOS_ocean-and-climate_by-country_2023-11-21.txt")) |> 
  dplyr::mutate(Countries.Regions = stringr::str_to_title(Countries.Regions)) |> 
  dplyr::mutate(Countries.Regions = stringr::str_replace_all(Countries.Regions, c("England"            = "United Kingdom",
                                                                                  "Scotland"           = "United Kingdom",
                                                                                  "Falkland Island"    = "United Kingdom",
                                                                                  "Usa"                = "United States",
                                                                                  "Russia"             = "Russian Federation",
                                                                                  "Iran"               = "Islamic Republic of Iran",
                                                                                  "Wales"              = "United Kingdom",
                                                                                  "U Arab Emirates"    = "United Arab Emirates",
                                                                                  "Tanzania"           = "United Republic Of Tanzania",
                                                                                  "Turkiye"            = "Turkey",
                                                                                  "Trinidad Tobago"    = "Trinidad and Tobago",
                                                                                  "Brunei"             = "Brunei Darussalam",
                                                                                  "Ussr"               = "Russian Federation",
                                                                                  "Syria"              = "Syrian Arab Republic",
                                                                                  "Bosnia Herceg"      = "Bosnia and Herzegovina",
                                                                                  "Antigua Barbu"      = "Antigua and Barbuda",
                                                                                  "Ascension Isl"      = "United Kingdom",
                                                                                  "Bonaire"            = "Netherlands",
                                                                                  "Sint Maarten"       = "Netherlands",
                                                                                  "Curacao"            = "Netherlands",
                                                                                  "British Virgin Isl" = "British Virgin Islands",
                                                                                  "Bundes Republik"    = "Germany",
                                                                                  "Fed rep Ger"        = "Germany",
                                                                                  "Cent Afr republ"    = "Central African", 
                                                                                  "Dem Rep Congo"      = "The Democratic Republic Of The Congo",
                                                                                  "Eswatini"           = "Swaziland",
                                                                                  "Iran"               = "Islamic Republic of Iran",
                                                                                  "Loas"               = "Lao People's Democratic Republic",
                                                                                  "Macedonia"          = "The Former Yugoslav Republic of Macedonia",
                                                                                  "North Macedonia"    = "The Former Yugoslav Republic of Macedonia",
                                                                                  "Yugoslavia"         = "The Former Yugoslav Republic of Macedonia",
                                                                                  "Rep Congo"          = "Republic of the Congo",
                                                                                  "Sao Tome Prin"      = "Sao Tome and Principe",
                                                                                  "St Barthelemy"      = "France",
                                                                                  "St Martin"          = "France",
                                                                                  "St Helena"          = "United Kingdom",
                                                                                  "Tristan Da Cunh"    = "United Kingdom",
                                                                                  "St Lucia"           = "Saint Lucia",
                                                                                  "St Vincent"         = "Saint Vincent and the Grenadines",
                                                                                  "Svalbard"           = "Svalbard and Jan Mayen",
                                                                                  "Timor Leste"        = "Timor-Leste",
                                                                                  "Turks Caicos"       = "Turks and Caicos Islands",
                                                                                  "Ukssr"              = "Ukraine",
                                                                                  "Vatican"            = "Vatican City State",
                                                                                  "Anguilla"           = "United Kingdom")),
                Country           = stringr::str_extract(Countries.Regions, paste(countries_ls$name_en, collapse = "|"))) |> 
  dplyr::filter(!is.na(Country)) |> 
  dplyr::group_by(Country) |> 
  dplyr::summarise(Record.Count = sum(Record.Count, na.rm = T))


ratio_ORO_totPub <- oceanClimate_byCountry |> 
  dplyr::left_join(ORO_per_country, by = c("Country" = "country_aff")) |> 
  dplyr::mutate(Count_ORO = ifelse(is.na(Count_ORO), 0 , Count_ORO),
                ratio = (Count_ORO/Record.Count)*100)





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




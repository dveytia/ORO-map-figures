#########################################################################################
#                                                                                       #
# FIGURE 5 -  Geographical distribution of ORO research based on country of first author affiliation                                                                               #
# Panel A: Map of # ORO publications/# publications on ocean & climate  in each country #  
# Panel B: Map: temporal trends of the # of publications per country (slope value)      #
# Panel C: Map ratio of mitigation/adaptation ORO publications                          #
# Panel D: Sankey plot of country of 1st auth to country of reserach                                                       #
#                                                                                       #
#########################################################################################

# NB adapted from Figure2_vf.R
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
library(nlme)
library(tibble)

### ----- LOAD FUNCTIONS -----
source(here::here("R", "functions_to_format.R")) # all functions needed to format data
source(here::here("R", "functions_to_plot.R")) # all functions needed to plot data


### ----- CONNECTION TO THE LATEST VERSION OF THE SQL DATABASE -----
sqliteDir <- here::here("data/sqlite-databases")
sqliteFiles <- dir(sqliteDir)
sqliteVersions <- as.numeric(gsub(".sqlite","",substring(sqliteFiles, regexpr("_v", sqliteFiles) + 2)))
latestVersion <- sqliteFiles[which.max(sqliteVersions)]
dbcon <- RSQLite::dbConnect(RSQLite::SQLite(), file.path(sqliteDir, latestVersion), create=FALSE)


### ----- PANEL A (#ORO papers) -----

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

# --- LDC = Least Developed Countries
# --- LLCD = Land Locked Developing Countries
# --- SIDS = Small Island Developing States

# --- Land locked country. Use countrycode::countrycode() to give the same name to countries for all different databasess
landlocked <- read.csv(file = here::here("data", "external", "special_country_groups", "landlocked-countries-2024.csv"), sep = ",") |> 
  dplyr::select(country, LandlockedCountries) |> 
  rename(Country    = country, 
         group_land = LandlockedCountries) |> 
  mutate(iso_code = countrycode(sourcevar   = Country,
                                origin      = "country.name",
                                destination = "iso3c"),
         group_land = case_when(group_land == "yes" ~ "Land-locked",
                                TRUE ~ "NA"))

# --- AMUNRC: Associate Members of United Nations Regional Commissions
AMUNRC <- c("American Samoa", "Anguilla", "Aruba", "Bermuda", "British Virgin Islands", "Cayman Islands", "Commonwealth of Northern Marianas",
            "Curacao", "French Polynesia", "Guadeloupe", "Guam", "Martinique", "Montserrat", "New Caledonia", "Puerto Rico", "Sint Maarten",
            "Turks and Caicos Islands", "United States Virgin Islands")

# --- Category of each country (SIDS, land-locked, coastal)
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

# --- Shape file of countrie's EEZ
eez_shp_path = here::here("data", "external", "eez_shp", "eez_v12.shp")
eez_shp_path = here::here("data", "external","eez_rast","MarineRegions_EEZ_v12_20231025", "eez_v12.shp")

eez_shp <- sf::st_read(eez_shp_path) |>  
  dplyr::select(MRGID, POL_TYPE, TERRITORY1, SOVEREIGN1, ISO_SOV1) |> 
  dplyr::rename(Country = SOVEREIGN1) |> 
  dplyr::mutate(Country = countrycode(sourcevar   = ISO_SOV1,
                                      origin      = "iso3c",
                                      destination = "country.name"),
                iso_code = countrycode(sourcevar   = Country,
                                       origin      = "country.name",
                                       destination = "iso3c")) |> 
  full_join(country_grp |>  dplyr::select(-iso_code), by = c("TERRITORY1" = "Country")) |> 
  mutate(group_land = case_when(Country == "Cape Verde" ~ "Island", TRUE ~ group_land))


## ---- FORMAT DATA 

# --- Subset to relevant rows and get the affiliation
oroAffiliations <- pred_relevance %>%
  filter(0.5 <= relevance_mean) %>%
  left_join(uniquerefs, by = "analysis_id") %>%
  dplyr::select(analysis_id, affiliation, year) %>%
  collect()

# --- Extract the country of the first author for each relevant publications
data_1stA_country <- extract_1stA_affiliation(data         = oroAffiliations, 
                                              countries_ls = countries_ls)

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
  full_join(ORO_per_country |> ungroup() |> dplyr::select(-country_aff), by = "iso_code") |> 
  replace_na(list(Count_ORO = 0))|> 
  mutate(Count_ORO = ifelse(is.na(Count_ORO), 0 , Count_ORO),
         layer     = (Count_ORO/Record.Count)*100) 

# Save for Devi
# save(ratio_ORO_totPub, file = here::here("data", "ratio_ORO_totPub.RData"))

# --- Format the shapefile of the world countries polygon and bind data
world_shp_boundaries <- format_shp_of_the_world(world_shp    = world_shp,
                                                data_to_bind = ratio_ORO_totPub,
                                                PROJ         = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") |> 
  left_join(country_grp |>  dplyr::select(-Country), by = "iso_code") |> 
  dplyr::select(-country.y) |> 
  rename(country = country.x) |> 
  mutate(group_land = case_when(group_land %in% c("Land-locked", "SIDS", "Coastal") ~ group_land,
                                !is.na(group_land) & NA2_DESCRI != country ~ "Island",
                                is.na(group_land)  & NA2_DESCRI != country ~ "Island",
                                is.na(group_land)  & NA2_DESCRI == country ~ "Coastal"))

# --- Format the shapefile of the eez countries polygon and bind data
eez_shp_islands <- full_join(eez_shp, ratio_ORO_totPub, by = "iso_code") |> 
  mutate(country = str_replace_all(country, c("Côte d’Ivoire" = "Ivory Coast",
                                              "Congo - Brazzaville" = "Republic of the Congo",
                                              "Congo - Kinshasa"    = "Democratic Republic of the Congo",
                                              "Somalia"             = "Federal Republic of Somalia"))) |> 
  sf::st_transform(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

# --- Format the data to produce the map
data_2_map_panelA <- try_format_data2map(
  list(
    data = world_shp_boundaries,
    PROJ = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  )
)


## ---- PLOT PANEL A 
source("R/functions_to_plot.R")
panelA <- try_univariate_map(
  list(
    data_map          = data_2_map_panelA,
    # eez               = eez_shp_islands,
    color_scale       = viridis::magma(10, direction = -1),
    midpoint          = NULL,
    second.var        = "Count_ORO",
    svarColour = "white",
    pointSizeRange = c(0,3)*4,
    # vals_colors_scale = NULL,
    title_color       = "#ORO/#O&C (%)",
    title_size        = "#ORO paper",
    show.legend       = TRUE,
    legTitleSize = 20,
    legTextSize = 14,
    name              = "main/map_ORO_O&Cpapers_TEST"
  )
)

## ---- Modelisation (ratio of ORO to ocean and climate by country type) ----
# No significant difference in proportion if SIDS or not
load(here::here("data", "ratio_ORO_totPub.RData"))
ratio_ORO_totPub_modDf <- ratio_ORO_totPub %>%
  mutate(propORO = layer/100) %>%
  left_join(country_grp, by = "iso_code") %>%
  mutate(SIDS = ifelse(group_land == "SIDS", "SIDS","Other")) %>%
  mutate(SIDS = ifelse(is.na(SIDS), "Other", SIDS)) %>%
  mutate(SIDS = factor(SIDS, levels = c("Other","SIDS")))

summary(ratio_ORO_totPub_modDf)

ggplot(ratio_ORO_totPub_glm, aes(x = SIDS, y = propORO))+
  geom_boxplot()

ratio_ORO_totPub_glm <- glm(propORO ~ SIDS, weights = Record.Count, family = "binomial", data = ratio_ORO_totPub_modDf)
summary(ratio_ORO_totPub_glm)

# Instead try by climate zone
# download ascii file from http://koeppen-geiger.vu-wien.ac.at/
# Or maybe current and future https://www.nature.com/articles/sdata2018214
# Used National Aggregates of Geospatial Data Collection: Population, Landscape and Climate Estimaes v2 (1990-2000)
# https://sedac.ciesin.columbia.edu/data/set/nagdc-population-landscape-climate-estimates-v2/data-download

## read in climate zone data
climZoneDf <- readxl::read_excel(here::here("data/external/nagdc-population-landscape-climate-estimates-v2.xls"), sheet = "AREA2000", col_names = TRUE, trim_ws = TRUE)
# Subset to columns of interest
climZoneDf <- climZoneDf[,c(1,grep("PARCZ", colnames(climZoneDf)),186)] %>%
  select(-PARCZ00) %>%
  rename(iso_code = ISO3V10, total_sq_km = T_ARCZ)
# Mutate to long format
climZoneDf <- reshape2::melt(climZoneDf, id.vars = c("iso_code", "total_sq_km"), variable.name = "climate_zone_S", value.name = "perc_land_area")
climZoneDf$prop_land_area = climZoneDf$perc_land_area/100
# Classify columns by larger zone
zoneLookup <- list(
  paste0("PARCZ", 1:6),
  paste0("PARCZ", 7:10),
  paste0("PARCZ", 11:26),
  paste0("PARCZ", 27:37),
  paste0("PARCZ", 38:41),
  paste0("PARCZ", 42:45)
)
names(zoneLookup) <- c("Tropical", "Polar","Temperate","Cold","Dry","Desert")
zoneLookup <- lapply(zoneLookup, function(x) {data.frame(climate_zone_S = x)})
zoneLookup <- mapply(cbind, zoneLookup, "climate_zone_L"=c("Tropical", "Polar","Temperate","Cold","Dry","Desert"),SIMPLIFY = F)
zoneLookup <- do.call(rbind.data.frame, zoneLookup)
climZoneDf <- climZoneDf %>%
  left_join(zoneLookup, by = "climate_zone_S")

## Join climate zone data to ratio data
ratio_ORO_totPub_modDf <- ratio_ORO_totPub %>%
  mutate(propORO = layer/100) %>%
  mutate(logOR = log(propORO/(1-propORO))) %>%
  left_join(country_grp, by = "iso_code") %>%
  mutate(group_land = ifelse(is.na(group_land), "Coastal", group_land)) %>%
  mutate(group_land = ifelse(group_land == "AMUNRC", "Coastal", group_land)) %>% 
  mutate(SIDS = ifelse(group_land == "SIDS", "SIDS","Other")) %>%
  mutate(SIDS_f = factor(SIDS, levels = c("Other","SIDS")),
         group_land_f = factor(group_land, levels = c("Land-locked","Coastal","SIDS"))) %>%
  left_join(climZoneDf, by = "iso_code") %>%
  mutate(climate_tropical = ifelse(climate_zone_L == "Tropical","Tropical","Other")) %>%
  mutate(climate_zone_L_f = factor(climate_zone_L, levels = c("Polar", "Cold","Temperate","Dry","Desert","Tropical")),
         climate_tropical_f = factor(climate_tropical, levels = c("Other","Tropical")))

summary(ratio_ORO_totPub_modDf)


## Fit models

# The likelihood of publishing an ORO as a function of % tropical land area classified as tropical
ratio_ORO_totPub_percTropical_glm <- glm(propORO ~ perc_land_area + group_land_f, weights = Record.Count, family = "binomial", data = ratio_ORO_totPub_modDf, subset = climate_zone_L == "Tropical")
summary(ratio_ORO_totPub_percTropical_glm)
# 1.005154 increase in odds of ORO pub as a function of % tropical land area
# perc_land_area 
# 1.004884
exp(ratio_ORO_totPub_percTropical_glm$coefficients[2]) 
# Odds of publishing ORO in coastal or SIDS is > 5x greater than for land-locked
# group_land_fCoastal    group_land_fSIDS 
# 5.359489            5.341645
exp(ratio_ORO_totPub_percTropical_glm$coefficients[3:4])-exp(ratio_ORO_totPub_percTropical_glm$coefficients[1])


# Plot predictions with raw data
pred.x <- seq(0,100, length.out = 100)
newdat <-  data.frame(
  group_land_f = rep(levels(ratio_ORO_totPub_modDf$group_land_f), length(pred.x)) %>% sort,
  perc_land_area = rep(pred.x, 3))
pred.fit <- predict(ratio_ORO_totPub_percTropical_glm,
                    se.fit = TRUE, 
                    newdata = newdat,
                    type = "link")
pred.fit <- as.data.frame(pred.fit)
pred.fit$group_land_f <- factor(newdat$group_land_f, levels = levels(ratio_ORO_totPub_modDf$group_land_f))
pred.fit$x <- newdat$perc_land_area


pvalSlope <- signif(with(summary(ratio_ORO_totPub_percTropical_glm), coefficients[2,"Pr(>|z|)"]), 2)
pvalSlope <- ifelse(pvalSlope < 0.01, "< 0.01", pvalSlope)
ORSlope <- format(exp(ratio_ORO_totPub_percTropical_glm$coefficients[2]), digits=4)
R2 = signif(with(summary(ratio_ORO_totPub_percTropical_glm), 1 - deviance/null.deviance),2)
ORDat <- data.frame(
  group_land_f = levels(ratio_ORO_totPub_modDf$group_land_f),
  OR = format(exp(ratio_ORO_totPub_percTropical_glm$coefficients[c(1,3:4)]), digits = 1),
  `P value` = signif(with(summary(ratio_ORO_totPub_percTropical_glm), coefficients[c(1,3:4),"Pr(>|z|)"]), 2),
  y = rep(-3.5, 3),
  x = rep(85, 3)
)
ORDat$P.value = ifelse(ORDat$P.value < 0.01, "< 0.01", pvalSlope)
ORDat$group_land_f = factor(ORDat$group_land_f, levels(ratio_ORO_totPub_modDf$group_land_f))

ratio_ORO_totPub_percTropical_glm_ggp <- ggplot() +
  geom_point(data = ratio_ORO_totPub_modDf %>%
               filter(climate_zone_L == "Tropical"),
             aes(x = perc_land_area, y = logOR, size = Record.Count, col = group_land_f))+ 
  geom_line(data = pred.fit, aes(x=x, y=fit))+
  geom_ribbon(data = pred.fit, aes(x=x, ymin=fit-se.fit, ymax = fit+se.fit),
              alpha = 0.5)+
  geom_text(data = ORDat,
            aes(x=x, y=y, label = paste("OR =", OR,"\np =", P.value), col = group_land_f), 
            nudge_y = -0.4,
            size = 3)+
  geom_text(data = data.frame(x=50, y = Inf, OR = ORSlope, P.value = pvalSlope, R2 = R2, group_land_f = factor("Coastal", levels = levels(ratio_ORO_totPub_modDf$group_land_f))),
            aes(x=x, y=y, label = paste("OR % Tropical land area =", OR,
                                        "\np =", P.value,
                                        "\nModel R2 = ", R2)),
            vjust = 1,
            size = 3)+
  labs(y = "log(p/(1-p))", x = "% Tropical land area",
       size = "N articles", col = "Country\ntype")+
  facet_wrap(vars(group_land_f))+
  theme_bw()+
  theme(
    legend.position = "bottom"
  )

ratio_ORO_totPub_percTropical_glm_ggp

ggsave(here::here("figures/supplemental/ratioOROtoOceanClimByCountryTypeAndTropical_binomialGLM.pdf"),
       width = 6, height = 4, units = "in")

## ---- End modelisation


### ----- End Panel A -----


### ----- Panel B (Trends) -----

## ---- LOAD DATA
uniquerefs <- tbl(dbcon, "uniquerefs") # metadata on the unique references
pred_relevance <- tbl(dbcon, "pred_relevance")  # which articles are relevant to OROs

## ---- FORMAT DATA

# -- Number of publication per country and per year
ORO_per_country_year <- data_1stA_country$oroAff_1stA |> 
  dplyr::filter(!is.na(country_aff)) |> 
  dplyr::mutate(country_aff = countrycode(sourcevar   = country_aff,
                                          origin      = "country.name",
                                          destination = "country.name"),
                iso_code = countrycode(sourcevar   = country_aff,
                                       origin      = "country.name",
                                       destination = "iso3c")) |>
  dplyr::filter(!is.na(year)) |> 
  dplyr::group_by(country_aff, iso_code, year) |> 
  dplyr::summarise(Count_ORO = n()) 


## ---- DEVI, YOU CAN PUT THE CODE TO MODEL #OROpub ~ f(year) HERE
## ---- You can use the dataframe called ORO_per_country_year
ORO_per_country_year_trends_df <- ORO_per_country_year %>%
  mutate(year_num = as.numeric(year),
         log_articles = log(Count_ORO)) %>%
  filter(1980 <= year & year <= 2022 & is.finite(log_articles))

# Function to fit simple overall exponential trend with corAR1 autocorrelation
corAR1_fit <- function(dat){
  tryCatch(
    {
      suppressWarnings(gls(log_articles ~ year_num, data = dat, correlation = corAR1()))
    },
    error = function(cond){
      paste("Error message:", message(conditionMessage(cond)), collapse = " ")
    },
    warning = function(cond){
      paste("Warning message:", message(conditionMessage(cond)), collapse = " ")
    },
    finally = {
      message(paste("Processed", dat$country_aff[1]))
    }
  )
}

# Split data by country group to fit models separately
splitDat <- split(ORO_per_country_year_trends_df, 
                  ORO_per_country_year_trends_df$country_aff)

# Fit model to each country
modFits <- lapply(splitDat, corAR1_fit)

# subset to only the model fits that worked
modFits <- modFits[which(unlist(lapply(modFits, class)) == "gls")] 

# Extract the summary statistics for just the slope (year)
summaryTable <- do.call(rbind, lapply(modFits, function(x) summary(x)$tTable['year_num',]))
summaryTable <- summaryTable |> 
  as.data.frame() |> 
  arrange(desc(Value)) |> 
  mutate(Coefficient = "Year") |> 
  # country_aff = names(modFits)) |>
  tibble::rownames_to_column(var = "country") |>
  dplyr::select(country, Coefficient, Value, Std.Error, `t-value`, `p-value`) |> 
  mutate(iso_code = countrycode(sourcevar   = country,
                                origin      = "country.name",
                                destination = "iso3c"))

# Save to outputs
# write.csv(summaryTable, here::here("outputs/glsCountryPublicationTrendsSummaryTable.csv"))

## ---- SUB-ANALYSIS OF TRENDS BY COUNTRY CHARACTERISTICS 
summaryTable <- read.csv(here::here("outputs/glsCountryPublicationTrendsSummaryTable.csv"))

data_Signif <- summaryTable |> 
  filter(p.value <= 0.05) |> 
  mutate(cut   = cut(Value, breaks = 10, dig.lab = 2),
         layer = factor(paste0(">", str_extract(cut, "0\\.\\d+(?=,)"))),
         layer = forcats::fct_relevel(layer, rev(levels(layer)))) |> 
  dplyr::select(-cut)

data_NSignif <- summaryTable |> 
  filter(p.value > 0.05) |> 
  mutate(layer = factor("NS"))

data_trends <- bind_rows(data_Signif, data_NSignif) 

library(conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")


# join with column to indicate country type: land locked, coastal, SIDS
landlocked <- read.csv(file = here::here("data", "external", "special_country_groups", "landlocked-countries-2024.csv"), sep = ",") |> 
  dplyr::select(country, LandlockedCountries) |> 
  rename(Country    = country, 
         group_land = LandlockedCountries) |> 
  mutate(iso_code = countrycode::countrycode(sourcevar   = Country,
                                             origin      = "country.name",
                                             destination = "iso3c"),
         group_land = case_when(group_land == "yes" ~ "Land-locked",
                                TRUE ~ "NA")) 

AMUNRC <- c("American Samoa", "Anguilla", "Aruba", "Bermuda", "British", "Virgin Islands", "Cayman Islands", "Commonwealth of Northern Marianas",
            "Curacao", "French Polynesia", "Guadeloupe", "Guam", "Martinique", "Montserrat", "New Caledonia", "Puerto Rico", "Sint Maarten",
            "Turks and Caicos Islands", "United States Virgin Islands")
SIDS <- read.csv(file = here::here("data", "external", "special_country_groups", "special-country-groups.csv"), sep = ",")
SIDS <- SIDS %>%
  mutate(iso_code = countrycode::countrycode(sourcevar   = Country,
                                             origin      = "country.name",
                                             destination = "iso3c"))

country_grp <-  SIDS |> 
  select(Country, LLDC, SIDS) |> 
  mutate(iso_code = countrycode::countrycode(sourcevar   = Country,
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
  rename(country = Country)

unique(country_grp$group_land)

trendCountryTypeDf <- summaryTable %>%
  #filter(p.value <= 0.05) %>%
  left_join(SIDS[,c("SIDS","iso_code")], by = "iso_code") %>% 
  mutate(SIDS = factor(SIDS, levels = c("Yes","No"))) %>%
  left_join(country_grp[,c("iso_code","group_land")], by = c("iso_code")) %>%
  #mutate(group_land = ifelse(SIDS == "Yes", "SIDS", group_land)) %>%
  mutate(group_land = factor(group_land, levels = c("Land-locked","Coastal","SIDS"))) %>%
  mutate(significant = ifelse(p.value <= 0.05, TRUE, FALSE))

# Number of SIDS is 13, but number of sids in the group_land column is 9...ffor some reason they are NA in the group_land column
summary(trendCountryTypeDf)
trendCountryTypeDf %>%
  filter(SIDS == "Yes" & is.na(group_land)) %>%
  print()

# Other countries are also NA in the group_land classification
View(trendCountryTypeDf[is.na(trendCountryTypeDf$group_land),])

# So actually SIDS have lower and insignificant trend in ORO publication
ggplot(trendCountryTypeDf, aes(x=SIDS, y = Value))+
  geom_boxplot()+
  geom_jitter(aes(col = significant))



## ---- Format map data

# --- Format the shapefile of the world countries polygon and bind data
world_shp_boundaries <- format_shp_of_the_world(world_shp    = world_shp,
                                                data_to_bind = data_trends,
                                                PROJ         = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") |> 
  left_join(country_grp |>  dplyr::select(-country), by = "iso_code") |> 
  dplyr::select(-country.y) |> 
  rename(country = country.x) |> 
  mutate(group_land = case_when(group_land %in% c("Land-locked", "SIDS", "Coastal") ~ group_land,
                                !is.na(group_land) & NA2_DESCRI != country ~ "Island",
                                is.na(group_land)  & NA2_DESCRI != country ~ "Island",
                                is.na(group_land)  & NA2_DESCRI == country ~ "Coastal"))

# --- Format the shapefile of the eez countries polygon and bind data
eez_shp_islands <- full_join(eez_shp, data_trends, by = "iso_code") |> 
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
  filter(group_land == "SIDS" & !is.na(MRGID) & !is.na(layer)) |>
  filter(! TERRITORY1 %in% c("French Guiana", "Greenland")) |> 
  sf::st_transform(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

# --- Format the data to produce the map
data_2_map_panelB <- try_format_data2map(
  list(data = world_shp_boundaries,
       PROJ = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
       )
)

## ---- PLOT PANEL B 
panelB <- try_univariate_map(
  list(
    data_map          = data_2_map_panelB,
    eez               = eez_shp_islands,
    color_scale       = "trends",
    midpoint          = NULL,
    second.var        = NULL,
    # vals_colors_scale = NULL,
    title_color       = "Slope",
    title_size        = NULL,
    show.legend       = TRUE,
    legTitleSize = 20,
    legTextSize = 14,
    name              = "main/map_temporal_trends"
  )
)

### ----- End Panel B -----


### ----- PANEL C (#mit papers/#ada papers) -----

## ---- LOAD DATA
pred_oro_branch <- tbl(dbcon, "pred_oro_branch") # predictions for ORO branch

# --- Cleaned geoparsed data (see .Rmd script called 0_data-processing-cleaning.Rmd)
geoparsed_data_clean <- get(load(here::here("data", "geoparsing", "tmp_clean.RData")))

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
  dplyr::select(analysis_id, adaptation, mitigation) %>%
  left_join(uniquerefs %>% dplyr::select(analysis_id, affiliation), by = "analysis_id") %>%
  collect()

# --- Extract the country of the first author for each relevant publications
data_1stA_country_MA <- extract_1stA_affiliation(data         = mitAdaptPubs, 
                                                 countries_ls = countries_ls) 

tmp <- data_1stA_country_MA$oroAff_1stA |> sample_n(size = 50)
lapply(data_1stA_country_MA, dim)

# --- Select relevant geoparsed data and ratio
geop_ratio_mitig_adapt <-  mitAdaptPubs |>
  inner_join(geoparsed_data_clean, by = "analysis_id", copy = TRUE) |> 
  dplyr::select(analysis_id, country_id, adaptation, mitigation) |> 
  distinct() |> 
  mutate(iso_code = countrycode(sourcevar   = country_id,
                                origin      = "country.name",
                                destination = "iso3c")) |> 
  group_by(country_id, iso_code) |> 
  summarise(adaptation = sum(adaptation, na.rm = TRUE),
            mitigation = sum(mitigation, na.rm = TRUE)) |> 
  mutate(ratio   = mitigation/adaptation,
         mit_ada = mitigation + adaptation,
         layer   = (mitigation/mit_ada)*100) |>  # % mitigation
  rename(Country = country_id)

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
                                                   data_to_bind = geop_ratio_mitig_adapt, # ratio_mitig_adapt
                                                   PROJ         = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") |> 
  # replace_na(list(adaptation = 0, mitigation = 0, mit_ada = 0, ratio = 0)) |> 
  left_join(country_grp |>  dplyr::select(-country), by = "iso_code") |> 
  mutate(group_land = case_when(group_land %in% c("Land-locked", "SIDS", "Coastal") ~ group_land,
                                !is.na(group_land) & NA2_DESCRI != country ~ "Island",
                                is.na(group_land)  & NA2_DESCRI != country ~ "Island",
                                is.na(group_land)  & NA2_DESCRI == country ~ "Coastal")) 

## Note to Gael -- why was greenland removed below? In this plot, greenland has high adaptation 
# a couple other small differences...

# --- Format the shapefile of the eez countries polygon and bind data
eez_shp_islands_MA <- full_join(eez_shp |>  dplyr::select(-Country), geop_ratio_mitig_adapt, by = "iso_code") |> # ratio_mitig_adapt
  # left_join(country_grp |>  select(-Country), by = "iso_code") |> 
  mutate(country = str_replace_all(Country, c("Côte d’Ivoire" = "Ivory Coast",
                                              "Congo - Brazzaville" = "Republic of the Congo",
                                              "Congo - Kinshasa"    = "Democratic Republic of the Congo",
                                              "Somalia"             = "Federal Republic of Somalia")),
         group_land = case_when(group_land %in% c("Island", "Land-locked", "SIDS", "Coastal") ~ group_land,
                                !is.na(group_land) & TERRITORY1 != Country ~ "Island",
                                is.na(group_land)  & TERRITORY1 != Country ~ "Island",
                                is.na(group_land)  & TERRITORY1 == Country ~ "Coastal")) |> 
  # filter(group_land %in% c("Island", "SIDS", "AMUNRC") & !is.na(MRGID)) |>
  filter(group_land == "SIDS" & !is.na(MRGID) & !is.na(layer)) |>
  filter(! TERRITORY1 %in% c("French Guiana", "Greenland")) |> 
  sf::st_transform(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

tmp <- sf::st_drop_geometry(world_shp_boundaries_MA)
tmp2 <- sf::st_drop_geometry(eez_shp_islands_MA)

# --- Format the data to produce the map
data_2_map_panelC <- try_format_data2map(
  list(
    data = world_shp_boundaries_MA,
    PROJ = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  )
)


## ---- PLOT PANEL C
panelC <- try_univariate_map(
  list(
    data_map          = data_2_map_panelC,
    eez               = eez_shp_islands_MA,
    color_scale       = c("#4c4680","white", "#197da8"),
    second.var        = NULL,
    midpoint          = 0.5, 
    title_color       = "Adapt. vs Mit. (%)",
    title_size        = NULL, 
    show.legend       = TRUE,
    legTitleSize = 20,
    legTextSize = 14,
    name              = "main/map_ratio_mit_adap_geoparsing"
  )
)

### ----- End Panel C -----


### ----- Panel D (1st author/geoparsed) Sankey diagram -----

# --- Select relevant geoparsed data
geop_data_all <- geoparsed_data_clean |>
  left_join(pred_relevance, by = "analysis_id", copy = TRUE) |> 
  filter(0.5 <= relevance_mean) |> 
  dplyr::select(analysis_id, shp_id, grid_df_id, TERRITORY1, country_id) |> 
  mutate(country_id = case_when(TERRITORY1 == "Greenland" ~ "Denmark",
                                TRUE ~ country_id)) |> 
  distinct(analysis_id, TERRITORY1, country_id) |> 
  rename(territory1_geop = TERRITORY1, 
         country_geop    = country_id) |> 
  mutate(iso_code_geoP = countrycode(sourcevar   = country_geop,
                                     origin      = "country.name",
                                     destination = "iso3c"))

# -- Find the corresponding continent and sumamrise
ccodes <- raster::ccodes() |>  
  select(NAME, continent, UNREGION1) |> 
  mutate(NAME = case_when(NAME == "Micronesia" ~ "Micronesia (Federated States of)",
                          TRUE ~ NAME)) |> 
  rename(region = UNREGION1) |> 
  mutate(NAME = countrycode(sourcevar   = NAME,
                            origin      = "country.name",
                            destination = "country.name"),
         region = case_when(str_detect(region, pattern = "Asia") == TRUE ~ "Asia",
                            str_detect(region, pattern = "Africa") == TRUE ~ "Africa",
                            str_detect(region, pattern = "Europe") == TRUE ~ "Europe",
                            TRUE ~ region))


# -- Add the continent to the geoparsed data
geop_data_conti <- geop_data_all |> 
  left_join(ccodes, by = c("country_geop" = "NAME")) |> 
  rename(continent_geoP = continent,
         region_geoP    = region) 

# -- Add the continent to the 1st author affiliation data
data_aff_conti <- data_aff |> 
  left_join(ccodes, by = c("country_aff" = "NAME")) |> 
  rename(continent_aff = continent,
         region_aff    = region)  


# -- Bind Data
data_aff_geoP <- left_join(geop_data_conti, data_aff_conti, by = "analysis_id") |> 
  replace_na(list(n_aff = 0,
                  country_aff   = "Unknown",
                  iso_code_aff  = "Unknown",
                  continent_aff = "Unknown",
                  region_aff    = "Unknown")) 


## --- Format data at the region scale
data_aff_geoP_region <- data_aff_geoP |> 
  dplyr::select(analysis_id, region_geoP, region_aff) |> 
  group_by(region_aff, region_geoP) |> 
  summarise(value = n()) |> 
  rename(source = region_aff,
         target = region_geoP) |> 
  mutate(target = paste(target, " ", sep=""))

nodes_region <- data.frame(name=c(as.character(data_aff_geoP_region$source), as.character(data_aff_geoP_region$target)) %>% unique())

data_aff_geoP_region <- data_aff_geoP_region |> 
  mutate(# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
    IDsource = match(source, nodes_region$name)-1,
    IDtarget = match(target, nodes_region$name)-1)



### ---- Plot Sankey Diagramm
library(patchwork)
library(hrbrthemes)
library(circlize)
library(networkD3)
library(webshot)


## --- Region scale
sankey_region <- sankeyNetwork(Links = data_aff_geoP_region, Nodes = nodes_region,
                               Source = "IDsource", Target = "IDtarget",
                               Value = "value", NodeID = "name", 
                               sinksRight = FALSE, nodeWidth = 40, fontSize = 30, nodePadding = 20) ; sankey_region

# saveNetwork(sankey_region, here::here("figures", "main", "sankey_region.html"))
# you convert it as png
webshot(here::here("figures", "main", "sankey_region.html"), 
        here::here("figures", "main", "sankey_region.png"), 
        vwidth = 1000, vheight = 900)



### ----- End Panel D


### ----- Arrange the figure -----

figure5 <- cowplot::ggdraw() +
  cowplot::draw_plot(panelA, x = 0.0, y = 0.66, width = 0.5, height = 0.3) +
  cowplot::draw_plot(panelB, x = 0.0, y = 0.33, width = 0.5, height = 0.3) +
  cowplot::draw_plot(panelC, x = 0.0, y = 0.0, width = 0.5, height = 0.3) +
  cowplot::draw_image(here::here("figures", "main", "sankey_region.png"), 
                      x = 0.5, y = 0, width = 0.5, height = 1) +
  cowplot::draw_plot_label(label = c("(a)", "(b)", "(c)", "(d)"),
                           size = 25,
                           x = c(0, 0.0, 0, 0.5),
                           y = c(0.99, 0.65, 0.34, 0.85)) 




ggplot2::ggsave(plot = figure5, here::here("figures", "main", "maps_1stA_data_sankey.png"), width = 25, height = 18, device = "png", dpi=600)
ggplot2::ggsave(plot = figure5, here::here("figures", "main", "maps_1stA_data_sankey.pdf"), width = 25, height = 18, device = "pdf")





### ----- Supplementary figure comparing 1st auth and geoparsed -----

## ---- LOAD DATA

# --- Cleaned geoparsed data (see .Rmd script called 0_data-processing-cleaning.Rmd)
geoparsed_data_clean <- get(load(here::here("data", "geoparsing", "tmp_clean.RData")))

# --- Shapefile of grid cells with the country & administration they belong to.
# grid_sf <- tbl(dbcon, "grid_df_res2.5") |> collect() |> 
#   sf::st_as_sf(coords = c("LON", "LAT"), crs = 4326)
# 
#   # - Join with land data
#   sf::sf_use_s2(FALSE)
#   grid_sf_land <- sf::st_join(grid_sf, world_shp) |> 
#     dplyr::select(grid_df_id, is_land, NA2_DESCRI, iso_NA2)
#   
#   # - Join with eez data
#   grid_sf_eez <- sf::st_join(grid_sf, eez_shp) |> 
#     dplyr::select(grid_df_id, is_land, MRGID, TERRITORY1, iso_NA2, Country, iso_code) |> 
#     distinct(grid_df_id, .keep_all = TRUE)
#   
#   # - Land + eez data
#   grid_sf_eez_land <- sf::st_join(grid_sf_land, grid_sf_eez |> dplyr::select(-grid_df_id))
#   grid_df_eez_land <- sf::st_drop_geometry(grid_sf_eez_land) |> 
#     mutate(country_id = case_when(is.na(Country) & !is.na(NA2_DESCRI) ~ NA2_DESCRI,
#                                   is.na(NA2_DESCRI) & !is.na(Country) ~ Country,
#                                   is.na(NA2_DESCRI) & is.na(Country)  ~ "High-seas",
#                                   !is.na(NA2_DESCRI) & !is.na(Country) & Country != NA2_DESCRI ~ "Verif",
#                                   !is.na(NA2_DESCRI) & !is.na(Country) & Country == NA2_DESCRI ~ Country),
#            TERRITORY1 = case_when(!is.na(TERRITORY1) ~ TERRITORY1,
#                                   is.na(TERRITORY1) ~ country_id)) |> 
#     dplyr::select(grid_df_id, TERRITORY1, country_id) |> 
#     left_join(grid_df |>  dplyr::select(-area_km, -is_land), by = "grid_df_id", copy = TRUE)
#   
# save(grid_df_eez_land, file = here::here("data", "geoparsing", "land_eez_grid_country.RData"))
load(here::here("data", "geoparsing", "land_eez_grid_country.RData"))


## ---- FORMAT geoparsed data

# --- Select relevant geoparsed data
geop_data_all <- geoparsed_data_clean |>
  left_join(pred_relevance, by = "analysis_id", copy = TRUE) |> 
  filter(0.5 <= relevance_mean) |> 
  dplyr::select(analysis_id, shp_id, grid_df_id, TERRITORY1, country_id) |> 
  mutate(country_id = case_when(TERRITORY1 == "Greenland" ~ "Denmark",
                                TRUE ~ country_id)) |> 
  distinct(analysis_id, TERRITORY1, country_id) |> 
  rename(territory1_geop = TERRITORY1, 
         country_geop    = country_id) |> 
  mutate(iso_code_geoP = countrycode(sourcevar   = country_geop,
                                     origin      = "country.name",
                                     destination = "iso3c"))

## ---- FORMAT 1st author affiliation data

# --- Subset to relevant rows and get the affiliation
oroAffiliations <- pred_relevance |> 
  left_join(uniquerefs, by = "analysis_id") |> 
  dplyr::select(analysis_id, affiliation) |> 
  collect() |> 
  filter(analysis_id %in% unique(geop_data_all$analysis_id)) |> 
  filter(!is.na(affiliation))

# --- Extract the country of the first author for each relevant publications
data_1stA_country <- extract_1stA_affiliation(data         = oroAffiliations, 
                                              countries_ls = countries_ls)

# -- Number of publication per country
data_aff <- data_1stA_country$oroAff_1stA |> 
  dplyr::filter(!is.na(country_aff)) |> 
  dplyr::mutate(country_aff = stringr::str_replace_all(country_aff, c("Guam" = "United States",
                                                                      "New Caledonia" = "France",
                                                                      "Puerto Rico"   = "United States")),
                country_aff = countrycode(sourcevar   = country_aff,
                                          origin      = "country.name",
                                          destination = "country.name"),
                iso_code_aff = countrycode(sourcevar   = country_aff,
                                           origin      = "country.name",
                                           destination = "iso3c")) |> 
  dplyr::select(-affiliation) 

# dplyr::group_by(country_aff, iso_code) 
# dplyr::summarise(n_firstA = n()) 

## ---- FORMAT geoparsed and 1st author data together

# --- Bind data
data_1A_geop <- full_join(geop_data_all, data_aff, by = "analysis_id")

# --- Data a the country scale for geoparsing
data_per_country_geoP <- data_1A_geop |> 
  group_by(country_geop, iso_code_geoP) |> 
  summarise(n_geop = n()) 

# --- Data a the country scale for 1 author affiliation
data_per_country_aff <- data_1A_geop |> 
  group_by(country_aff, iso_code_aff) |> 
  summarise(n_aff = n()) 

# --- Common data at the country scale
ORO_per_country_common_Aff_Geop <- full_join(data_per_country_aff, data_per_country_geoP, by = c("iso_code_aff" = "iso_code_geoP")) |> 
  replace_na(list(n_geop = 0, n_aff = 0)) |> 
  mutate(tot = n_geop + n_aff, 
         layer = (n_aff/tot)) |> 
  # layer = ((n_aff - n_geop)/n_geop)*100) |> 
  ungroup() |> 
  rename(iso_code = iso_code_aff,
         country  = country_aff) |> 
  dplyr::select(-country_geop)

# --- Format data to map
# --- Format the shapefile of the world countries polygon and bind data
world_shp_boundaries <- format_shp_of_the_world(world_shp    = world_shp,
                                                data_to_bind = ORO_per_country_common_Aff_Geop,
                                                PROJ         = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

data_2_map <- try_format_data2map(
  list(
    data = world_shp_boundaries,
    PROJ = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  )
)

# --- Format the shapefile of the eez countries polygon and bind data
eez_shp_SIDS <- eez_shp |>
  filter(group_land == "SIDS") |>
  left_join(ORO_per_country_common_Aff_Geop, by = c("ISO_SOV1" = "iso_code")) |>
  filter(!is.na(MRGID)) |> 
  sf::st_transform(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")


## ---- supplementary figure
sFigGeo1stA <- try_univariate_map(
  list(
    data_map          = data_2_map,
    eez               = eez_shp_SIDS,
    color_scale       = c("#145796","white", "#31850b"),
    midpoint          = 0.5,
    second.var        = NULL,
    # vals_colors_scale = NULL,
    title_color       = "Foreign-led vs Local",
    title_size        = NULL,
    show.legend       = TRUE,
    name              = "supplemental/map_geoP_vs_affiliation"
  )
)

ggsave(sFigGeo1stA,filename= here::here("figures", "supplemental/map_geoP_vs_affiliation.png"), width = 7, height = 4.5, device = "png",dpi = 600)


### -----



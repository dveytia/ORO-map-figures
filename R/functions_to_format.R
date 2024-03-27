#' Extract The Affiliation Of The First Author Of Each Paper
#'
#' @param data a dataframe with at least two column called analysis_id and affiliation
#' @param countries_ls a dataframe with all existing countries (load sql-pays.csv file)
#'
#' @return a list of 3 dataframes. 
#' 1st DF = data + new column for the country of the 1st Author
#' 2nd DF = data with an affiliation for which no countries has been identified
#' 3rd DF = data without an affiliation
#'
#'
extract_1stA_affiliation <- function(data, countries_ls){
  
  ### 1st extraction
  ### Carrefull modification of coutries name in the affiliation 
  ### column to match with countries_ls names
  aff_1stA <- data |> 
    filter(!is.na(affiliation)) |> 
    mutate(affiliation = stringr::str_replace_all(affiliation, c("Georgia"                     = "GeorgiA", # To differentiate it from University of Georgia
                                                                 "England\\."                  = "United Kingdom.",
                                                                 "Papua N Guinea"              = "Papua New Guinea",
                                                                 "\\, ENGLAND"                 = ", United Kingdom",
                                                                 "Engl\\,"                     = "United Kingdom,", # cf ref 378521
                                                                 "Univ\\ New\\ S\\ Wales" = "Univ.New.S.Wales", # to avoid transforming it to UK since it is an Australian univ.
                                                                 "Wales\\."                       = "United Kingdom", 
                                                                 "New South United Kingdom"    = "New South Wales", # ref 259903
                                                                 "South United Kingdom"        = "South Wales", # ref 327284
                                                                 "WALES\\."                       = "United Kingdom",
                                                                 "Scotland\\."                 = "United Kingdom", # \\ to avoid replacing in ref 353573 and other
                                                                 # "Irel,"                       = "United Kingdom.",
                                                                 # "\\, North Ireland\\."        = ", United Kingdom.",
                                                                 # "\\,NORTH IRELAND\\."         = ", United Kingdom.",
                                                                 # "\\, Ireland\\."              = ", United Kingdom.",
                                                                 # "North Irel"                  = "United Kingdom",
                                                                 "Irel,"                       = "Ireland.",
                                                                 "\\, North Ireland\\."        = ", Ireland.",
                                                                 "\\,NORTH IRELAND\\."         = ", Ireland.",
                                                                 "\\, Ireland\\."              = ", Ireland.",
                                                                 "North Irel"                  = "Ireland",
                                                                 "\\(British\\)"               = "United Kingdom",
                                                                 "Great Britain"               = "United Kingdom",
                                                                 "\\, Scotl$"                  = ", United Kingdom",
                                                                 "\\,USA\\."                   = "United States",
                                                                 "\\, USA\\,"                  = "United States",
                                                                 "\\ USA\\."                   = ", United States",
                                                                 "\\, U.S.A"                   = ", United States",
                                                                 "Iowa U.S.A"                  = "Iowa, United States",
                                                                 "Russia\\."                   = "Russian Federation",
                                                                 "RUSSIA\\."                   = "Russian Federation",
                                                                 "\\, USSR\\,"                 = ", Russian Federation,",
                                                                 "\\, Iran"                    = ", Islamic Republic of Iran",
                                                                 "\\,ITALY."                   = ", Italy",
                                                                 "\\,NORWAY."                  = ", Norway",
                                                                 "Tanzania"                    = "United Republic Of Tanzania",
                                                                 "\\, North Macedonia"         = ", The Former Yugoslav Republic of Macedonia",
                                                                 "\\, AUSTRALIA\\."            = ", Australia",
                                                                 "Macau"                       = "Macao",
                                                                 "Macaolay"                    = "Macaulay", # ref 203560
                                                                 "\\, Can\\,"                  = ", Canada,",
                                                                 "\\, CANADA"                  = ", Canada",
                                                                 "\\,CANADA"                   = ", Canada",
                                                                 "\\, Viet Nam"                = ", Vietnam",
                                                                 "\\,VIETNAM\\."               = ", Vietnam",
                                                                 "\\,JAPAN\\."                 = ", Japan",
                                                                 "\\, Jpn"                     = ", Japan",
                                                                 "\\, PHILIPPINES\\."          = ", Philippines",
                                                                 "\\,PHILIPPINES\\."           = ", Philippines",
                                                                 "U Arab Emirates"             = "United Arab Emirates",
                                                                 "\\,BELGIUM\\."               = ", Belgium.",
                                                                 "\\, SWITZERLAND\\."          = ", Switzerland.",
                                                                 "\\,SWITZERLAND\\."           = ", Switzerland.",
                                                                 "\\, NORWAY\\."               = ", Norway",
                                                                 "\\,NORWAY\\."                = ", Norway",
                                                                 "Trinidad Tobago"             = "Trinidad and Tobago",
                                                                 "\\,INDIA\\."                 = ", India.",
                                                                 "\\,SOUTH AFRICA\\."          = ", South Africa.",
                                                                 "\\,CYPRUS\\."                = ", Cyprus.",
                                                                 "\\,FRANCE\\."                = ", France.",
                                                                 "\\,SPAIN\\."                 = ", Spain.",
                                                                 "\\,ECUADOR\\."               = ", Ecuador.",
                                                                 "\\, FRANCE\\."               = ", France.",
                                                                 "\\,SWEDEN\\."                = ", Sweden.",
                                                                 "\\,GERMANY\\."               = ", Germany.",
                                                                 "French Guiana"               = "France",
                                                                 "West Ger"                    = "Germany",
                                                                 "\\, NETHERLANDS\\."          = ", Netherlands.",
                                                                 "\\,NETHERLANDS\\."           = ", Netherlands.",
                                                                 "\\,UKRAINE\\."               = ", Ukraine.",
                                                                 "\\,ISRAEL\\."                = ", Israel.",
                                                                 "\\,EGYPT\\."                 = ", Egypt.",
                                                                 "\\,POLAND\\."                = ", Poland.",
                                                                 "\\,FINLAND\\."               = ", Finland.",
                                                                 "\\,SRI LANKA\\."             = ", Sri Lanka.",
                                                                 "\\,HONG KONG\\."             = ", Hong Kong.",
                                                                 "Korea (the Republic of)"     = "Republic of Korea",
                                                                 "\\,DENMARK\\."               = ", Denmark.",
                                                                 "\\, DENMARK\\."              = ", Denmark.",
                                                                 "\\,INDONESIA\\."             = ", Indonesia.",
                                                                 "\\, PANAMA."                 = ", Panama",
                                                                 "\\,GAMBIA\\."                = ", Gambia.",
                                                                 "\\, SINGAPORE"               = ", Singapore.",
                                                                 "\\,GREECE\\."                = ", Greece.",
                                                                 "\\,ENGLAND\\."               = ", United Kingdom.",
                                                                 "\\, ENGLAND\\."              = ", United Kingdom.",
                                                                 "\\,SCOTLAND\\."              = ", United Kingdom.",
                                                                 "\\,MEXICO\\."                = ", Mexico.",
                                                                 "\\,HUNGARY\\."               = ", Hungary.",
                                                                 "\\,AUSTRALIA\\."             = ", Australia.",
                                                                 "\\, JAPAN\\."                = ", Japan.",
                                                                 "\\, S Afr"                   = ", South Africa",
                                                                 "\\,MALAYSIA\\."              = ", Malaysia.",
                                                                 "Syria"                       = "Syrian Arab Republic",
                                                                 "Marshall Island"             = "Marshall Islands",
                                                                 "\\, Micronesia."             = "Federated States of Micronesia",
                                                                 "\\, Moldova."                = "Republic of Moldova",
                                                                 "\\, Peoples R China."        = "China",
                                                                 "Korea \\(the Republic of\\)" = "Republic of Korea"))) |> 
    dplyr::mutate(country_aff = stringr::str_extract(affiliation, paste(countries_ls$name_en, collapse = "|")))
  
  
  ### 2nd extraction 
  ### Carrefull modification of coutries name in the affiliation 
  ### column to match with countries_ls names
  aff_1stA_2 <- dplyr::filter(aff_1stA, is.na(country_aff)) |> 
    dplyr::mutate(affiliation = stringr::str_replace_all(affiliation, c("UK"              = "United Kingdom",
                                                                        "\\, U.K"         = "United Kingdom",
                                                                        "\\, Norw"        = "Norway",
                                                                        "\\, Can"         = "Canada,",
                                                                        "\\, Finl"        = ", Finland",
                                                                        "\\, Belg\\,"     = ", Belgium",
                                                                        "\\, Switz\\,"    = ", Switzerland.",
                                                                        "Fr"              = "France",
                                                                        "Swed"            = "Sweden",
                                                                        "Isr"             = "Israel",
                                                                        "Port"            = "Portugal",
                                                                        "\\, Hong Kong"   = ", China",
                                                                        "\\, HONG KONG"   = ", China")))
  
  ### Data with NA (e.g., many large companies such as Total, Petronas, Aramco)
  NA_country <- aff_1stA_2 |>
    dplyr::mutate(country_aff = stringr::str_extract(affiliation, paste(countries_ls$name_en, collapse = "|"))) |> 
    dplyr::filter(is.na(country_aff)) |> 
    dplyr::group_by(affiliation) |> 
    dplyr::summarise(Count_ORO = n())
  
  ### 2nd extraction and binding with data of the 1st extraction
  oroAffiliations_country1stA <- aff_1stA_2 |> 
    dplyr::mutate(country_aff = stringr::str_extract(affiliation, paste(countries_ls$name_en, collapse = "|"))) |> 
    dplyr::filter(!is.na(country_aff)) |> 
    rbind(aff_1stA) |> 
    dplyr::filter(!is.na(country_aff))
  
  ### Papers without affiliation
  NA_affiliation <- filter(data, is.na(affiliation))
  
  
  ### CHECKS
  
    ## Check that there are no duplicate rows
    if(sum(duplicated(oroAffiliations_country1stA)==TRUE) != 0){
      stop(paste0("Duplicated cells identifies in the oroAffiliation_country1stA data.frame"))
    } 
  
    ## Check that the number of lines in the three outputs files = nrow(data)
    if((nrow(NA_affiliation) + sum(NA_country$Count_ORO, na.rm = T) + nrow(oroAffiliations_country1stA)) != nrow(data)){
      stop(paste0("The number of lines differs between the input data & the three output dataframes"))
    }
  
  
  ### Store all data
  all_data <- list(oroAff_1stA    = oroAffiliations_country1stA,
                   NA_country     = NA_country,
                   NA_affiliation = NA_affiliation)
  
  return(all_data)
  
}


#' Format Data Of The Number Of Publications On O&A By Country As Found On WOS
#'
#' @param data load the file WOS_ocean-and-climate_by-country_2023-11-21.txt
#' @param countries_ls load the file sql-pays.csv
#'
#' @return
#' @export
#'
#' @examples
number_OandC_paper_formating <- function(data, countries_ls){
  
  output_data <- data |> 
    dplyr::mutate(Countries.Regions = stringr::str_to_title(Countries.Regions)) |> 
    dplyr::mutate(Countries.Regions = stringr::str_replace_all(Countries.Regions, c("England"            = "United Kingdom",
                                                                                    "Scotland"           = "United Kingdom",
                                                                                    "Falkland Island"    = "United Kingdom",
                                                                                    "North Ireland"      = "United Kingdom",
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
                                                                                    "Anguilla"           = "United Kingdom",
                                                                                    "Hong Kong"          = "China",
                                                                                    "South Sudan"        = "Sudan",
                                                                                    "Papua N Guinea"     = "Papua New Guinea",
                                                                                    "Fed Rep Ger"        = "Germany",
                                                                                    "Micronesia"         = "Micronesia (Federated States of)",
                                                                                    "Cent Afr Republ"    = "Central African Republic")),
                  country = countrycode(sourcevar   = Countries.Regions,
                                        origin      = "country.name",
                                        destination = "country.name"),
                  iso_code = countrycode(sourcevar   = country,
                                         origin      = "country.name",
                                         destination = "iso3c")) |> 
    dplyr::filter(!is.na(country) & !is.na(iso_code)) |> 
    dplyr::group_by(country, iso_code) |> 
    dplyr::summarise(Record.Count = sum(Record.Count, na.rm = T))
  
  return(output_data)
  
}


#' Format The Shape File Of World's Country Boundaries And Bind Data
#'
#' @param world_shp load "world_shp"
#' @param data_to_bind data that you want to map
#' @param PROJ the wanted projection
#'
#' @return
#' @export
#'
#' @examples
format_shp_of_the_world <- function(world_shp, data_to_bind, PROJ){
  
  world_bounds <- world_shp |> 
    dplyr::mutate(country  = countrycode(sourcevar   = NA2_DESCRI,
                                         origin      = "country.name",
                                         destination = "country.name"),
                  country  = case_when(is.na(country) == FALSE ~ country,
                                       is.na(country) == TRUE & NA2_DESCRI == "Ashmore and Cartier Islands" ~ "Australia",
                                       is.na(country) == TRUE & NA2_DESCRI == "Navassa Island" ~ "United States",
                                       is.na(country) == TRUE & NA2_DESCRI == "Coral Sea Islands" ~ "Australia",
                                       is.na(country) == TRUE & NA2_DESCRI == "Jarvis Island" ~ "United States",
                                       is.na(country) == TRUE & NA2_DESCRI == "Europa Island" ~ "France",
                                       is.na(country) == TRUE & NA2_DESCRI == "Baker Island" ~ "United States",
                                       is.na(country) == TRUE & NA2_DESCRI == "Glorioso Islands" ~ "France",
                                       is.na(country) == TRUE & NA2_DESCRI == "Howland Island" ~ "United States",
                                       is.na(country) == TRUE & NA2_DESCRI == "Clipperton Island" ~ "France",
                                       is.na(country) == TRUE & NA2_DESCRI == "Jan Mayen" ~ "Norway",
                                       is.na(country) == TRUE & NA2_DESCRI == "Johnston Atoll" ~ "United States",
                                       is.na(country) == TRUE & NA2_DESCRI == "Juan De Nova Island" ~ "United States",
                                       is.na(country) == TRUE & NA2_DESCRI == "Kingman Reef" ~ "United States",
                                       is.na(country) == TRUE & NA2_DESCRI == "Palmyra Atoll" ~ "United States",
                                       is.na(country) == TRUE & NA2_DESCRI == "Midway Islands" ~ "United States",
                                       is.na(country) == TRUE & NA2_DESCRI == "Paracel Islands" ~ "China",
                                       is.na(country) == TRUE & NA2_DESCRI == "Tromelin Island" ~ "United States",
                                       is.na(country) == TRUE & NA2_DESCRI == "Virgin Islands" ~ "United States",
                                       is.na(country) == TRUE & NA2_DESCRI == "Wake Island" ~ "United States"),
                  iso_code = countrycode(sourcevar   = country,
                                         origin      = "country.name",
                                         destination = "iso3c"),
                  admin_iso = paste0(NA2_DESCRI, ", ", iso_code)) |> 
    dplyr::select(NA2_DESCRI,iso_code,country, admin_iso) |> 
    # dplyr::select(NA2_DESCRI, NA3_DESCRI, geometry) |> 
    # dplyr::mutate(NA2_DESCRI = stringr::str_replace_all(NA2_DESCRI, c("The Bahamas"                        = "Bahamas",
    #                                                                   "Iran"                               = "Islamic Republic of Iran",
    #                                                                   "Congo (Democratic Republic of the)" = "The Democratic Republic Of The Congo",
    #                                                                   "Congo"                              = "Republic of the Congo",
    #                                                                   "Svalbard"                           = "Svalbard and Jan Mayen",
    #                                                                   "Vatican City"                       = "Vatican City State",
    #                                                                   "Brunei"                             = "Brunei Darussalam",
    #                                                                   "Burma"                              = "Myanmar",
    #                                                                   "Russia"                             = "Russian Federation",
    #                                                                   "Syria"                              = "Syrian Arab Republic",
    #                                                                   "Turks and Caicas Islands"           = "Turks and Caicos Islands",
    #                                                                   "The Gambia"                         = "Gambia",
    #                                                                   "Tanzania"                           = "United Republic Of Tanzania"))) |> 
    
    # dplyr::full_join(data_to_bind, by = c("NA2_DESCRI" = "Country")) |>
    dplyr::full_join(data_to_bind, by = "iso_code") |>
    sf::st_transform(crs = PROJ)
  
}


#' Format Data To Map 
#'
#' @param data an sf object with the data to map
#' @param PROJ the PROJECTION to which data is formatted
#'
#'
format_data2map <- function(data, PROJ){
  
  ### Load graticules and other stuffs
  load(here::here("data", "data_map.RData"))
  # load(here::here("data", "geo_data_1.RData"))
  
  
  ### Modify projection
  NE_box_2 <- sf::st_sfc(sf::st_polygon(list(cbind(c(rep(180,1801), rep(-180,1801), 180), 
                                                   c(rev(seq(-90, 90, by = 0.1)), seq(-90, 90, by = 0.1), 90)))),
                         crs = sf::st_crs(geo_data))
  
  grid           <- sf::st_transform(geo_data, PROJ)
  borders        <- sf::st_transform(geo_borders, PROJ)
  box_rob        <- sf::st_transform(NE_box_2, PROJ)
  NE_graticules  <- sf::st_as_sf(NE_graticules)
  graticules_rob <- sf::st_transform(NE_graticules, PROJ)
  
  
  ## project long-lat coordinates for graticule label data frames (two extra columns with projected XY are created)
  prj.coord <- rgdal::project(cbind(lbl.Y$lon, lbl.Y$lat), proj = PROJ)
  lbl.Y.prj <- cbind(prj.coord, lbl.Y)
  names(lbl.Y.prj)[1:2] <- c("X.prj", "Y.prj")
  
  ## position label 
  lbl.Y.prj$X.prj  <- (-(lbl.Y.prj$X.prj))
  lbl.Y.prj$X.prj2 <- lbl.Y.prj$X.prj#-1.10e6
  
  ## X
  prj.coord <- rgdal::project(cbind(lbl.X$lon, lbl.X$lat), proj = PROJ)
  lbl.X.prj <- cbind(prj.coord, lbl.X)
  names(lbl.X.prj)[1:2] <- c("X.prj", "Y.prj")
  lbl.X.prj <- subset(lbl.X.prj, Y.prj < 0)
  
  
  ### Format all data in list
  data_map <- list("data"       = data, 
                   "borders"    = borders, 
                   "graticules" = graticules_rob,
                   "box"        = box_rob,
                   "lat_text"   = lbl.Y.prj,
                   "lon_text"   = lbl.X.prj)
  
  return(data_map)
  
}


#' Bivariate Color Scale 
#'
#' @param nquantiles the number of quantile to split the data
#' @param upperleft the color in the top left corner
#' @param upperright the color in the top right corner
#' @param bottomleft the color in the bottom left corner
#' @param bottomright the color in the bottom right corner
#' @param xlab x axis label for the plot
#' @param ylab y axis label for the plot
#'
#' @return
#' @export
#'
#' @examples
color_bivariate_map <- function(nquantiles, upperleft, upperright, bottomleft, bottomright, xlab, ylab){
  
  ### Create classes
  my.data <- seq(0, 1, .01)
  my.class <- classInt::classIntervals(my.data, n = nquantiles, style = "quantile") 
  
  ### Extract classes from my.class and assign them a color
  my.pal.1 <- classInt::findColours(my.class, c(upperleft,bottomleft)) 
  my.pal.2 <- classInt::findColours(my.class, c(upperright, bottomright)) 
  
  ### Create a matrix of colors
  col.matrix <- matrix(nrow = 101, ncol = 101, NA)

    ## For loop to assign a color to each element of the matrix
    for(i in 1:101){
      my.col <- c(paste(my.pal.1[i]), paste(my.pal.2[i]))
      col.matrix[102-i,] <- classInt::findColours(my.class, my.col) # Assign a different color betwwen color extract in my.col for each element of the row
    }
  

    plot(c(1,1), pch = 19, col = my.pal.1, cex = 0.5, xlim = c(0,1), ylim = c(0,1), frame.plot = F, xlab = xlab, ylab = ylab, cex.lab = 1.3) # pch = shape of the point
  
  ### For loop to plot squares
  for(i in 1:101){
    col.temp <- col.matrix[i-1,]
    points(my.data, rep((i-1)/100, 101), pch = 15, col = col.temp, cex = 1) # Drawing a sequence of point at the specified coordinates.
  }
  
    
  ### Get the quantiles values
  seqs <- seq(0, 100, (100/nquantiles)) 
  seqs[1] <- 1
  col.matrix <- col.matrix[c(seqs), c(seqs)] # Matrix of color with with 10 rows and 10 columns
  
  
  ### Turn it into a table that would match with data
  ### To obtain this for all quantiles values:
  # as.character(c(seq(1.10, 10.1, 1), seq(1.2, 10.2, 1), seq(1.3, 10.3, 1), seq(1.4, 10.4, 1), seq(1.5, 10.5, 1), seq(1.6, 10.6, 1),
  #                seq(1.70, 10.7, 1), seq(1.8, 10.8, 1), seq(1.9, 10.9, 1),
  #                "1.10", "2.10", "3.10", "4.10", "5.10", "6.10", "7.10", "8.10", "9.10", "10.10"))
  q1 <- 1
  qmax <- nquantiles
  
  start <- paste0(q1:qmax, ".", q1) 
  middle <- unlist(lapply(2:(nquantiles-1), function(i) {
    qstart <- 1
    qend <- i
    qmax <- nquantiles
    vals <- paste0("seq(", qstart, ".", qend, ",", qmax, ".", qend, ",", 1, ")")
    return(as.character(eval(parse(text = vals))))
  }))
  end <- paste0(q1:qmax, ".", qmax)
  all_vals <- c(start, middle, end)
  

  tbl_color <- dplyr::as_tibble(col.matrix[2:(nquantiles+1), 2:(nquantiles+1)])  |>
    tidyr::pivot_longer(cols = 1:nquantiles, names_to = "group", values_to = "fill", cols_vary = "slowest") |>
    dplyr::mutate(group = all_vals) |>
    as.data.frame() 
    
  # tbl_color <- dplyr::as_tibble(col.matrix[2:(nquantiles+1), 2:(nquantiles+1)])  |>
  #   tidyr::gather(group, fill, V1, V2, V3, V4, V5, V6, V7, V8, V9 ,V10) |>
  #   dplyr::mutate(group = as.character(c(seq(1.10, 10.1, 1), seq(1.2, 10.2, 1), seq(1.3, 10.3, 1), seq(1.4, 10.4, 1), seq(1.5, 10.5, 1), seq(1.6, 10.6, 1),
  #                                        seq(1.70, 10.7, 1), seq(1.8, 10.8, 1), seq(1.9, 10.9, 1),
  #                                        "1.10", "2.10", "3.10", "4.10", "5.10", "6.10", "7.10", "8.10", "9.10", "10.10"))) |>
  #   as.data.frame(.) |>
  #   dplyr::rename()
  
  return(tbl_color)
  
}

#' Format Data To Create A Bivariate Map
#'
#' @param data_x a dataframe with the data column named "x"
#' @param data_y a dataframe with the data column named "y"
#' @param color_table the color data table, use CarcasSink::color_bivariate_map 
#' @param grid a sf object with cell id, their longitude and latitude and the column geometry
#' @param PROJ a character representing the projection  
#'
#' @return
#' @export
#'
#' @examples
format_data_bivariate_map <- function(data, data.x, data.y, color_table, nquantiles, probs.quant.y = seq(0,1,0.1), probs.quant.x = seq(0,1,0.1)){
  
  ### Calculate quantiles
  
    ## For data x
    # x_quantile <- quantile(data[, data.x], probs = probs.quant.x, na.rm = TRUE)
    # x_tile = ntile(x = data[, data.x], 10)
    # y_tile = ntile(x = data[, data.y], 10)
    ## For data y
    # y_quantile <- quantile(data[, data.y], probs = probs.quant.y, na.rm = TRUE)
  

  ## Cut data into groups
  data_xy <- data |>
    mutate(group_x = ntile(get(data.x), nquantiles),
           group_y = ntile(get(data.y), nquantiles),
           group   = paste0(group_x, ".", group_y)) |>
    left_join(color_table, by = "group")
    
  # data_xy <- data |>
  #   dplyr::mutate(x_quantile = cut(data |> pull(get(data.x)), breaks = unique(x_quantile), include.lowest = TRUE),
  #                 y_quantile = cut(data |> pull(get(data.y)), breaks = unique(y_quantile), include.lowest = TRUE),
  #                 group      = ifelse(!is.na(y_quantile) & !is.na(x_quantile), paste0(as.numeric(x_quantile), ".", as.numeric(y_quantile)), NA)) |>
  #   dplyr::left_join(color_table, by = "group")
  
  return(data_xy)
  
}


#' Normalize Raster Values Between 0 and 1
#'
#' @param r the raster to modify
#'
#' @return a raster with values normalized between 0 and 1
#' @export
#'
#' @examples
normalize_rast_values_01 <- function(r){
  
  # get the min max values
  minmax_r = range(raster::values(r), na.rm=TRUE) 
  
  # rescale 
  rast_i <- (r-minmax_r[1]) / (diff(minmax_r))
  
  # Transform NA into 0
  r <- raster::reclassify(rast_i, cbind(NA, 0))
  return(r)
  
}



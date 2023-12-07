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

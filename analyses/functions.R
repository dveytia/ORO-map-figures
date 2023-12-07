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


univariate_map <- function(data_map, color_scale, legend, show.legend, name = NULL){
  
  ### Produce the map
  map <- ggplot2::ggplot() +
    
    ## DBEM output grid
    ggplot2::geom_sf(data    = data_map$data,
                     mapping = ggplot2::aes(fill     = layer,
                                            geometry = geometry),
                     color   = NA,
                     size    = 0.01,
                     show.legend = show.legend) +
    
    # Add graticules
    # ggplot2::geom_sf(data     = data_map$graticules,
    #                  linetype = "dotted",
    #                  color    = "grey70",
    #                  size     = 0.4) +
    
    ## Add borders grid
    # ggplot2::geom_sf(data   = data_map$borders, 
    #                  colour = NA,  
    #                  fill   = "gray70") +
    
    ggplot2::geom_sf(data   = data_map$box, 
                     colour = "black", 
                     fill   = NA, 
                     size   = 0.1) +
    
    ggplot2::scale_fill_gradientn(colors   = color_scale,
                                  na.value = "grey50") +
    
    ggplot2::theme_void() +
    
    
    
    ## Add latitude and longitude labels
    ggplot2::geom_text(data = data_map$lat_text, mapping = ggplot2::aes(x = X.prj2-1*10e5, y = Y.prj,          label = lbl), color = "grey20", size = 1.5) +
    ggplot2::geom_text(data = data_map$lon_text, mapping = ggplot2::aes(x = X.prj,         y = Y.prj-0.5*10e5, label = lbl), color = "black",  size = 1.5) + 
    
    ggplot2::labs(fill = legend) +
    
    ggplot2::guides(size = "none", fill = ggplot2::guide_colourbar(title.position = "right", barwidth = 0.7)) +
    
    ## Theme
    ggplot2::theme(panel.grid.major.x = ggplot2::element_line(color = NA),
                   panel.background   = ggplot2::element_blank(),
                   axis.text          = ggplot2::element_blank(),
                   axis.ticks         = ggplot2::element_blank(), 
                   axis.title         = ggplot2::element_blank(),
                   plot.margin        = ggplot2::unit(c(0,0,0,0), "cm"),
                   plot.title         = ggplot2::element_text(size  = 12, 
                                                              face  = "bold", 
                                                              hjust = 0.5, 
                                                              vjust = -0.5),
                   legend.title       = ggplot2::element_text(size  = 12, 
                                                              face  = "bold", 
                                                              hjust = 0.5, 
                                                              vjust = 0.5, angle = 90),
                   legend.title.align = 0.5, 
                   legend.direction   = "vertical",
                   legend.text        = ggplot2::element_text(size = 12))
  
  
  ### Save map
  if(! is.null(name)) {
    
    save(map, file = here::here("results", paste0(name, ".RData")))
    ggplot2::ggsave(here::here("figures", paste0(name, ".png")), width = 7, height = 4.5, device = "png")
    
  }
  
  return(map)
  
  
}

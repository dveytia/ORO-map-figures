#' Univariate Map
#'
#' @param data_map a list of objects obtained using the function format_data2map()
#' @param color_scale the color scale to use e.g., viridis::viridis()for colorblind friendly colorscales
#' @param legend a caracter vector corresponding to the title of the color scale
#' @param show.legend TRUE/FALSE to show legend in the final plot 
#' @param name default NULL. If not null, a caracter vector corresponding to the name under which the figure is to be saved.
#'
#'
#' @examples
univariate_map <- function(data_map, color_scale, legend, show.legend, name = NULL){
  
  ### Produce the map
  map <- ggplot2::ggplot() +
    
    ## DBEM output grid
    ggplot2::geom_sf(data    = data_map$data,
                     mapping = ggplot2::aes(fill     = layer,
                                            geometry = geometry),
                     color   = "grey10",
                     size    = 0.1,
                     show.legend = show.legend) +
    
    ggplot2::scale_fill_gradientn(colors   = color_scale,
                                  na.value = "grey50") +
    
    # Add graticules
    # ggplot2::geom_sf(data     = data_map$graticules,
    #                  linetype = "dotted",
    #                  color    = "grey70",
    #                  size     = 0.4) +
    
    ## Add borders grid
    # ggplot2::geom_sf(data   = data_map$borders,
    #                  colour = "grey10",
    #                  fill   = "transparent",
    #                  size   = 0.1) +
  
    ggplot2::geom_sf(data   = data_map$box, 
                     colour = "black", 
                     fill   = NA, 
                     size   = 0.1) +
      
    
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
    
    ggplot2::ggsave(here::here("figures", paste0(name, ".jpeg")), width = 7, height = 4.5, device = "jpeg")
    
  }
  
  return(map)
  
  
}

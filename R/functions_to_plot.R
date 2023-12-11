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



#' Biplot For Figure 2 Panel C
#'
#' @param data the data
#' @param xlab a character vector of the name of the x axis
#' @param ylab a character vector of the name of the y axis
#' @param log.transf TRUE/FALSE if log transformation is needed
#' @param quant.prob numeric between 0 and 1 (i.e., 0.8 generate labels for points in the top 20% highest values of all residuals)
#' @param name default NULL. If not null, a caracter vector corresponding to the name under which the figure is to be saved.
#' @param color_scale the color scale (3 colors)
#'
#' @return
#' @export
#'
#' @examples
biplot_fig2c <- function(data, xlab, ylab, color_scale, log.transf, quant.prob, name = NULL){
  
  data <- data |> filter(!is.na(dominant_ORO) & !is.na(Count_ORO) & !is.na(Record.Count))
    
  ### Log transformation if wanted
  if(log.transf == TRUE){
    data <- data |> 
      mutate(Count_ORO    = log(Count_ORO+1),
             Record.Count = log(Record.Count+1))
  }
  
  ### Residuals
  data <- data |> 
    mutate(residuals = resid(lm(Count_ORO ~ Record.Count, data = cur_data())),
           labels    = abs(residuals) >= quantile(abs(residuals), prob = quant.prob))
  

  
  plot <- ggplot(data    = data, 
                 mapping = aes(x = Record.Count, 
                               y = Count_ORO)) +
    geom_point(mapping = aes(color = dominant_ORO)) +
    geom_smooth(method  = lm, 
                col     = "grey10") +
    
    ylim(c(0, max(data$Count_ORO))) +

    xlab(label = xlab) +
    ylab(label = ylab) +
    geom_text_repel(data        = filter(data, labels == TRUE), 
                    mapping     = aes(label = Country, color = dominant_ORO), 
                    show.legend = FALSE,
                    min.segment.length = 0.1) +
    scale_color_manual(values = color_scale, 
                       name   = "ORO branch:",
                       labels = c("Adaptation", "50/50", "Mitigation")) +
    theme_bw() +
    theme(legend.position = c(0.15, 0.85),
          axis.text.x     = element_text(size = 11),
          axis.text.y     = element_text(size = 11),
          axis.title.x    = element_text(size = 13),
          axis.title.y    = element_text(size = 13),
          legend.text     = element_text(size = 12),
          legend.title    = element_text(size = 13))
  
  if(! is.null(name)) {
    
    ggplot2::ggsave(here::here("figures", paste0(name, ".jpeg")), width = 7, height = 5, device = "jpeg")
    
  }
  
  return(plot)
  
}
  
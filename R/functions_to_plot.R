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
univariate_map <- function(data_map, color_scale, vals_colors_scale, legend, show.legend, name = NULL){
  
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
                                  values   = vals_colors_scale,
                                  na.value = "grey80") +
    
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
biplot_fig2c <- function(data, xlab, ylab, color_scale, vals_colors_scale, log.transf, quant.prob, name = NULL){
  
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
    geom_point(mapping = aes(color = layer)) +
    geom_smooth(method  = lm, 
                col     = "grey10") +
    
    ylim(c(0, max(data$Count_ORO))) +

    xlab(label = xlab) +
    ylab(label = ylab) +
    geom_text_repel(data         = filter(data, labels == TRUE), 
                    mapping      = aes(label = Country, color = layer),
                    max.overlaps = 100,
                    show.legend  = FALSE,
                    min.segment.length = 0.1) +
    # scale_color_manual(values = color_scale, 
    #                    name   = "ORO branch:",
    #                    labels = c("Adaptation", "50/50", "Mitigation")) +
    scale_color_gradientn(name = "% mit. ORO",
                          colors   = color_scale,
                          values   = vals_colors_scale,
                          na.value = "grey80") +
    theme_bw() +
    guides(size = "none", color = guide_colourbar(title.position = "top", barwidth = 8, barheight = 0.7)) +
    theme(legend.position = c(0.15,0.9),
          axis.text.x     = element_text(size = 11),
          axis.text.y     = element_text(size = 11),
          axis.title.x    = element_text(size = 13),
          axis.title.y    = element_text(size = 13),
          legend.text     = element_text(size = 12),
          legend.title    = element_text(size  = 13, 
                                         face  = "bold", 
                                         hjust = 0.5, 
                                         vjust = 0.5),
          legend.title.align = 0.5, 
          legend.direction   = "horizontal") 
    
  
  if(! is.null(name)) {
    
    ggplot2::ggsave(here::here("figures", paste0(name, ".jpeg")), width = 7, height = 5, device = "jpeg")
    
  }
  
  return(plot)
  
}


#' Check Geoparsing Outputs
#'
#' @param data the data
#' @param land TRUE/FALSE to select only land or sea results. DEFAULT = NULL to have both land and sea cells
#' @param Place2Filter the place to select (e.g., "bolivarian republic of venezuela")
#' @param world the shapefile of the world
#'
#' @return
#' @export
#'
#' @examples
plot_geoparsing <- function(data, world, land = NULL, Place2Filter){
  
  if(land == TRUE){data <- filter(data, is_land == 1)}
  if(land == FALSE){data <- filter(data, is_land == 0)}
  
  ### ---- Filter the wanted place
  subset_data <- sf::st_as_sf(data |> filter(place == Place2Filter), coords = c("LON", "LAT"), crs = 4326)
  
  ### ---- Plot
  ggplot(data = subset_data) +
    geom_sf(data = world_shp, fill = "grey95") +
    geom_sf(color = "red") +
    theme_bw()
  
}




#' Test Correlation Bewteen Variables
#'
#' @param x the x variable
#' @param y the y variable
#'
#' @return
#' @export
#'
#' @examples
correlation_btw_var <- function(data, log.transf, quant.prob, name = NULL){
  
  ### Log transformation if wanted
  if(log.transf == TRUE){
    data <- data |> 
      mutate(energy_per_capita    = log(energy_per_capita),
             GDP_per_capita = log(GDP_per_capita))
  }
  
  ### Residuals
  data$residuals <- resid(lm(data$energy_per_capita ~ data$GDP_per_capita, data = data))
  data$labels <- abs(data$residuals) >= quantile(abs(data$residuals), prob = quant.prob)
  
  ggplot2::ggplot(data, mapping = ggplot2::aes(x = GDP_per_capita, 
                                               y = energy_per_capita)) +
    
    ggplot2::geom_point(data, mapping = aes(x = GDP_per_capita, 
                                            y = energy_per_capita,
                                            color = continent2)) +
    
    ggplot2::geom_smooth(method = lm, col = "grey15") +
    
    ggpubr::stat_regline_equation(mapping = ggplot2::aes(label = paste(..adj.rr.label.., sep = "~~~~")),
                                  formula = y~x) +
    
    geom_text_repel(data        = filter(data, labels == TRUE), 
                    mapping     = aes(label = country, color = continent2), 
                    show.legend = FALSE,
                    min.segment.length = 0.1) +
    
    scale_color_manual(name = NULL, 
                       values = c("Africa" = "#9c40b8",
                                  "Asia"   = "#1f7819",
                                  "Europe" = "#3456ad",
                                  "North America" = "#e89338",
                                  "Oceania"       = "#7d431f",
                                  "South America" = "#eb4e49")) +
    ggplot2::labs(y = "Energy per capita", 
                  x = "GDP per capita") +
    ggplot2::theme_bw()
  
  
  if(! is.null(name)) {
    
    ggplot2::ggsave(here::here("figures", paste0(name, ".jpeg")), width = 7, height = 5, device = "jpeg")
    
  }
  
}


#' Bivariate Map
#'
#' @param data_map the data ready to map obtained with CarcasSink::format_data_bivariate_map() 
#' @param bivariate_color_scale a df of the bivariate color scale, obtained with CarcasSink::color_bivariate_map()
#' @param name the name of the map to be saved
#'
#' @return
#' @export
#'
#' @examples
bivariate_map <- function(data_map, data_map_univ, data_world, bivariate_color_scale, xlab, ylab, name){
  
  # data_map <- tibble::as.tibble(data_map)
  
  ### Produce the map
  map <- ggplot2::ggplot() +
    
    ## DBEM output grid
    ggplot2::geom_sf(data    = data_map, 
                     mapping = ggplot2::aes(fill     = fill,
                                            geometry = geometry), 
                     color   = NA, 
                     size    = 0.01) +
    
    ggplot2::scale_fill_identity(na.value = "grey80") +
    ggplot2::theme_void() +
    
    ## Add graticules
    # ggplot2::geom_sf(data     = data_map$graticules,
    #                  linetype = "dotted",
    #                  color    = "black",
    #                  size     = 0.4) +
    
    ggnewscale::new_scale_fill() +

    ## Add borders grid
    geom_sf(data        = data_map_univ,
            # mapping     = aes(fill = log(Count_ORO)),
            colour      = "black",
            fill        = "grey90",
            size        = 0.1,
            show.legend = TRUE) +
    scale_fill_gradientn(colors   = viridis::magma(10, direction = -1),
                         na.value = "grey80") +
      
    theme_bw() +
    theme(legend.position = "bottom")
    
    
    # ggplot2::geom_sf(data   = data_map$box,
    #                  colour = "black",
    #                  fill   = NA,
    #                  size   = 0.1) +
    
    ## Add latitude and longitude labels
    # ggplot2::geom_text(data = data_map$lat_text, mapping = ggplot2::aes(x = X.prj2-1*10e5, y = Y.prj,          label = lbl), color = "grey20", size = 1.5) +
    # ggplot2::geom_text(data = data_map$lon_text, mapping = ggplot2::aes(x = X.prj,         y = Y.prj-0.5*10e5, label = lbl), color = "black",  size = 1.5) +
    # 
    ## Theme
    # ggplot2::theme(panel.grid.major.x = ggplot2::element_line(color = NA),
    #                panel.background   = ggplot2::element_blank(),
    #                axis.text          = ggplot2::element_blank(),
    #                axis.ticks         = ggplot2::element_blank(), 
    #                axis.title         = ggplot2::element_blank(),
    #                plot.margin        = ggplot2::unit(c(0,0,0,0), "cm"),
    #                plot.title         = ggplot2::element_text(size  = 12, 
    #                                                           face  = "bold", 
    #                                                           hjust = 0.5, 
    #                                                           vjust = -0.5),
    #                legend.title       = ggplot2::element_text(size  = 20, 
    #                                                           face  = "bold", 
    #                                                           hjust = 0.5, 
    #                                                           vjust = 0.5),
    #                legend.text        = ggplot2::element_text(size = 16))
  
  
  ### Color legend
  
  ## Separate groups
  color <- bivariate_color_scale |> 
    dplyr::mutate(x = as.integer(rep(seq(1, 10, 1), 10)),
                  y = as.integer(rep(1:10, each = 10)))
  
  
  ## Plot
  legend <- ggplot2::ggplot() +
    
    ggplot2::geom_tile(data    = color, 
                       mapping = ggplot2::aes(x = x, y = y, fill = fill)) +
    
    ggplot2::scale_fill_identity() +
    ggplot2::labs(x = xlab, y = ylab) +
    # ggplot2::geom_hline(yintercept = 3.5, color = "red") +
    cowplot::theme_map() +
    ggplot2::theme(axis.title      = ggplot2::element_text(size = 16), 
                   axis.title.x    = ggplot2::element_text(margin = ggplot2::margin(t = 0, 
                                                                                    r = 0, 
                                                                                    b = 0, 
                                                                                    l = 0)),
                   axis.title.y    = ggplot2::element_text(angle  = 90,
                                                           margin = ggplot2::margin(t = 0,
                                                                                    r = 5,
                                                                                    b = 0,
                                                                                    l = 0)),
                   plot.background = ggplot2::element_rect(fill  = "white", 
                                                           color = "transparent")) +
    ggplot2::coord_fixed()
  
  
  ### Arrange map with legend
  map_bi <- cowplot::ggdraw() +
    cowplot::draw_plot(map,    x = 0.0, y = 0.00, width = 0.70, height = 1.0) +
    cowplot::draw_plot(legend, x = 0.65, y = 0.30, width = 0.35, height = 0.35)
  
  ### Save map
  if(! is.null(name)) {
    
    ggplot2::ggsave(here::here("figures", paste0(name, ".jpeg")), width = 8.5, height = 6, device = "jpeg")
    
  }
  
  return(map_bi)
  
}
  
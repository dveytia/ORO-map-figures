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
univariate_map <- function(data_map, eez = NULL, color_scale, second.var, midpoint, title_color, title_size, show.legend, name = NULL){
  
  ### Produce the map
  map <- ggplot2::ggplot() +
    
    ## DBEM output grid
    ggplot2::geom_sf(data    = data_map$data,
                     mapping = ggplot2::aes(fill     = layer,
                                            geometry = geometry),
                     color   = "grey10",
                     size    = 0.1,
                     show.legend = show.legend) +

    
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
    ggplot2::geom_text(data = data_map$lon_text, mapping = ggplot2::aes(x = X.prj,         y = Y.prj-0.5*10e5, label = lbl), color = "black",  size = 1.5) 
    
    # ggplot2::labs(fill = legend)
  
  if((!is.null(color_scale)) & color_scale != "trends"){

    map <- map +

    ggplot2::guides(size = ggplot2::guide_legend(title = title_size, title.position = "right", title.hjust = 0.5, ncol = 1, override.aes = list(fill = "transparent")),
                    fill = ggplot2::guide_colourbar(title = title_color, title.position = "right", barwidth = 0.7))

  }

  if((!is.null(color_scale)) & color_scale == "trends"){
     map <- map 
       # ggplot2::guides(size = ggplot2::guide_legend(title = title_color, title.position = "right", title.hjust = 0.5, ncol = 1, override.aes = list(fill = "transparent")))
       #                 fill = ggplot2::guide_coloursteps(title = title_color, title.position = "right", barwidth = 0.7)) 
  }
    
    ## Theme
    map <- map +
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
                     legend.position     = "right",
                     legend.justification = "center",
                     legend.key           = element_rect(color = "white"),
                     legend.text        = ggplot2::element_text(size = 12))
  
  
  if(!is.null(midpoint)){
    map <- map + 
      ggplot2::scale_fill_gradient2(low  = color_scale[1], 
                                    high = color_scale[3],
                                    mid  = color_scale[2],
                                    midpoint = midpoint,
                                    na.value = "grey80")
  }
  
  
  if((!is.null(color_scale)) & color_scale != "trends"){
    map <- map + 
      ggplot2::scale_fill_gradientn(colors   = color_scale,
                                    # values   = vals_colors_scale,
                                    na.value = "grey80") 
  }
  
  
  if((!is.null(color_scale)) & color_scale == "trends"){
    map <- map +
      ggplot2::scale_fill_manual(name = "Slope",
                                 values   = c(viridis::mako(length(unique(data_2_map_panelB$data$layer))-2, direction = 1), "#e6df85"),
                                 na.value = "grey80")
  }
  
  
  if(!is.null(eez)){
    
    map <- map + 
      
      # ggnewscale::new_scale_fill() +
      
      ggplot2::geom_sf(data    = eez,
                       mapping = ggplot2::aes(fill     = layer,
                                              geometry = geometry),
                       color   = "grey10",
                       size    = 0.1,
                       show.legend = FALSE) 
      
      if((!is.null(color_scale)) & color_scale != "trends"){
        map <- map + 
          ggplot2::scale_fill_gradientn(colors   = color_scale,
                                        # values   = vals_colors_scale,
                                        na.value = "grey80") 
      }
    
    
      if((!is.null(color_scale)) & color_scale == "trends"){
        map <- map + 
          ggplot2::scale_fill_manual(name = "Slope",
                                     values   = c(viridis::mako(length(unique(data_2_map_panelB$data$layer))-2, direction = 1), "#e6df85"),
                                     na.value = "grey80") +
          ggplot2::theme(legend.title = ggplot2::element_text(size  = 12, 
                                                              face  = "bold",
                                                              hjust = 0,
                                                              angle = 0))
      }
      
      # ggplot2::scale_fill_gradientn(colors   = color_scale,
      #                               # values   = vals_colors_scale,
      #                               na.value = "grey80")
    
  }
  
  if(!is.null(second.var)){
    map <- map +
      stat_sf_coordinates(data        = data_map$data |> filter(! group_land %in% c("Island", "AMUNRC")),
                          mapping     = aes(size = get(second.var)),
                          colour      = "darkblue",
                          # fill        = "grey90",
                          # size        = 1,
                          show.legend = TRUE) +
      scale_size(range = c(0,3))
  }
  
  ### Save map
  if(! is.null(name)) {
    
    ggplot2::ggsave(here::here("figures", paste0(name, ".pdf")), width = 7, height = 4.5, device = "pdf")
    
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
biplot_fig2c <- function(data, var.x, var.y, var.col, var.shape, xlab, ylab, one_one_line, color_scale, color_title, log.transf, quant.prob, name = NULL){
  
  # data <- data |> filter(!is.na(dominant_ORO) & !is.na(Count_ORO) & !is.na(Record.Count))
    
  ### Log transformation if wanted
  if(log.transf == TRUE){
    data <- data |> 
      mutate(var_x = log(get(var.x)+1),
             var_y = log(get(var.y)+1)) |> 
      ungroup()
  } else {data <- data |> rename(var_x = all_of(var.x), var_y = all_of(var.y)) |>  ungroup()}
  
  ### Residuals
  data <- data |> 
    mutate(residuals = resid(lm(var_y ~ var_x, data = cur_data())),
           labels    = abs(residuals) >= quantile(abs(residuals), prob = quant.prob))
  
  
  # data$labelsFR <- FALSE
  # data$labelsFR[data$country == "France"] <- TRUE
  
  plot <- ggplot(data    = data, 
                 mapping = aes(x = var_x, 
                               y = var_y)) +
    geom_point(mapping = aes(color = get(var.col)), size = 2.5) + # , shape = get(var.shape)
    geom_smooth(method  = lm, 
                col     = "grey10") +
    
  
    # ylim(c(0, max(data$Count_ORO))) +

    xlab(label = xlab) +
    ylab(label = ylab) +
    geom_text_repel(data         = filter(data, labels == TRUE), 
                    mapping      = aes(label = country), # 
                    max.overlaps = 100,
                    size   = 5,
                    show.legend  = FALSE,
                    min.segment.length = 0.1) +
    
    # geom_text_repel(data         = filter(data, labelsFR == TRUE), 
    #                 mapping      = aes(label = country), # 
    #                 # color = "darkred",
    #                 max.overlaps = 100,
    #                 size   = 10,
    #                 show.legend  = FALSE,
    #                 min.segment.length = 0.1) +
    
    scale_color_manual(values = c("SIDS" = "#0fbcd6",
                                 "Land-locked" = "#b36705",
                                 "Coastal" = "#3f47e8"),
                      name = NULL) +

    # scale_shape_manual(name = NULL, 
    #                    values = c("Land-locked" = 15,
    #                               "Coastal"     = 16,
    #                               "SIDS"        = 17)) +
    
    # scale_color_gradientn(colors   = color_scale,
    #                       na.value = "grey80") +
    theme_bw() +
    
    # guides(size = "none", color = guide_colourbar(title.position = "top", barwidth = 8, barheight = 0.7)) +
    guides(color = ggplot2::guide_legend(title.position = "top", title.hjust = 0.5, ncol = 1, override.aes = list(size = 6, fill = "transparent"))) +
           # color = "none") +
           # color = ggplot2::guide_colourbar(title = color_title, title.position = "top", barwidth = 0.7)) +
    # 
    theme(axis.text.x     = element_text(size = 12),
          axis.text.y     = element_text(size = 12),
          axis.title.x    = element_text(size = 14),
          axis.title.y    = element_text(size = 14),
          legend.text     = element_text(size = 25,
                                         face  = "bold"),
          legend.title    = element_text(size  = 14, 
                                         face  = "bold", 
                                         hjust = 0.5, 
                                         vjust = 0.5),
                                         # angle = -90),
          legend.title.align = 0.5, 
          legend.position    = c(0.2,0.85), # "right" 
          legend.direction   = "horizontal") 
  
  if(one_one_line == TRUE){plot <- plot + geom_abline(slope = 1, linetype = "dotted")}
  
  # if(length(color_scale) == 3){
    # plot <- plot +
      # scale_fill_gradient2(low  = color_scale[1], 
      #                      high = color_scale[3],
      #                      mid  = color_scale[2],
      #                      midpoint = 0.5,
      #                      na.value = "grey80")
    

    # }
    
  if(! is.null(name)) {
    
    ggplot2::ggsave(here::here("figures", paste0(name, ".pdf")), width = 9, height = 5, device = "pdf")
    
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
    
    ggplot2::ggsave(here::here("figures", paste0(name, ".pdf")), width = 7, height = 5, device = "pdf")
    
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
bivariate_map <- function(data_map, data_map_univ, eez = NULL, data_world, color, univariate_color_scale, xlab, ylab, lab_univ, name){
  
  # data_map <- tibble::as.tibble(data_map)
  
  ### Produce the map
  map <- ggplot2::ggplot() +
    
    ## DBEM output grid
    ggplot2::geom_sf(data    = data_map$data, 
                     mapping = ggplot2::aes(fill     = fill,
                                            geometry = geometry), 
                     color   = "black", 
                     size    = 0.1) +
    
    ggplot2::scale_fill_identity(na.value = "grey80") +
    
    ggplot2::geom_sf(data   = data_map$box, 
                     colour = "black", 
                     fill   = NA, 
                     size   = 0.1) +
    
    
    ## Add latitude and longitude labels
    ggplot2::geom_text(data = data_map$lat_text, mapping = ggplot2::aes(x = X.prj2-1*10e5, y = Y.prj,          label = lbl), color = "grey20", size = 1.5) +
    ggplot2::geom_text(data = data_map$lon_text, mapping = ggplot2::aes(x = X.prj,         y = Y.prj-0.5*10e5, label = lbl), color = "black",  size = 1.5) + 
    
    ggplot2::theme_void() +
    
    ## Add graticules
    # ggplot2::geom_sf(data     = data_map$graticules,
    #                  linetype = "dotted",
    #                  color    = "black",
    #                  size     = 0.4) +
    
    # ggnewscale::new_scale_fill() +

    ## Add borders grid
    # geom_sf(data = data_map_univ,
    #         mapping     = aes(fill = log(Count_ORO)),
    #         # fill = "grey85",
    #         color = "black",
    #         size  = 0.2) +
  
    # stat_sf_coordinates(data        = data_map_univ,
    #                     mapping     = aes(color = log(Count_ORO)),
    #                     # colour      = "black",
    #                     # fill        = "grey90",
    #                     size        = 0.8,
    #                     show.legend = TRUE) +

    # scale_fill_gradientn(name     = lab_univ,
    #                      colors   = univariate_color_scale,
    #                      na.value = "grey90") +
    
    # guides(size = "none", color = guide_colourbar(title.position = "top", barwidth = 8, barheight = 0.7)) +
    # guides(size = "none", fill = guide_colourbar(title.position = "top", barwidth = 8, barheight = 0.7, direction = "horizontal")) +
    
    # theme_bw() +
    theme(axis.title.x     = element_blank(),
          axis.title.y     = element_blank())
    # theme(legend.position = "bottom",
    #       # legend.justification = "top",
    #       axis.title.x     = element_blank(),
    #       axis.title.y     = element_blank())
    
    
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
  
  
  if(!is.null(data_world) == TRUE){
    map <- map +
      geom_sf(data = data_world,
              color = "black",
              size  = 0.1)
  }

  if(!is.null(eez)){

    map <- map +

      # ggnewscale::new_scale_fill() +

      ggplot2::geom_sf(data    = eez,
                       mapping = ggplot2::aes(fill     = fill,
                                              geometry = geometry),
                       color   = "grey10",
                       size    = 0.1,
                       show.legend = FALSE) +

      ggplot2::scale_fill_identity(na.value = "grey80")


  }

  ## Separate groups
  data_col <- data_map$data |>  
    sf::st_drop_geometry() |> 
    separate(col = group, into = c("x", "y"), sep = "\\.", convert = TRUE, remove = FALSE) |>
    filter(! is.na(fill)) |> 
    group_by(fill) |> 
    summarize(x=  x, y = y, count = n())
  
  # x_quantile <- c(0, quantile(data$Count_ORO, probs = seq(0,1,0.1), na.rm = TRUE))
  
  color <- color |> 
    separate(col = group, into = c("x", "y"), sep = "\\.", convert = TRUE, remove = FALSE) 
    # left_join(data_col |>  dplyr::select(NA2_DESCRI, cumulative_co2_including_luc, Count_ORO, fill), by = "fill")
    # dplyr::mutate(x = as.integer(rep(seq(1, 10, 1), 10)),
    #               y = as.integer(rep(1:10, each = 10)))
  
  ## Plot
  legend <- ggplot2::ggplot() +
    
    ggplot2::geom_tile(data    = color,
                       mapping = ggplot2::aes(x = x, y = y, fill = fill)) +
    # ggplot2::geom_tile(data    = color2, 
    #                    mapping = ggplot2::aes(x = Count_ORO, y = cumulative_co2_including_luc, fill = fill)) +
    
    # ggplot2::scale_x_continuous(labels = as.vector(x_quantile), breaks = as.vector(x_quantile)) +
    ggplot2::scale_fill_identity() +
    
    ggplot2::geom_point(data = data_col, mapping = aes(x = x, y = y, size = count), show.legend = TRUE) +
    ggplot2::scale_size(range = c(0, 4)) +
    
    ggplot2::labs(x = xlab, y = ylab) +
    ggplot2::geom_vline(xintercept = 3.5, color = "red") +
    cowplot::theme_map() +
    guides(size = guide_legend(title = "# country", title.position = "right", title.hjust = 0.5, ncol = 1)) +
    ggplot2::theme(axis.title      = ggplot2::element_text(size = 13), 
                   axis.title.x    = ggplot2::element_text(margin = ggplot2::margin(t = 0,
                                                                                    r = 0,
                                                                                    b = 0,
                                                                                    l = 0)),
                   axis.title.y    = ggplot2::element_text(angle  = 90,
                                                           margin = ggplot2::margin(t = 0,
                                                                                    r = 5,
                                                                                    b = 0,
                                                                                    l = 0)),
                   legend.key      = element_rect(fill = 'transparent', colour = 'transparent'),
                   legend.position = "right",
                   legend.justification = "center",
                   legend.title         = element_text(angle = -90),
                   legend.margin      = margin(0,0,0,0),
                   legend.box.spacing = unit(0, "pt"),
                   plot.background = ggplot2::element_rect(fill  = "white", 
                                                           color = "transparent")) +
    ggplot2::coord_fixed() ; legend
  
  
  ### Arrange map with legend
  # map_bi <- cowplot::ggdraw() +
  #   cowplot::draw_plot(map,    x = 0.0, y = 0.00, width = 0.70, height = 1.0) +
  #   cowplot::draw_plot(legend, x = 0.65, y = 0.30, width = 0.35, height = 0.35)
  map_bi <- cowplot::ggdraw() +
    cowplot::draw_plot(map,    x = 0.0, y = 0.00, width = 0.65, height = 1.0) +
    cowplot::draw_plot(legend, x = 0.65, y = 0.3, width = 0.35, height = 0.35)
  
  ### Save map
  if(! is.null(name)) {
    
    ggplot2::ggsave(here::here("figures", paste0(name, ".pdf")), width = 8.5, height = 6, device = "pdf")
    
  }
  
  return(map_bi)
  
}
  

#' Donuts Plots
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
donuts_plots <- function(data, group){
  
  ### Quartiles
  GDP_quartiles <- quantile(data$GDP_per_capita, probs = seq(0, 1, 0.25), na.rm = TRUE)
  data <- data |> 
    mutate(gdp_quartile = cut(GDP_per_capita, breaks = unique(GDP_quartiles), include.lowest = TRUE),
           group        = ifelse(!is.na(gdp_quartile), as.numeric(gdp_quartile), NA))
  
  
  ### Format Data
  if(group == "sids"){
    
    data_grp <- data |> 
      # filter(! is.na(group)) |> 
      group_by(group_land, oro_type) |> 
      summarise(n_mean = sum(n_mean, na.rm = TRUE)) |> 
      mutate(n_paper_tot = case_when(group_land == "Coastal" ~ sum(n_mean[group_land == "Coastal"], na.rm = TRUE),
                                     group_land == "SIDS" ~ sum(n_mean[group_land == "SIDS"], na.rm = TRUE),
                                     group_land == "Land-locked" ~ sum(n_mean[group_land == "Land-locked"], na.rm = TRUE),
                                     group_land == "AMUNRC" ~ sum(n_mean[group_land == "AMUNRC"], na.rm = TRUE)),
             contrib     = (n_mean/n_paper_tot)*100) |>
      group_split(group_land)
    
  }
  
  if(group == "gdp"){
    
    data_grp <- data |> 
      filter(! is.na(group)) |>
      group_by(group, oro_type) |> 
      summarise(n_mean = sum(n_mean, na.rm = TRUE),
                group  = unique(group)) |> 
      mutate(n_paper_tot = case_when(group == 4 ~ sum(n_mean[group == 4], na.rm = TRUE),
                                     group == 3 ~ sum(n_mean[group == 3], na.rm = TRUE),
                                     group == 2 ~ sum(n_mean[group == 2], na.rm = TRUE),
                                     group == 1 ~ sum(n_mean[group == 1], na.rm = TRUE)),
             contrib     = (n_mean/n_paper_tot)*100) |>
      group_split(group)
    
  }
  
  
  if(group == "continent"){
    
    data_grp <- data |> 
      # filter(! is.na(group)) |>
      group_by(continent, oro_type) |> 
      summarise(n_mean = sum(n_mean, na.rm = TRUE)) |> 
      mutate(n_paper_tot = case_when(continent == "North America" ~ sum(n_mean[continent == "North America"], na.rm = TRUE),
                                     continent == "Europe" ~ sum(n_mean[continent == "Europe"], na.rm = TRUE),
                                     continent == "Asia" ~ sum(n_mean[continent == "Asia"], na.rm = TRUE),
                                     continent == "South America" ~ sum(n_mean[continent == "South America"], na.rm = TRUE),
                                     continent == "Africa" ~ sum(n_mean[continent == "Africa"], na.rm = TRUE),
                                     continent == "Oceania" ~ sum(n_mean[continent == "Oceania"], na.rm = TRUE)),
             contrib     = (n_mean/n_paper_tot)*100) |>
      group_split(continent)
    
  }
  

  

  ### Plot data
  plot_ls <- lapply(data_grp, function(x){
    
    x_plot <- x |> 
      # mutate(oro_type = factor(oro_type, levels = c("Mitigation", "Natural resilience", "Societal adaptation")),
      mutate(oro_type = factor(oro_type, levels = c("Marine renewable energy",
                                                    "CO2 removal or storage",
                                                    "Increase efficiency",
                                                    "Conservation",
                                                    "Human assisted evolution",
                                                    "Built infrastructure & technology",
                                                    "Socio-institutional")),
             Type     = "Type",
             pos         = round(cumsum(contrib) - (0.5 * contrib), 2)) |> 
      arrange(oro_type) 
    
    ggplot(x_plot, aes(x = Type, y = -contrib, fill = oro_type)) +
      geom_col(show.legend = F) +
      # geom_text(aes(label = paste0(round(contrib, 1), "%"), x = Type, y = pos), size = 4, color = "white") +
      # Colors
      scale_fill_manual(name   = NULL,
                        values = c("Marine renewable energy"            = "#026996",
                                   "CO2 removal or storage"             = "#0688c2",
                                   "Increase efficiency"                = "#9ed7f0",
                                   "Conservation"                       = "#078257",
                                   "Human assisted evolution"           = "#43b08a",
                                   "Built infrastructure & technology"  = "#600787",
                                   "Socio-institutional"                = "#ad5ad1")) +
                        # values = c("Mitigation" = "#35a7d9", 
                        #            "Natural resilience" = "forestgreen", 
                        #            "Societal adaptation" = "#7670a8")) +
      scale_x_discrete(limits = c(" ", "Type")) +
      coord_polar("y") +
      theme_void()
    
  })

  cowplot::plot_grid(plotlist = plot_ls, ncol = 2)
  
  return(plot_ls) 

}


#' Title
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
barplots_gdp <- function(data, group = FALSE){
  
  
    ### Quartiles
    data <- data |> 
      select(country_aff, GDP_per_capita) |> 
      distinct() |> 
      mutate(group = ntile(GDP_per_capita, 4)) |> 
      select(-GDP_per_capita) |> 
      left_join(data, by = "country_aff")
    
    ### Format data
    if(group == "sids"){
      
      val_order <- data |> 
        group_by(country) |> 
        summarise(GDP_per_capita = unique(GDP_per_capita),
                  group_land  = unique(group_land)) |>
        arrange(group_land, -GDP_per_capita) |> 
        ungroup() |> 
        mutate(valueOrder = as.factor(row_number())) |> 
        select(-GDP_per_capita) |> 
        filter(group_land != "AMUNRC")
        
        ### Horizontal lines for quartiles
        vline1 <- length(unique(val_order$country[val_order$group_land == "Coastal"])) + 0.5
        vline2 <- length(unique(val_order$country[val_order$group_land == "Land-locked"])) + vline1 
        
        val_order <- select(val_order, -group_land)
    
    }
    
    if(group == "gdp"){
      
      val_order <- data |> 
        group_by(country) |> 
        summarise(GDP_per_capita = unique(GDP_per_capita),
                  group  = unique(group)) |>
        arrange(-group, -GDP_per_capita) |>
        ungroup() |> 
        mutate(valueOrder = as.factor(row_number())) |> 
        select(-GDP_per_capita) 
      
      ### Horizontal lines for quartiles
      vline1 <- length(unique(val_order$country[val_order$group == 4])) + 0.5
      vline2 <- length(unique(val_order$country[val_order$group == 3])) + vline1 
      vline3 <- length(unique(val_order$country[val_order$group == 2])) + vline2
      
      val_order <- select(val_order, -group)
      
      
    }
    
    if(group == "continent"){
      
      val_order <- data |> 
        group_by(country) |> 
        summarise(GDP_per_capita = unique(GDP_per_capita),
                  continent  = unique(continent)) |>
        arrange(continent, -GDP_per_capita) |>
        ungroup() |> 
        mutate(valueOrder = as.factor(row_number())) |> 
        select(-GDP_per_capita) 
      
      ### Horizontal lines for quartiles
      vline1 <- length(unique(val_order$country[val_order$continent == "North America"])) + 0.5
      vline2 <- length(unique(val_order$country[val_order$continent == "Europe"])) + vline1 
      vline3 <- length(unique(val_order$country[val_order$continent == "Asia"])) + vline2
      vline4 <- length(unique(val_order$country[val_order$continent == "Oceania"])) + vline3 
      vline5 <- length(unique(val_order$country[val_order$continent == "Africa"])) + vline4
      
      val_order <- select(val_order, -continent)
      
      
    }
    
    if(group == FALSE){
        
        val_order <- data |> 
          group_by(country) |> 
          # summarise(n_mean = sum(n_mean),
          #           group  = unique(group)) |> 
          # arrange(-group, -n_mean) |> 
          summarise(GDP_per_capita = unique(GDP_per_capita)) |>
          arrange(-GDP_per_capita) |> 
          ungroup() |> 
          mutate(valueOrder = as.factor(row_number())) |> 
          select(-GDP_per_capita)
    }

    data_arrange <- left_join(data, val_order, by = "country") |> 
      filter(!is.na(country)) |> 
      # mutate(oro_branch = factor(oro_branch, levels = c("# ORO pubs", "Mitigation", "Natural resilience", "Societal adaptation")),
      mutate(oro_branch = factor(oro_branch, levels = rev(c("# ORO pubs", 
                                                        "Marine renewable energy",
                                                        "CO2 removal or storage",
                                                        "Increase efficiency",
                                                        "Conservation",
                                                        "Human assisted evolution",
                                                        "Built infrastructure & technology",
                                                        "Socio-institutional"))),
             panel = factor(panel, levels = c("Total number of articles", "% of total"))) |> 
      filter(!is.na(valueOrder))
    

    ### Plot data
    plot <- ggplot(data_arrange, aes(x = valueOrder, y = n_mean, fill = oro_branch)) +
      
      # Bars
      # geom_col(position = position_dodge(), show.legend = TRUE) +
      geom_col() +
      # geom_errorbar(data     = data_arrange |>  filter(panel == "# ORO pubs"),
      #               mapping  = aes(ymin = n_lower, ymax = n_upper),
      #               position = position_dodge(0.9),
      #               width    = .2) +
      geom_errorbar(data     = data_arrange |>  filter(panel == "# ORO pubs"),
                    mapping  = aes(ymin = n_lower, ymax = n_upper)) +
      
      # geom_vline(xintercept = c(0.25*n_country, 0.5*n_country, 0.75*n_country)) +
      
      scale_x_discrete(labels = gsub("_"," ", unique(data_arrange$iso_code[order(data_arrange$valueOrder)]))) +
      
      # facet_grid(oro_branch ~ ., 
      #            scales = "free_y") +
      
      facet_grid(panel ~ ., 
                 scales = "free_y",
                 switch = "y") +
      
      # Colors
      scale_fill_manual(name = "ORO branch",
                        # values = c("# ORO pubs" = "grey20",
                        #            "Mitigation" = "#35a7d9",
                        #            "Natural resilience" = "forestgreen",
                        #            "Societal adaptation" = "#7670a8"),
                        values = c("# ORO pubs" = "grey20",
                                   "Marine renewable energy"            = "#026996",
                                   "CO2 removal or storage"             = "#0688c2",
                                   "Increase efficiency"                = "#9ed7f0",
                                   "Conservation"                       = "#078257",
                                   "Human assisted evolution"           = "#43b08a",
                                   "Built infrastructure & technology"  = "#600787",
                                   "Socio-institutional"                = "#ad5ad1"),
                        # limits = c("Mitigation", "Natural resilience", "Societal adaptation"))+
                        limits = c("Marine renewable energy","CO2 removal or storage","Increase efficiency",
                                   "Conservation","Human assisted evolution","Built infrastructure & technology","Socio-institutional"),
                        labels = c("Marine renewable energy"            = "MRE",
                                   "CO2 removal or storage"             = "CO2 removal & storage",
                                   "Increase efficiency"                = "Increase efficiency",
                                   "Conservation"                       = "Conservation",
                                   "Human assisted evolution"           = "Human assisted  evolution",
                                   "Built infrastructure & technology"  = "Built infrastructure & technology",
                                   "Socio-institutional"                = "Socio-institutional")) +
                        # labels = c("# ORO pubs" = "#",
                        #            "Mitigation" = "Mitigation",
                        #            "Natural resilience" = "Natural resilience",
                        #            "Societal adaptation" = "Societal adaptation")) +
      
      
      labs(y = NULL, x = NULL) +
     
      theme_bw() +
      
      # guides(size = "none", fill = guide_colourbar(title.position = "top")) + #  barwidth = 8, barheight = 0.7,direction = "horizontal"
      guides(fill = guide_legend(nrow = 1)) +
    
      theme(axis.text.x     = element_text(size = 10, angle = 60, hjust = 1, vjust = 1.1), # , vjust = 0.5
            legend.position = "bottom",
            axis.text.y     = element_text(size = 13),
            strip.text.y    = element_text(size = 14),
            # axis.title.x    = element_text(size = 13),
            # axis.title.y    = element_text(size = 13),
            legend.text     = element_text(size = 15),
            legend.title    = element_text(size  = 15, face = "bold"),
            strip.placement = "outside", 
            strip.background.y = element_rect(color = NA,  fill=NA),
            legend.justification = "top") ; plot
            # strip.background = element_rect(fill = c("#35a7d9", "forestgreen", "#7670a8")))
    
    if(group == "sids"){ plot <- plot + geom_vline(xintercept = c(vline1, vline2))}
    if(group == "gdp"){ plot <- plot + geom_vline(xintercept = c(vline1, vline2, vline3))}
    if(group == "continent"){ plot <- plot + geom_vline(xintercept = c(vline1, vline2, vline3, vline4, vline5))}
    
    ### Change strips backgrounds
    # fill_colors = c("white","#35a7d9", "forestgreen", "#7670a8")
    # plot2 <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(plot))
    # strips <- which(startsWith(plot2$layout$name,'strip'))
    # 
    # for (s in seq_along(strips)) {
    #   plot2$grobs[[strips[s]]]$grobs[[1]]$children[[1]]$gp$fill <- fill_colors[s]
    # }
    # 
    # plot(plot2)
    
    # if(label_x == FALSE){
    #   
    #   plot <- plot +
    #     theme(axis.text.x  = element_blank(),
    #           axis.title.x = element_blank(),
    #           axis.ticks.x = element_blank())
    #   
    # }
    # 
    # return(plot)
    
  # }
  
    ## Plot Mitigation
    # plot_mit <- function_plot(data_oro = data_mit,
    #                           label_x  = FALSE) 
    # 
    # ## Plot Nature
    # plot_nat <- function_plot(data_oro = data_nat,
    #                           label_x  = FALSE) 
    # 
    # ## Plot Societal Adaptation
    # plot_ada <- function_plot(data_oro = data_ada,
    #                           label_x  = TRUE) 
    
    ## Add plots
    # plot_final <- cowplot::ggdraw() +
    #   cowplot::draw_plot(plot_mit,    x = 0.0, y = 0.725, width = 1, height = 0.275) +
    #   cowplot::draw_plot(plot_nat,    x = 0.0, y = 0.45, width = 1, height = 0.275) +
    #   cowplot::draw_plot(plot_ada,    x = 0.0, y = 0.00, width = 1, height = 0.45)
      
  return(plot)

}

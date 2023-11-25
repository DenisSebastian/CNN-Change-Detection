library(ggplot2)
library(dplyr)



# Histogramas de Raster ---------------------------------------------------
hist_raster <-  function(r, bins = 50, title_g = NULL, fill_color = "#225ea8"){
  if(is.null(title_g)){
    title_g <- "Histograma de Raster"
  }
  vr <- values(r)
  vr_df <-  data.frame(values_r = vr)
  p <- ggplot(vr_df, aes(x = values_r)) +
    geom_histogram(bins = bins, fill = "#225ea8", color ="gray70")+
    theme_bw()+
    labs(title=title_g, x ="Valores", y = "Frecuencias")+
    theme(plot.title = element_text(face = "bold",colour= "gray60", size=10)) 
  
  return(p)
  
}

plot_raster <- function(r, title_g = NULL, palette_col = NULL, legend = FALSE) {
  if (is.null(title_g)) {
    title_g <- "Raster"
  }
  
  if (is.null(palette_col)) {
    palette_col <- rev(colorspace::diverge_hcl(7))# create a color ramp of grey colors
  }
  
  plot(
    r,
    col = palette_col,
    legend = legend,
    main = title_g,
    axes = FALSE
  )
  
}

plot_raster_g <- function(r, title_g = NULL, palette_col = NULL, legend = FALSE) {
  if(is.null(title_g)){
    title_g <- "Raster"
  }
  if (is.null(palette_col)) {
    palette_col <- rev(colorspace::diverge_hcl(7))# create a color ramp of grey colors
  }
  
  names(r) <- "layer"
  r_df <- raster::as.data.frame(r, xy = TRUE) %>% 
    na.omit(r_df)
  
  ggplot() +
    geom_raster(data = r_df , aes(x = x, y = y, fill = layer))+
    geom_tile()+
    coord_fixed() +
    scale_fill_gradientn(colours = palette_col, name=NULL, )+
    guides(fill = guide_colourbar(barwidth = 0.5,
                                  barheight = 15))+
    # scale_fill_viridis_c(direction = -1)+
    theme_bw()+
    labs(title=title_g, x = "", y = "")+
    theme(plot.title = element_text(face = "bold",colour= "gray60", size=10)) 
  
  
  
}

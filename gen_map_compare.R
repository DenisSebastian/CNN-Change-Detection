
# Generarción de Mapas Web para presentaciones

# Opciones Generales ------------------------------------------------------
options(browser="/usr/bin/open -a 'Google Chrome'")
setwd("../../book-change-detection/")

# Librerias ---------------------------------------------------------------
library(raster)
library(mapview)
library(leafsync)

# Recursos ----------------------------------------------------------------
source("R/DI.R")
source("R/graphics.R")
source("R/functions.R")
source("R/utils_image.R")
source("R/Douglas-Peucker.R")
source("R/log_mean_ratio.R")


# Paletas -----------------------------------------------------------------
palette_grey <- grey(1:100/100)
palette_rainbow <- rainbow(100)

# Si no existe directorio lo crea
make_dir <- function(path){
  if (!dir.exists(path)){
    print(paste0("Directorio Creado: ", path))
    dir.create(path, recursive = TRUE)
  }
}

make_map_web <- function(map, name, path_out, show=T){
  make_dir(path_out)
  
  path_out_html <- paste0(path_out, "/", name,".html") %>% 
    gsub(patter="//", replacement = "/", .)
  mapview::mapshot(map, url = path_out_html)
  if(isTRUE(show)){
    utils::browseURL(path_out_html)
  }
  print(paste0("Mapa web guardado en: ", path_out_html))
}

delete_black <- function(path_tif){
  img_rbg <- terra::rast(path_tif) %>% 
    raster::brick() %>%
    subset(c(1, 2, 3))
  # set all values with 0 to NA
  vals = values(img_rbg)
  idx = which(rowSums(vals) == 0)
  vals[idx, ] = cbind(NA, NA, NA)
  
  img_rbg = setValues(img_rbg, vals)
  return(img_rbg)
}

save_map_sync <- function (map_sync, path_out, name = "map", selfcontained = F, libdir = "./lib",  show=T) 
{
  make_dir(path_out)
  file <- paste0(path_out, "/", name,".html") %>% 
    gsub(patter="//", replacement = "/", .)
  
  if (is.null(libdir)) {
    libdir <- paste(tools::file_path_sans_ext(basename(file)), 
                    "_files", sep = "")
  }
  htmltools::save_html(map_sync, file = file, libdir = libdir)
  if (selfcontained) {
    # if (!htmlwidgets:::pandoc_available()) {
    #   stop("Saving a widget with selfcontained = TRUE requires pandoc. For details see:\n", 
    #        "https://github.com/rstudio/rmarkdown/blob/master/PANDOC.md")
    # }
    htmlwidgets:::pandoc_self_contained_html(file, file)
    unlink(libdir, recursive = TRUE)
  }
  if(isTRUE(show)){
    utils::browseURL(file)
  }
  
  return(print(paste0("Mapa web guardado en: ", file)))
  # https://github.com/r-spatial/mapview/issues/35
}

# Área de Estudio ---------------------------------------------------------

SITIO <- "ST_021"
suffix <- "_ODC"
suffix_null <- ""
path_avence <- "../presentaciones/avance_tesis"
path_html <- paste0(path_avence, "/html/", SITIO)
alpha_layer <- 0.8


# ViewRGB -----------------------------------------------------------------
# RGB
st_2017 <- delete_black(paste0("results/", SITIO, "/", SITIO,"_2017_RGB_reproy.tif"))
st_2022 <- delete_black(paste0("results/", SITIO, "/", SITIO,"_2021_RGB_reproy.tif"))

m_2017 <- viewRGB(st_2017, r = 1, g = 2, b = 3, layer.name = "RBG_2017",
                  na.color = "transparent", quantiles = NULL)
m_2022 <- viewRGB(st_2022, r = 1, g = 2, b = 3, layer.name = "RBG_2022",
                  na.color = "transparent", quantiles = NULL)

rgb_img <- m_2017 + m_2022
make_map_web(map = rgb_img, name = "RGB_roi", path_out = path_html)


# Radar -------------------------------------------------------------------
# Insumos -----------------------------------------------------------------

img1_vh <- raster(paste0("data/turberas/tif/", SITIO, "/S1_GRD_SAR_VH_1S_2017.tif"))
img1_vv <- raster(paste0("data/turberas/tif/", SITIO, "/S1_GRD_SAR_VV_1S_2017.tif"))
img2_vh <- raster(paste0("data/turberas/tif/",SITIO,  "/S1_GRD_SAR_VH_1S_2023.tif"))
img2_vv <- raster(paste0("data/turberas/tif/",SITIO,  "/S1_GRD_SAR_VV_1S_2023.tif"))

# img1 <- img1_vv+img1_vh
# img2 <- img2_vv+img2_vh


# img1 <- img1_vh
# img2 <- img2_vh

img1 <- raster(paste0("data/turberas/tif/", SITIO, "/MEDIAN_VH_2016_asf_s1_grd.tif"))
img2 <-raster(paste0("data/turberas/tif/", SITIO, "/MEDIAN_VH_2023_asf_s1_grd.tif"))


sar_map_gee <- rgb_img + 
  mapview(img1_vh, layer.name = "img1_vh_gee", na.color =NA, alpha = alpha_layer, legend=F) + 
  mapview(img2_vh, layer.name = "img2_vh_gee", na.color =NA, alpha = alpha_layer, legend=F)

sar_map_odc <- rgb_img + 
  mapview(img1, layer.name = "img1_vh_odc", na.color =NA, alpha = alpha_layer, legend=F) +
  mapview(img2, layer.name = "img2_vh_odc", na.color =NA, alpha = alpha_layer, legend=F)



sync_map <- sync(sar_map_gee, sar_map_odc)
save_map_sync(map =  sync_map, name = "SAR_roi_sync", path_out = path_html)


# MC: Diferencia Directa --------------------------------------------------


type_dif <-  "DD"
name_image <- paste0(type_dif, suffix_null)
r_old <- raster(paste0("results/", SITIO,"/", name_image,".tif"))
m_old <- mapview(r_old, na.color =NA, alpha = alpha_layer, legend=F,
                 layer.name = paste0(name_image, "_GEE"))



name_new <- paste0(type_dif, suffix)
r_new <- raster(paste0("results/", SITIO,"/", name_new,".tif"))
m_new <- mapview(r_new, na.color =NA, alpha = alpha_layer, legend=F, 
                 layer.name = name_new)

sar_map_old <- rgb_img + m_old
sar_map_new <- rgb_img + m_new


sync_map <- sync(sar_map_old, sar_map_new)
save_map_sync(map =  sync_map, 
              name = paste0(type_dif, "_roi_sync"),
              path_out = path_html)






# Log Ratio ---------------------------------------------------------------
type_dif <-  "LR"
name_image <- paste0(type_dif, suffix_null)
r_old <- raster(paste0("results/", SITIO,"/", name_image,".tif"))
m_old <- mapview(r_old, na.color =NA, alpha = alpha_layer, legend=F,
               layer.name = paste0(name_image, "_GEE"))



name_new <- paste0(type_dif, suffix)
r_new <- raster(paste0("results/", SITIO,"/", name_new,".tif"))
m_new <- mapview(r_new, na.color =NA, alpha = alpha_layer, legend=F, 
                   layer.name = name_new)

sar_map_old <- rgb_img + m_old
sar_map_new <- rgb_img + m_new


sync_map <- sync(sar_map_old, sar_map_new)
save_map_sync(map =  sync_map, 
              name = paste0(type_dif, "_roi_sync"),
              path_out = path_html)




# Relación de verosimilitud logarítmica (LLR) -----------------------------

type_dif <-  "LLR"
name_image <- paste0(type_dif, suffix_null)
r_old <- raster(paste0("results/", SITIO,"/", name_image,".tif"))
m_old <- mapview(r_old, na.color =NA, alpha = alpha_layer, legend=F,
                 layer.name = paste0(name_image, "_GEE"))



name_new <- paste0(type_dif, suffix)
r_new <- raster(paste0("results/", SITIO,"/", name_new,".tif"))
m_new <- mapview(r_new, na.color =NA, alpha = alpha_layer, legend=F, 
                 layer.name = name_new)

sar_map_old <- rgb_img + m_old
sar_map_new <- rgb_img + m_new


sync_map <- sync(sar_map_old, sar_map_new)
save_map_sync(map =  sync_map, 
              name = paste0(type_dif, "_roi_sync"),
              path_out = path_html)


# Enhanced Difference Image (EDI) -----------------------------------------


type_dif <-  "EDI"
name_image <- paste0(type_dif, suffix_null)
r_old <- raster(paste0("results/", SITIO,"/", name_image,".tif"))
m_old <- mapview(r_old, na.color =NA, alpha = alpha_layer, legend=F,
                 layer.name = paste0(name_image, "_GEE"))



name_new <- paste0(type_dif, suffix)
r_new <- raster(paste0("results/", SITIO,"/", name_new,".tif"))
m_new <- mapview(r_new, na.color =NA, alpha = alpha_layer, legend=F, 
                 layer.name = name_new)

sar_map_old <- rgb_img + m_old
sar_map_new <- rgb_img + m_new


sync_map <- sync(sar_map_old, sar_map_new)
save_map_sync(map =  sync_map, 
              name = paste0(type_dif, "_roi_sync"),
              path_out = path_html)


# Log Mean Ratio ----------------------------------------------------------


type_dif <-  "LMR"
name_image <- paste0(type_dif, suffix_null)
r_old <- raster(paste0("results/", SITIO,"/", name_image,".tif"))
m_old <- mapview(r_old, na.color =NA, alpha = alpha_layer, legend=F,
                 layer.name = paste0(name_image, "_GEE"))



name_new <- paste0(type_dif, suffix)
r_new <- raster(paste0("results/", SITIO,"/", name_new,".tif"))
m_new <- mapview(r_new, na.color =NA, alpha = alpha_layer, legend=F, 
                 layer.name = name_new)

sar_map_old <- rgb_img + m_old
sar_map_new <- rgb_img + m_new


sync_map <- sync(sar_map_old, sar_map_new)
save_map_sync(map =  sync_map, 
              name = paste0(type_dif, "_roi_sync"),
              path_out = path_html)


# MC: Triangular Threshold Segmentation (Douglas-Peucker) -----------------
type_dif_main <- "TTS"

# OLD
# A
type_dif_sec <- "DD"
type_dif <- paste0(type_dif_main, "_", type_dif_sec)
name_image <- paste0(type_dif, suffix_null)
r_old <- raster(paste0("results/", SITIO,"/", name_image,".tif"))
m_old_a <- mapview(r_old, na.color =NA, alpha = alpha_layer, legend=F, 
                   col.regions ="magenta",
                   layer.name = paste0(name_image, "_GEE"))

# B
type_dif_sec <- "EDI"
type_dif <- paste0(type_dif_main, "_", type_dif_sec)
name_image <- paste0(type_dif, suffix_null)
r_old <- raster(paste0("results/", SITIO,"/", name_image,".tif"))
m_old_b <- mapview(r_old, na.color =NA, alpha = alpha_layer, legend=F, 
                   col.regions ="cyan",
                   layer.name = paste0(name_image, "_GEE"))

# C
type_dif_sec <- "LMR"
type_dif <- paste0(type_dif_main, "_", type_dif_sec)
name_image <- paste0(type_dif, suffix_null)
r_old <- raster(paste0("results/", SITIO,"/", name_image,".tif"))
m_old_c <- mapview(r_old, na.color =NA, alpha = alpha_layer, legend=F, 
                   col.regions ="navy",
                   layer.name = paste0(name_image, "_GEE"))


# NEW

#A

type_dif_sec <- "DD"
type_dif <- paste0(type_dif_main, "_", type_dif_sec)
name_image <- paste0(type_dif, suffix)
r_new <- raster(paste0("results/", SITIO,"/", name_image,".tif"))
m_new_a <- mapview(r_new, na.color =NA, alpha = alpha_layer, legend=F, 
                   col.regions ="magenta",
                   layer.name = name_image)

# B
type_dif_sec <- "EDI"
type_dif <- paste0(type_dif_main, "_", type_dif_sec)
name_image <- paste0(type_dif, suffix)
r_new <- raster(paste0("results/", SITIO,"/", name_image,".tif"))
m_new_b <- mapview(r_new, na.color =NA, alpha = alpha_layer, legend=F, 
                   col.regions ="cyan",
                   layer.name =name_image)

# C
type_dif_sec <- "LMR"
type_dif <- paste0(type_dif_main, "_", type_dif_sec)
name_image <- paste0(type_dif, suffix)
r_new <- raster(paste0("results/", SITIO,"/", name_image,".tif"))
m_new_c <- mapview(r_new, na.color =NA, alpha = alpha_layer, legend=F, 
                   col.regions ="navy",
                   layer.name =name_image)

sar_map_old <- rgb_img + m_old_c + m_old_a + m_old_b 
sar_map_new <- rgb_img + m_new_c + m_new_a + m_new_b 


sync_map <- sync(sar_map_old, sar_map_new)
save_map_sync(map =  sync_map, 
              name = paste0(type_dif_main, "_roi_sync"),
              path_out = path_html)



#  PCA K-Means ------------------------------------------------------------

pcak_edi <- raster("results/ST_001/PCAK_EDI.tif")

map_pcak_edi <- rgb_img + mapview(pcak_edi, na.color = NA, col.region="orange") 
make_map_web(map = map_pcak_edi, name = "PCAK_EDI_roi", path_out = "results/ST_001/")



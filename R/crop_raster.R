# Antecedentes ------------------------------------------------------------

# objetivos: Crop images Opens Data Cube
# fecha: 16-06-2023

# Recursos ----------------------------------------------------------------

# Lectura de Funciones
source("R/libraries.R")
source("R/fnc_spatials.R")


# Parámetros Generales ----------------------------------------------------

folder_roi <-  "data/turberas/shapes/"
path_roi <- paste0(folder_roi, "changes.shp")



# Región of Interest -------------------------------------------------------


roi <-  read_shps(path_file = path_roi, crs_dest = 4326)
roi <- roi %>% mutate(ID = sprintf("%03d", as.numeric(ID)))
mapview(roi)

ID_area <- "021"
roi_1 <- filter(roi, ID == ID_area)
region <- get_bbox_region(roi_1, d_buffer = 500)
mapview(region)



# Read images -------------------------------------------------------------

SITIO <- "ST_021"
path_odc <- paste0("data/turberas/tif/", SITIO)

library(purrr)
library(terra)

files_tif <- list.files(path = path_odc, pattern = "*gamma0.tif",
                        full.names = T) %>% 
  map(rast) %>% 
  map(~crop(., y = vect(region)))
  

# Guadar los resultados ---------------------------------------------------

names_tif <- list.files(path = path_odc, pattern = "*gamma0.tif") %>% 
  gsub("_gamma0", replacement = "", x = .) %>% 
  paste0(path_odc, "/", .)

names(files_tif) <- names_tif

names_tif %>% 
  map(~writeRaster(files_tif[[.]], filename= ., overwrite=FALSE))

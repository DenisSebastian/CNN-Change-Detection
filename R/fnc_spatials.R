
read_shps <-  function(path_file, crs_dest = 4326){
  shp <- sf::st_read(path_file, quiet = T) %>% 
    sf::st_transform(crs = crs_dest)
  return(shp)
}

# read_sf_file <- function(file_path, crs_work = 32719) {
#   file <- sf::read_sf(file_path, quiet = T) %>% 
#     st_transform(crs_work) 
#   return(file)
# }

# save_sf_shp <- function(object_sf, region, year, name ="ILN", 
#                         out_path = "results/shapes", crs_work = 32719) {
#   make_dir(out_path)
#   out = paste0(out_path, "/R", region, "_", name, "_", year, ".shp")
#   file <- object_sf %>% 
#     st_transform(crs_work) %>% 
#     st_write(out, delete_dsn = TRUE)
# 
#   return(file)
# }
# 
# save_sf_rds <- function(object_sf, region, year, name ="ILN", 
#                         out_path = "results/rds", crs_work = 32719) {
#   make_dir(out_path)
#   out = paste0(out_path, "/R", region, "_", name, "_", year, ".rds")
#   file <- object_sf %>% 
#     st_transform(crs_work) %>% 
#     saveRDS(out)
#   return(file)
# }




# Trasformar CRS ----------------------------------------------------------

# transform_crs <- function(sf_object, crs_dest = 4326){
#   sf_object <-  sf::st_transform(sf_object, crs = crs_dest)
#   return(sf_object)
# }
#   




read_insumos_reg <-  function(region, crs_dest = 4326, 
                              ruta = "../insumos/regiones/Regiones_Chile.rds"){
  reg <- read_rds_file(ruta) %>% 
    dplyr::filter(REGION == region) %>% 
    sf::st_transform(crs = crs_dest)
  return(reg)
}

read_insumos_com <-  function(region, crs_dest = 4326, 
                              ruta = "../insumos/comunas/Comunas_Chile.rds"){
  reg <- read_rds_file(ruta) %>% 
    dplyr::filter(REGION == region) %>% 
    sf::st_transform(crs = crs_dest)
  return(reg)
}


read_insumos_mz <-  function(region, crs_dest = 4326, 
                             ruta = "../insumos/manzanas/"){
  path_file <- paste0(ruta, "MZ_REGION_", region, ".rds")
  mz <- read_rds_file(path_file) %>% 
    sf::st_transform(crs = crs_dest)
  
  return(mz)
  
}

extract_vals <- function(pol_sf, image, name_col, fun = "mean"){
  pol_sf <- pol_sf %>%
    dplyr::mutate({{name_col}} :=
      exactextractr::exact_extract(x = image, y = .,
                                   fun = fun))
  
  return(pol_sf)
  
}



make_box <-  function(x, d_buffer = 1000){
  x <- st_buffer(x, dist = d_buffer) %>% 
    st_bbox() %>% 
    st_as_sfc()
  return(x)
} 


get_bbox_region <- function(roi, d_buffer = 1000 ){
  geom <- roi %>% 
    st_transform(32719) %>% 
    st_buffer(dist = d_buffer) %>% 
    st_transform(4326) %>% 
    dplyr::select(geometry) %>%
    st_bbox() %>%
    st_as_sfc()
  return(geom)
}

get_manzanas <- function(path_mz){
  mz <- read_sf(path_mz) %>%
    st_transform(4326)
  return(mz)
}

  

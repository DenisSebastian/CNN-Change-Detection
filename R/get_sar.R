# Antecedentes ------------------------------------------------------------

# objetivos: Códido de Obtenga Información SAR
# fecha: 30-07-2023

# Recursos ----------------------------------------------------------------

# Lectura de Funciones
source("R/libraries.R")
source("R/fnc_spatials.R")
source("R/fnc_gee.R")
ee_Initialize('denis.berroeta@gmail.com', drive = TRUE)




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
mapview(region)+mapview(img1)



ee_roi <- region %>% 
  sf_as_ee()


# save bpc sitio
path_out_box <- paste0("data/turberas/shapes/ST_",ID_area)
make_dir(path_out_box)
box_name <- paste0(path_out_box, "/ST_", ID_area, "_box_500.shp")
st_write(region, box_name, delete_dsn= T)



# Disponibilidad de Imagenes ----------------------------------------------

sentinel1 <- ee$ImageCollection('COPERNICUS/S1_GRD')$
  filterBounds(ee_roi)
  

  # Filter by metadata properties.
VV <- sentinel1$
  filter(ee$Filter$eq("instrumentMode", "IW"))$
  filter(ee$Filter$listContains("transmitterReceiverPolarisation", "VV"))$
  select("VV")

df_VV <- ee_get_date_ic(VV)%>%
  arrange(time_start)
df_VV



VH <- sentinel1$
  filter(ee$Filter$eq("instrumentMode", "IW"))$
  filter(ee$Filter$listContains("transmitterReceiverPolarisation", "VH"))$
  select("VH")
df_VH <- ee_get_date_ic(VH)%>%
  arrange(time_start)

#



# Analizar Diferencias ----------------------------------------------------




# //Filter by date
beforeVV <- VV$filterDate('2017-01-01', '2017-04-30')$
  median()$clip(ee_roi) # median
  # first()$clip(ee_roi) # first
afterVV <- VV$filterDate('2023-01-01', '2023-06-30')$
  median()$clip(ee_roi) # median
  # limit(1, 'system:time_start')$first()$clip(ee_roi) # last image

beforeVH <- VH$filterDate('2017-01-01', '2017-06-30')$
  median()$clip(ee_roi) # median
  # first()$clip(ee_roi) #first
afterVH <- VH$filterDate('2023-01-01', '2023-06-30')$
  median()$clip(ee_roi) # median
  # limit(1, 'system:time_start')$first()$clip(ee_roi)# last image

path_out_tif <- paste0("data/turberas/tif/ST_", ID_area)
make_dir(path_out_tif)

dl_VV_2017 <- dl_image(image_ee = beforeVV, roi = region, n_sat = "S1_GRD", 
                  year = "2017", description_d = "SAR_VV_1S", 
                  folder_name_d =  paste0("ST_",ID_area), 
                  scale_r = 10, path_out = path_out_tif)

dl_VV_2023 <- dl_image(image_ee = afterVV, roi = region, n_sat = "S1_GRD", 
                  year = "2023", description_d = "SAR_VV_1S", 
                  folder_name_d =  paste0("ST_",ID_area), 
                  scale_r = 10, path_out = path_out_tif)

dl_VH_2017 <- dl_image(image_ee = beforeVH, roi = region, n_sat = "S1_GRD", 
                  year = "2017", description_d = "SAR_VH_1S", 
                  folder_name_d =  paste0("ST_",ID_area), 
                  scale_r = 10, path_out = path_out_tif)

dl_VH_2023 <- dl_image(image_ee = afterVH, roi = region, n_sat = "S1_GRD", 
                  year = "2023", description_d = "SAR_VH_1S", 
                  folder_name_d =  paste0("ST_",ID_area), 
                  scale_r = 10, path_out = path_out_tif)


# Display as a composite of polarization and backscattering characteristics.
Map$centerObject(eeObject = ee_roi,zoom = 14) 
Map$addLayer(beforeVV,list(min = -15, max =  0),"beforeVV")+
Map$addLayer(afterVV,list(min = -15, max =  0),"afterVV")+
Map$addLayer(beforeVH,list(min = -25, max =  0),"beforeVH")+
Map$addLayer(afterVH,list(min = -25, max =  0),"afterVH")


# composite
Map$centerObject(eeObject = ee_roi,zoom = 14) 
Map$addLayer(beforeVH$addBands(afterVH)$addBands(beforeVH),
             list(min = -25, max =  -8),"Composite")



# Apply filter to reduce speckle
SMOOTHING_RADIUS <- 10
beforeVV_filtered <- beforeVV$focal_mean(SMOOTHING_RADIUS, 'circle', 'meters'); 
beforeVH_filtered <- beforeVH$focal_mean(SMOOTHING_RADIUS, 'circle', 'meters');
afterVV_filtered <- afterVV$focal_mean(SMOOTHING_RADIUS, 'circle', 'meters'); 
afterVH_filtered <- afterVH$focal_mean(SMOOTHING_RADIUS, 'circle', 'meters');


Map$centerObject(eeObject = ee_roi,zoom = 14) 
Map$addLayer(beforeVV_filtered,list(min = -15, max =  0),"beforeVV")+
  Map$addLayer(afterVV_filtered,list(min = -15, max =  0),"afterVV")+
  Map$addLayer(beforeVH_filtered,list(min = -25, max =  0),"beforeVH")+
  Map$addLayer(afterVH_filtered,list(min = -25, max =  0),"afterVH")


# // Calculate difference between before and after
differenceVH <- afterVH_filtered$divide(beforeVH_filtered)
Map$centerObject(eeObject = ee_roi,zoom = 14) 
Map$addLayer(differenceVH,list(min = 0, max =  2),"difference VH filtered")

differenceVV <- afterVV_filtered$divide(beforeVV_filtered)
Map$centerObject(eeObject = ee_roi,zoom = 14) 
Map$addLayer(differenceVV,list(min = 0, max =  2),"difference VH filtered")

# //Apply Threshold
DIFF_UPPER_THRESHOLD = 1.15
differenceVH_thresholded = differenceVH$gt(DIFF_UPPER_THRESHOLD)
Map$addLayer(differenceVH_thresholded$updateMask(differenceVH_thresholded),
             list(palette="0000FF"),"Changed areas - blue")


DIFF_UPPER_THRESHOLD = 1.5
differenceVV_thresholded = differenceVV$gt(DIFF_UPPER_THRESHOLD)
Map$addLayer(differenceVV_thresholded$updateMask(differenceVV_thresholded),
             list(palette="0000FF"),"Changed areas - blue")







vhAscending <- vh$filter(ee$Filter$eq("orbitProperties_pass", "ASCENDING"))
vhDescending <- vh$filter(ee$Filter$eq("orbitProperties_pass", "DESCENDING"))

# Create a composite from means at different polarizations and look angles.
VH_Ascending_mean <- vhAscending$select("VH")$mean()
VV_Ascending_Descending_mean <- vhAscending$select("VV") %>%
  ee$ImageCollection$merge(vhDescending$select("VV")) %>%
  ee$ImageCollection$mean()

VH_Descending_mean <- vhDescending$select("VH")$mean()

composite <- ee$Image$cat(list(
  VH_Ascending_mean,
  VV_Ascending_Descending_mean,
  VH_Descending_mean
))$focal_median()

# Display as a composite of polarization and backscattering characteristics.
Map$centerObject(eeObject = sar_1,zoom = 8) 
Map$addLayer(
  composite,
  list(min = c(-25, -20, -25), max = c(0, 10, 0)),
  "composite"
)




df_disponible <- ee_get_date_ic(dataset)%>%
  arrange(time_start)# ordenar por fecha

df_disponible
head(df_disponible)
summary(df_disponible)

escena <- df_disponible$id[1] # selección de primera
escena

VH_Ascending_mean <- vhAscending$select("VH")

viz = list(min = 0, max = 0.7, bands = c('B4','B3','B2'), gamma = 1.75)
sar_1 <- ee$Image(escena) $
  filter(ee$Filter$listContains('transmitterReceiverPolarisation', 'VV'))
Map$centerObject(eeObject = sar_1,zoom = 8) 
Map$addLayer(eeObject = landsat,visParams = viz)




# Parámetros de Visualización ---------------------------------------------
pal_green <- colorRampPalette(c("white", "yellow", "green","springgreen4", "darkgreen"))( 200 )
viz_ndvi<- list(min=-1, max=1, palette=pal_green)

pal_bin <- c('white', 'green')
viz_bin<- list(min=0, max=1, palette=pal_bin)
colors_icul <- c("#AFECCF","#76DDAB","#66C266","#3B753D","#004529")

pal_temp <- c("#FFFF00","#F5CC00","#E69900","#F56200","#FF0000")


# Periodos de Estudio -----------------------------------------------------
year = "2023"

# Verano: inicia el 21 de junio y finaliza el 23 de septiembre.
periodos_ver <- c("2023-04-10", "2023-06-10")
periodos_ver_date <- periodos_ver

# Invierno: inicia el 21 de diciembre y finaliza el 20 de marzo.
periodos_inv <- c("2022-11-21", "2023-02-10")
periodos_inv_date <- periodos_inv

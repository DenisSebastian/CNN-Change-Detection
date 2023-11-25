# Cargar la biblioteca
library(raster)


# Recursos ----------------------------------------------------------------
source("R/fnc_simulitud.R")


img_gt <- raster("data/samples/tif/1_3.tif")
img_eval <-  raster("results/dd_clean.tif")
img_eval <-  raster("results/PCAK_adj.tif")

# img_eval <- img_eval*255



simDICE <- Dice(raster1 = img_eval,raster2 =img_gt)
print(simDICE)

simJaccard<- Jaccard(raster1 = img_eval,raster2 =img_gt)
print(simJaccard)

similitary_visual(image_eval = img_eval,ground_truth =img_gt)






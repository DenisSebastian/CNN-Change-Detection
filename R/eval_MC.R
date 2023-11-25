
# Métodos Convencionales



# Librerias ---------------------------------------------------------------
library(raster)

# Recursos ----------------------------------------------------------------


source("R/DI.R")
source("R/graphics.R")
source("R/functions.R")
source("R/utils_image.R")
source("R/Douglas-Peucker.R")
source("R/log_mean_ratio.R")


palette_grey <- grey(1:100/100)
palette_rainbow <- rainbow(100)



# Insumos -----------------------------------------------------------------

img1 <- raster("data/samples/tif/1_1.tif")
img2 <- raster("data/samples/tif/1_2.tif")
img3 <- raster("data/samples/tif/1_3.tif")



# plot(img1, col=palette_grey)
# plot(img2, col=palette_grey)
plot(img3, col=palette_grey)
# 

# Diferencia Directa (DD) -------------------------------------------------


before_img <- as.matrix(img1)
image_after <- as.matrix(img2)
dd <-  diff_image(image_before = before_img, image_after = image_after)
dd_raster <- matrix(data = dd, ncol = ncol(img1), 
                          nrow = nrow(img1)) %>% raster()
plot(dd_raster, col=palette_grey)
# write_img(image = dd_raster, name = "DDI")
# tif2png("results/DDI.tif")


plot_raster_g(dd_raster, title_g ="Diferencia Directa (DD)", 
              palette_col = palette_grey)


hist_raster(dd_raster, title_g = "Histograma de Diferencia Directa (DD)", bins = 50)

library(terra)
dd_rast <- rast(dd_raster)


plot(dd_rast, col=palette_grey)
d_lr2 <- app(dd_rast, fun = th_gte_bin, th_gte=100)
plot(d_lr2, col=palette_grey)

img_padding <- padding(dd_rast, padding_size = 1)
plot(img_padding, col=palette_grey)
bin_dd <- app(img_padding, fun = th_gte_bin, th_gte=120)
plot(bin_dd, col=palette_grey)
bin_dd <- filter_noise_pad(bin_dd, vmax_255 = T)
plot(bin_dd, col=palette_grey)
plot_raster_g(bin_dd, title_g ="Diferencia Directa (DD)", 
              palette_col = palette_grey)

write_img(image = bin_dd, name =  "dd_clean")



plot(dd_rast, col=palette_grey)
img_padding <- padding_mean(dd_rast, padding_size = 1, vmax_255 = F)
plot(img_padding, col=palette_grey)
write_img(image = img_padding, name =  "dd_softly")
tif2png("results/dd_softly.tif")

dd_bin <- app(img_padding, fun = th_gte_bin, th_gte=100)
plot(dd_bin, col=palette_grey)
dd_pad <- filter_noise_pad(dd_bin, nb = 8)
img_padding <- padding_mean(dd_pad, padding_size = 1, n_matrix= 3)
plot(img_padding, col=palette_grey)
dd_bin <- app(img_padding, fun = th_gte_bin, th_gte=70)
plot(dd_bin, col=palette_grey)

write_img(image = dd_bin, name =  "dd_clean2")


# Relación logarítmica (LR) (Dekker, 1998) --------------------------------

d_lr <-  log_ratio(img1, img2)
# write_img(d_lr, name = "LR")

plot_raster_g(d_lr,  palette_col = palette_grey,
              title_g ="Relación Logarítmica (LR)")



hist_raster(d_lr, title_g = "Histograma de Relación Logarítmica (LR)", bins = 100)



# Relación de verosimilitud logarítmica (LLR) (Cui et al., 2019) ----------



# Define the radius of the neighborhood
r <- 9

# Define the kernel
kernel <- matrix(1, nrow=r, ncol=r)


d_llr <-  log_likelihood_ratio(img1, img2, kernel = kernel)


# write_img(d_llr, name = "LLR")


plot_raster_g(d_llr, title_g ="Relación de Verosimilitud Logarítmica (LLR)",
              palette_col = palette_grey)

hist_raster(img_mod, title_g = "Histograma Relación de Verosimilitud Logarítmica (LLR)")

# Definción de Umbral

img_mod =scaleRaster(d_llr)

img_matrix = img_mod %>% as.matrix()

dp_clases <- triangular_threshold_segmentation(img_matrix)
umbral = dp_clases$threshold
menor <- dp_clases$leq_th %>% raster()
mayor <- dp_clases$gt_th %>% raster()

# plot(menor, col=palette_grey, main = "DDI bajo el umbral triangular")
plot(mayor, col=palette_grey, main = "DDI sobre el umbral triangular")

plot(img_mod, col=palette_grey)
bin_llr <- filter_noise_pad(img_mod, vmax_255 = T, nb = 2)
bin_llr <-  rast(bin_llr)
bin_llr <- app(bin_llr, fun = th_gte_bin, th_gte=65)
plot(bin_llr, col=palette_grey)

write_img(image = d_llr, name =  "LLR_raw")
write_img(image = mayor, name =  "LLR_th_t")
write_img(image = bin_llr, name =  "LLR_th_hand")



# Enhanced difference image (EDI) -----------------------------------------

  
d_lr <- calc(d_lr, fun = inf_na)
edi <-  enhanced_difference(d_lr, d_llr)




# write_img(edi, name = "EDI")

plot_raster_g(edi,  palette_col = palette_grey,
              title_g ="Enhanced difference image (EDI)")




hist_raster(edi, title_g = "Histograma Enhanced difference image (EDI)")




# ed_mod <- calc(ed, fun = keep_outliers)
plot_raster_g(dd_raster, title_g ="Enhanced difference image (EDI) - Ouliers")
dd_raster <-  scaleRaster(dd_raster, max = 1)
d_llr_sc <-  scaleRaster(d_llr, max = 1)
dd_matrix <- dd_raster %>% as.matrix()
d_ll_matrix <- d_llr_sc %>% as.matrix()

edi <-  enhanced_difference(dd_matrix, d_ll_matrix)
edi <- edi %>% rast()

edi <-  scaleRaster(edi, max = 255)
plot(edi, col=palette_grey)

edi_padding <- padding_mean(edi, padding_size = 1, n_matrix= 3)
plot(edi_padding, col=palette_grey)

edi_bin <- app(edi_padding, fun = th_gte_bin, th_gte=20)
plot(edi_bin, col=palette_grey)
edi_bin <- filter_noise_pad(edi_bin, vmax_255 = T, nb = 8)
plot(edi_bin, col=palette_grey)


write_img(image = edi, name =  "EDI_raw")
write_img(image = edi_bin, name =  "EDI_adj")
tif2png("results/EDI_adj.tif")




## Triangular Threshold Segmentation

### Método Douglas-Peucker a imagen Diferencia Mejorada (EDI)


edi_mod = scaleRaster(edi)
edi_matrix = edi_mod %>% as.matrix()

dp_clases <- triangular_threshold_segmentation(edi_matrix)
umbral = dp_clases$threshold
menor <- dp_clases$leq_th %>% raster()
mayor <- dp_clases$gt_th %>% raster()


plot(menor, col=palette_grey, main = "EDI bajo el umbral triangular")
plot(mayor, col=palette_grey, main = "EDI sobre el umbral triangular")


### Método Douglas-Peucker a imagen Diferencia Directa (DDI)

dd_raster <-  scaleRaster(dd_raster, max = 255)
d_direct_matrix <- matrix(data = dd_raster, ncol = ncol(img1), 
                          nrow = nrow(img1), byrow = T)

dp_dd <- triangular_threshold_segmentation(d_direct_matrix)
umbral = dp_dd$threshold
menor <- dp_dd$leq_th %>% raster()
mayor <- dp_dd$gt_th %>% raster()
# write_img(menor, name = "DDI_TH_m")
# write_img(mayor, name = "DDI_TH_g")




plot(menor, col=palette_grey, main = "DDI bajo el umbral triangular")
plot(mayor, col=palette_grey, main = "DDI sobre el umbral triangular")



# Log Mean Ratio (LMR) ----------------------------------------------------



neighbourhood_size <- 5
lmr_matrix <- lmr_neighbourhood(img1, img2, neighbourhood_size)
lmr_raster <- raster(lmr_matrix)
plot_raster_g(lmr_raster, palette_col = palette_grey,
              title_g = "Log Mean Ratio (LMR) de ambas imágenes" )

hist_raster(lmr_raster, title_g = "Histograma de Log Mean Ratio (LMR) de ambas imágenes")




lmr_mod <- scaleRaster(lmr_raster, max = 255)


edi_matrix = lmr_mod %>% as.matrix()

dp_clases <- triangular_threshold_segmentation(edi_matrix)
umbral = dp_clases$threshold
menor <- dp_clases$leq_th %>% raster()
mayor <- dp_clases$gt_th %>% raster()

plot(menor, col=palette_grey, main = "EDI bajo el umbral triangular")
plot(mayor, col=palette_grey, main = "EDI sobre el umbral triangular")


# write_img(lmr_raster, name = "LMR")
# write_img(lmr_mod, name = "LMR_scaled")
tif2png("data/samples/tif/1_3.tif")


plot(lmr_mod, col=palette_grey)
lmr_mod <-  rast(lmr_mod)
lmr_bin <- app(lmr_mod, fun = th_gte_bin, th_gte=130)
plot(lmr_bin, col=palette_grey)

lmr_bin <- filter_noise_pad(lmr_bin, vmax_255 = T, nb = 7)
plot(lmr_bin, col=palette_grey)
write_img(lmr_bin, name = "LMR_adj")


# PCA k-means  ------------------------------------------------------------


library(magick)

# Ruta de la imagen PNG de entrada
png_file <- "results/pca_kmenas.png"

# Cargar la imagen PNG
image <- image_read(png_file)

# Ruta y nombre del archivo TIFF de salida
tiff_file <- "results/PCAK.tif"

# Guardar la imagen como TIFF
image_write(image, tiff_file)



#| label: fig-pPCAkmean
#| fig-cap: "Resultado  del PCA K-Means de la Imagen de Diferencias"
#| 
pca_kmenas <-  raster("results/PCAK.tif")

plot_raster_g(pca_kmenas, palette_col = palette_grey,
              title_g = "PCA K-Means de la Imagen de Diferencias" )



hist_raster(pca_kmenas, title_g = "pca_kmenas")


plot(pca_kmenas, col=palette_grey)
pca_kmenas <-  rast(pca_kmenas)
pca_kmenas <- padding_mean(pca_kmenas, padding_size = 1, n_matrix= 3)
plot(pca_kmenas, col=palette_grey)


pcam_bin <- app(pca_kmenas, fun = th_gte_bin, th_gte=90)
plot(pcam_bin, col=palette_grey)

pcam_bin <- filter_noise_pad(pcam_bin, vmax_255 = T, nb = 8)
plot(pcam_bin, col=palette_grey)
write_img(pcam_bin, name = "PCAK_adj")



# Multi-hierarchical FCM --------------------------------------------------
# https://github.com/xhwNobody/Change-Detection/blob/master/pre_pseudo_label_FCM.py

# https://github.com/facebookresearch/segment-anything
# https://book.geemap.org/chapters/06_data_analysis.html

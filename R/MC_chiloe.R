
# Métodos Convencionales

# Librerias ---------------------------------------------------------------
library(raster)
library(mapview)
# Recursos ----------------------------------------------------------------


source("R/DI.R")
source("R/graphics.R")
source("R/functions.R")
source("R/utils_image.R")
source("R/Douglas-Peucker.R")
source("R/log_mean_ratio.R")


palette_grey <- grey(1:100/100)
palette_rainbow <- rainbow(100)

SITIO <- "general"
suffix <- "_Chiloe"

# Insumos -----------------------------------------------------------------

# img1_vh <- raster(paste0("data/turberas/tif/", SITIO, "/S1_GRD_SAR_VH_1S_2017.tif"))
# img1_vv <- raster(paste0("data/turberas/tif/", SITIO, "/S1_GRD_SAR_VV_1S_2017.tif"))
# img2_vh <- raster(paste0("data/turberas/tif/",SITIO,  "/S1_GRD_SAR_VH_1S_2023.tif"))
# img2_vv <- raster(paste0("data/turberas/tif/",SITIO,  "/S1_GRD_SAR_VV_1S_2023.tif"))

# img1 <- img1_vh
# img2 <- img2_vh

img1 <- raster(paste0("data/Sentinel_1/general/MEDIAN_VH_2016_asf_s1_grd_gamma0-2.tif"))
img2 <-raster(paste0("data/Sentinel_1/general/MEDIAN_VH_2023_asf_s1_grd_gamma0-2.tif"))
# Calcula estadísticas

# Calcular la media y desviación estándar de ambas distribuciones
mean_raster1 <- mean(values(img1))
sd_raster1 <- sd(values(img1))

mean_raster2 <- mean(values(img2))
sd_raster2 <- sd(values(img2))

# Función para aplicar la normalización z-score
z_score <- function(x, mean, sd) {
  return (x - mean) / sd
}

# Aplicar normalización z-score a ambas distribuciones
normalized_stats_raster1 <- z_score(values(img1), mean_raster1, sd_raster1)
normalized_stats_raster2 <- z_score(values(img2), mean_raster1, mean_raster1)

values(img1) <- normalized_stats_raster1
values(img2) <- normalized_stats_raster2



# plot(img1, col=palette_grey)
# img1 <- focal(img1, fun = median, 
# w = matrix(1, nrow = 3, ncol = 3))
# plot(img1b, col=palette_grey)

# plot(img2, col=palette_grey)
# img2 <- focal(img2, fun = median, 
# w = matrix(1, nrow = 3, ncol = 3))


# ViewRGB -----------------------------------------------------------------
# st_2017 <- stack(x = paste0("results/", SITIO, "/", SITIO,"_2017_RGB_reproy.tif"))
# st_2022 <- stack(paste0("results/", SITIO, "/", SITIO,"_2021_RGB_reproy.tif"))
# m_2017 <- viewRGB(st_2017, r = 1, g = 2, b = 3)
# m_2022 <- viewRGB(st_2022, r = 1, g = 2, b = 3)


# Diferencia Directa (DD) -------------------------------------------------


before_img <- as.matrix(img1)
after_img <- as.matrix(img2)
dd <-  diff_image(image_before = before_img, image_after = after_img, 
                  is_abs = T)
dd_raster <- matrix(data = dd, ncol = ncol(img1), 
                    nrow = nrow(img1)) %>% 
  raster()

dd_r <- img1
values(dd_r) <- as.vector(dd_raster)
# mapview(dd_r)


# 
# sobel_filter <- matrix(c(
#   1, 2, 1,
#   2, 4, 2,
#   1, 2, 1),
#   nrow=3, ncol=3)
# #
# dd_r <- focal(dd_r, w=sobel_filter, fun=sum)
dd_r <- dd_r %>% scaleRaster(max = 1)

#
# sobel_filter <- matrix(c(
#   0, 1, 0,
#   0, -4, 0,
#   0, 1, 0),
#   nrow=3, ncol=3)
# # # 
# dd_r <- focal(dd_r, w=sobel_filter, fun=sum)
# dd_r <- 1-dd_r 
# mapview(dd_r)

name_image <- paste0("DD", suffix)
write_img(image = dd_r, name = name_image, 
          path_out = paste0("results/", SITIO))
tif2png(paste0("results/", SITIO,"/", name_image,".tif"))

# plot_raster_g(dd_r, title_g ="Diferencia Directa (DD)", 
#               palette_col = palette_grey)



# 
# 
# hist_raster(dd_r, title_g = "Histograma de Diferencia Directa (DD)",
#             bins = 50)
# 

# denoised DD
library(terra)

denoised_dd <- filter_noise_pad(dd_r, vmax_255 = T)
# mapview(denoised_dd)


# plot(denoised_dd, col=palette_grey)

name_image <- paste0( "DD_denoised", suffix)
write_img(image = denoised_dd, name = name_image, 
          path_out = paste0("results/", SITIO))
tif2png(paste0("results/", SITIO,"/", name_image,".tif"))





# plot(dd_rast, col=palette_grey)
# d_lr2 <- app(dd_rast, fun = th_gte_bin, th_gte=4)
# plot(d_lr2, col=palette_grey)
# 
# img_padding <- padding_mean(dd_rast, padding_size = 1)
# plot(img_padding, col=palette_grey)
# bin_dd <- app(img_padding, fun = th_gte_bin, th_gte=1.5)
# plot(bin_dd, col=palette_grey)
# bin_dd <- filter_noise_pad(bin_dd, vmax_255 = T)
# plot(bin_dd, col=palette_grey)
# plot_raster_g(bin_dd, title_g ="Diferencia Directa (DD)", 
#               palette_col = palette_grey)
# 
# write_img(image = bin_dd, name =  "dd_clean_ST_1")


# kernel mean padding
dd_r <- rast(dd_r)

# plot(dd_r, col=palette_grey)
img_padding <- padding_mean(dd_r, padding_size = 1, vmax_255 = F)
# plot(img_padding, col=palette_grey)


# img_padding <- img_padding %>% scaleRaster(max = 1)
name_image <- paste0("DD_pad_mean", suffix)
write_img(image = img_padding, name = name_image, 
          path_out = paste0("results/", SITIO))
tif2png(paste0("results/", SITIO,"/", name_image,".tif"))


# binario

# 
dd_bin <- app(img_padding, fun = th_gte_bin, th_gte=0.15)
# plot(dd_bin, col=palette_grey)

name_image <- paste0("DD_pad_bin", suffix)
write_img(image = img_padding, name = name_image, 
          path_out = paste0("results/", SITIO))
tif2png(paste0("results/", SITIO,"/", name_image,".tif"))

dd_bin_r <- dd_bin
dd_bin_r[dd_bin_r ==0] <- NA
# m_2017+m_2022+mapview(raster(dd_bin_r), na.color =NA, alpha = 0.5)
# 
# 
# 
# dd_pad <- filter_noise_pad(dd_bin, nb = 8)
# img_padding <- padding_mean(dd_pad, padding_size = 1, n_matrix= 3)
# plot(img_padding, col=palette_grey)
# dd_bin <- app(img_padding, fun = th_gte_bin, th_gte=70)
# plot(dd_bin, col=palette_grey)
# 
# write_img(image = dd_bin, name =  "dd_clean2")


# Relación logarítmica (LR) (Dekker, 1998) --------------------------------

d_lr <-  log_ratio(img1, img2)
# sobel_filter <- matrix(c(
#   1, 2, 1, 
#   2, 4, 2,
#   1, 2, 1), 
#   nrow=3, ncol=3)
# 
# d_lr <- focal(d_lr, w=sobel_filter, fun=sum)
# d_lr <- d_lr %>% scaleRaster(max = 1)

# write_img(d_lr, name = "LR")

# plot_raster_g(d_lr,  palette_col = palette_grey,
              # title_g ="Relación Logarítmica (LR)")

# d_lr <- scale(d_lr, center = 0)

# m_2017+m_2022+mapview(d_lr,  na.color =NA, alpha = 0.8)

# hist_raster(d_lr, title_g = "Histograma de Relación Logarítmica (LR)", bins = 100)



# plot(d_lr, col=palette_grey)

name_image <- paste0("LR", suffix)
write_img(image = d_lr, name = name_image, 
          path_out = paste0("results/", SITIO))
tif2png(paste0("results/", SITIO,"/", name_image,".tif"))




# Relación de verosimilitud logarítmica (LLR) (Cui et al., 2019) ----------



# Define the radius of the neighborhood
r <- 3

# Define the kernel
kernel <- matrix(1, nrow=r, ncol=r)


d_llr <-  log_likelihood_ratio(img1, img2, kernel = kernel)
d_llr <- d_llr %>% scaleRaster(max = 1)




# plot(d_llr, col=palette_grey)

name_image <- paste0("LLR", suffix)
write_img(image = d_llr, name = name_image, 
          path_out = paste0("results/", SITIO))
tif2png(paste0("results/", SITIO,"/", name_image,".tif"))

d_llr_cp <- d_llr
d_llr_cp[d_llr_cp < 0.05] <- NA
# m_2017+m_2022+mapview(d_llr_cp,  na.color =NA, alpha = 0.8)


# plot_raster_g(d_llr, title_g ="Relación de Verosimilitud Logarítmica (LLR)",
              # palette_col = palette_grey)

# hist_raster(d_llr, title_g = "Histograma Relación de Verosimilitud Logarítmica (LLR)")

# # Definción de Umbral
# 
# img_mod =scaleRaster(d_llr)
# 
# img_matrix = img_mod %>% as.matrix()
# 
# dp_clases <- triangular_threshold_segmentation(img_matrix)
# umbral = dp_clases$threshold
# menor <- dp_clases$leq_th %>% raster()
# mayor <- dp_clases$gt_th %>% raster()
# 
# # plot(menor, col=palette_grey, main = "DDI bajo el umbral triangular")
# plot(mayor, col=palette_grey, main = "DDI sobre el umbral triangular")
# 
# plot(img_mod, col=palette_grey)
# bin_llr <- filter_noise_pad(img_mod, vmax_255 = T, nb = 2)
# bin_llr <-  rast(bin_llr)
# bin_llr <- app(bin_llr, fun = th_gte_bin, th_gte=65)
# plot(bin_llr, col=palette_grey)
# 
# 
# 
# write_img(image = d_llr, name =  "LLR_raw")
# write_img(image = mayor, name =  "LLR_th_t")
# write_img(image = bin_llr, name =  "LLR_th_hand")



# Enhanced difference image (EDI) -----------------------------------------


# d_lr <- calc(d_lr, fun = inf_na)
edi <-  enhanced_difference(d_lr, d_llr)
edi <- edi %>% scaleRaster(max = 1)
plot(edi)
edi_cp <- edi
edi_cp[edi_cp <0.05] <- NA
# m_2017+m_2022+mapview(edi_cp,  na.color =NA, alpha = 0.8)



name_image <- paste0("EDI", suffix)
write_img(image = edi, name = name_image, 
          path_out = paste0("results/", SITIO))
tif2png(paste0("results/", SITIO,"/", name_image,".tif"))


# write_img(edi, name = "EDI")

# plot_raster_g(edi,  palette_col = palette_grey,
              # title_g ="Enhanced difference image (EDI)")




# hist_raster(edi, title_g = "Histograma Enhanced difference image (EDI)")




edi_outliers <- calc(edi, fun = keep_outliers)
plot_raster_g(edi_outliers, title_g ="Enhanced difference image (EDI) - Ouliers")

name_image <- paste0("EDI_Outliers", suffix)
write_img(image = edi_outliers, name = name_image, 
          path_out = paste0("results/", SITIO))
tif2png(paste0("results/", SITIO,"/", name_image,".tif"))
# m_2017+m_2022+mapview(edi_outliers,  na.color =NA, alpha = 0.8)





# 
# dd_raster <-  scaleRaster(dd_raster, max = 1)
# d_llr_sc <-  scaleRaster(d_llr, max = 1)
# dd_matrix <- dd_raster %>% as.matrix()
# d_ll_matrix <- d_llr_sc %>% as.matrix()
# 
# edi <-  enhanced_difference(dd_matrix, d_ll_matrix)
# edi <- edi %>% rast()
# 
# edi <-  scaleRaster(edi, max = 255)
# plot(edi, col=palette_grey)
# 
# edi_padding <- padding_mean(edi, padding_size = 1, n_matrix= 3)
# plot(edi_padding, col=palette_grey)
# 
# edi_bin <- app(edi_padding, fun = th_gte_bin, th_gte=20)
# plot(edi_bin, col=palette_grey)
# edi_bin <- filter_noise_pad(edi_bin, vmax_255 = T, nb = 8)
# plot(edi_bin, col=palette_grey)
# 
# 
# write_img(image = edi, name =  "EDI_raw")
# write_img(image = edi_bin, name =  "EDI_adj")
# tif2png("results/EDI_adj.tif")
# 



## Triangular Threshold Segmentation

### Método Douglas-Peucker a imagen Diferencia Mejorada (EDI)


edi_mod =  scaleRaster(edi, max = 255)
edi_matrix = edi_mod %>% as.matrix()

dp_clases <- triangular_threshold_segmentation(edi_matrix)
umbral_edi = dp_clases$threshold
menor_edi <- dp_clases$leq_th %>% raster()
mayor_edi <- dp_clases$gt_th %>% raster()


plot(menor_edi, col=palette_grey, main = "EDI bajo el umbral triangular")
plot(mayor_edi, col=palette_grey, main = "EDI sobre el umbral triangular")

dd_raster <-  scaleRaster(dd_r, max = 255)
d_direct_matrix <- matrix(data = dd_raster, ncol = ncol(img1), 
                          nrow = nrow(img1), byrow = T)

dp_dd <- triangular_threshold_segmentation(d_direct_matrix)
umbral_dd = dp_dd$threshold
menor_dd <- dp_dd$leq_th %>% raster()
mayor_dd <- dp_dd$gt_th %>% raster()

# plot(mayor_dd, col=palette_grey, main = "DD bajo el umbral triangular")
# plot(mayor_dd, col=palette_grey, main = "DD sobre el umbral triangular")
# write_img(menor, name = "DDI_TH_m")
# write_img(mayor, name = "DDI_TH_g")

mayor_edi <- setValues(img1, values(mayor_edi))
mayor_edi[mayor_edi == 0] <- NA
# m_2017+m_2022+mapview(mayor_edi, na.color =NA, alpha = 0.8)

name_image <- paste0("TTS_EDI", suffix)
write_img(image = mayor_edi, name = name_image, 
          path_out = paste0("results/", SITIO))
tif2png(paste0("results/", SITIO,"/", name_image,".tif"))

mayor_dd <- setValues(img1, values(mayor_dd))
mayor_dd[mayor_dd == 0] <- NA
# m_2017+m_2022+mapview(mayor_dd,  na.color =NA )

name_image <- paste0("TTS_DD", suffix)
write_img(image = mayor_dd, name = name_image, 
          path_out = paste0("results/", SITIO))
tif2png(paste0("results/", SITIO,"/", name_image,".tif"))




# Log Mean Ratio (LMR) ----------------------------------------------------



neighbourhood_size <- 3
lmr_matrix <- lmr_neighbourhood(img1, img2, neighbourhood_size)

lmr_raster <- raster(lmr_matrix)


lmr_r <- setValues(img1, values(lmr_raster))
lmr_r <- scaleRaster(lmr_r, max = 1)
# mapview(lmr_r,  na.color =NA, alpha = 0.8)



# 
# plot_raster_g(lmr_r, palette_col = palette_grey,
#               title_g = "Log Mean Ratio (LMR) de ambas imágenes" )
# 
# hist_raster(lmr_r, title_g = "Histograma de Log Mean Ratio (LMR) de ambas imágenes")

name_image <- paste0("LMR", suffix)
write_img(image = lmr_r, name = name_image, 
          path_out = paste0("results/", SITIO))
tif2png(paste0("results/", SITIO,"/", name_image,".tif"))



lmr_cp <- lmr_r
lmr_cp[lmr_cp < 0.18] <- NA
# mapview(lmr_cp,  na.color =NA, alpha = 0.8)

name_image <- paste0("LMR_th", suffix)
write_img(image = lmr_cp, name = name_image, 
          path_out = paste0("results/", SITIO))
tif2png(paste0("results/", SITIO,"/", name_image,".tif"))



lmr_mod <- scaleRaster(lmr_r, max = 255)
edi_matrix = lmr_mod %>% as.matrix()

dp_clases <- triangular_threshold_segmentation(edi_matrix)
umbral = dp_clases$threshold
menor <- dp_clases$leq_th %>% raster()
mayor <- dp_clases$gt_th %>% raster()

# plot(menor, col=palette_grey, main = "LMR bajo el umbral triangular")
# plot(mayor, col=palette_grey, main = "LMR sobre el umbral triangular")


mayor_lmr <- setValues(img1, values(mayor))
mayor_lmr[mayor_lmr == 0] <- NA
# m_2017+m_2022+mapview(mayor_dd,  na.color =NA )


name_image <- paste0("TTS_LMR", suffix)
write_img(image = mayor_lmr, name = name_image, 
          path_out = paste0("results/", SITIO))
tif2png(paste0("results/", SITIO,"/", name_image,".tif"))



# plot(lmr_mod, col=palette_grey)
lmr_mod <-  rast(lmr_mod)
lmr_bin <- app(lmr_mod, fun = th_gte_bin, th_gte=50)
# plot(lmr_bin, col=palette_grey)

lmr_bin_cp <- lmr_bin
lmr_bin_cp[lmr_bin_cp == 0] <- NA
# m_2017 +m_2022+mapview(raster(lmr_bin_cp),  na.color =NA, alpha = 0.8)

name_image <- paste0("LMR_bin", suffix)
write_img(image = lmr_bin_cp, name = name_image, 
          path_out = paste0("results/", SITIO))
tif2png(paste0("results/", SITIO,"/", name_image,".tif"))


# lmr_bin <- filter_noise_pad(lmr_bin, vmax_255 = T, nb = 7)
# plot(lmr_bin, col=palette_grey)
# write_img(lmr_bin, name = "LMR_adj")


# PCA k-means  ------------------------------------------------------------

# 
# library(magick)
# 
# 
# # Ruta de la imagen PNG de entrada
# # png_file <- "results/ST_021/PCAK_1S_VV.png"
# 
# # Cargar la imagen PNG
# image <- image_read(png_file)
# 
# # Ruta y nombre del archivo TIFF de salida
# tiff_file <- "results/ST_021/PCAK_1S_VV.tif"
# 
# # Guardar la imagen como TIFF
# image_write(image, tiff_file)



#| label: fig-pPCAkmean
#| fig-cap: "Resultado  del PCA K-Means de la Imagen de Diferencias"
#| 
# pca_kmenas <-  raster("results/ST_021/PCAK_1S_VV.tif")
# 
# pca_kmenas_r <- setValues(img1, values(pca_kmenas))
# 
# 
# plot_raster_g(pca_kmenas_r, palette_col = palette_grey,
#               title_g = "PCA K-Means de la Imagen de Diferencias" )
# 
# pca_kmenas_r[pca_kmenas_r<1] <- NA
# mapview(pca_kmenas_r)
# 
# name_image <- paste0("PCAK", suffix)
# write_img(image = pca_kmenas_r, name = name_image, 
#           path_out = paste0("results/", SITIO))
# tif2png(paste0("results/", SITIO,"/", name_image,".tif"))
# 
# 
# 
# hist_raster(pca_kmenas, title_g = "pca_kmenas")
# 
# 
# plot(pca_kmenas, col=palette_grey)
# pca_kmenas <-  rast(pca_kmenas)
# pca_kmenas <- padding_mean(pca_kmenas, padding_size = 1, n_matrix= 3)
# plot(pca_kmenas, col=palette_grey)
# 
# 
# pcam_bin <- app(pca_kmenas, fun = th_gte_bin, th_gte=90)
# plot(pcam_bin, col=palette_grey)
# 
# 
# pcam_bin <- filter_noise_pad(pcam_bin, vmax_255 = T, nb = 8)
# plot(pcam_bin, col=palette_grey)
# 
# pcam_bin_cp <- img1
# values(pcam_bin_cp) <- as.vector(pcam_bin)
# mapview(pcam_bin_cp)
# pcam_bin_cp[pcam_bin_cp == 0] <- NA
# m_2017 +m_2022+mapview(pcam_bin_cp, na.color =NA, alpha = 0.8)

# write_img(pcam_bin, name = "PCAK_adj")




# Division ----------------------------------------------------------------

library(mapview)
composite <-  stack(img1, img2, img1)
# viewRGB(composite, r = 3, g = 2, b = 1)

differenceVH <- img1/img2
# mapview(differenceVH)

# differenceVH <- scaleRaster(differenceVH)


name_image <- "DIV_VH"

name_image <- paste0("DIV", suffix)
write_img(image = differenceVH, name = name_image, 
          path_out = paste0("results/", SITIO))
tif2png(paste0("results/", SITIO,"/", name_image,".tif"))




# differenceVH_cp <- differenceVH
# differenceVH_cp[differenceVH_cp < 1.4] <- NA
# mapview(differenceVH_cp,  na.color =NA, alpha = 0.8)




# Multi-hierarchical FCM --------------------------------------------------
# https://github.com/xhwNobody/Change-Detection/blob/master/pre_pseudo_label_FCM.py

# https://github.com/facebookresearch/segment-anything
# https://book.geemap.org/chapters/06_data_analysis.html

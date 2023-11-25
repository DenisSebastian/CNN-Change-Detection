gene_block <- function(img, block_sz=4, gene_block_vec=TRUE) {
  # Generate block vector or block pixel.
  # Args:
  # - img: row image
  # - block_sz: block size
  # - gene_block_vec: generate block vector (TRUE) or block pixel (FALSE)
  # Returns:
  # - block vector or block pixel

  vectors <- list()
  
  if (gene_block_vec) {
    # generate xd(y, x)
    n_y <- floor(dim(img)[1] / block_sz)
    n_x <- floor(dim(img)[2] / block_sz)
    
    for (y in 1:n_y) {
      for (x in 1:n_x) {
        vert_start <- (y - 1) * block_sz + 1
        vert_end <- vert_start + block_sz - 1
        horiz_start <- (x - 1) * block_sz + 1
        horiz_end <- horiz_start + block_sz - 1
        vec <- as.vector(img[vert_start:vert_end, horiz_start:horiz_end])
        vectors <- rbind(vectors, vec)
      }
    }
  } else {
    # generate xd(i, j)
    
    left_pad <- ceiling(block_sz / 2) - 1
    right_pad <- block_sz - ceiling(block_sz / 2)
    up_pad <- ceiling(block_sz / 2) - 1
    down_pad <- block_sz - ceiling(block_sz / 2)
    pad <- matrix(c(up_pad, down_pad, left_pad, right_pad), nrow=2)
    pad_img <- zero_pad(img, pad)  # pad image because the margin of image also need block vector
    
    n_y <- dim(img)[1]
    n_x <- dim(img)[2]
    
    for (y in (up_pad + 1):(up_pad + n_y)) {
      for (x in (left_pad + 1):(left_pad + n_x)) {
        vert_start <- y - ceiling(block_sz / 2) + 1
        vert_end <- vert_start + block_sz - 1
        horiz_start <- x - ceiling(block_sz / 2) + 1
        horiz_end <- horiz_start + block_sz - 1
        vec <- as.vector(pad_img[vert_start:vert_end, horiz_start:horiz_end])
        vectors[[length(vectors) + 1]] <- vec
      }
    }
  }
  res <-  do.call(rbind, vectors)
  
  return(res)
}



gene_change_map <- function(image_diff, feature_vectors, cluster_center) {
  img_dims <- dim(image_diff)
  
  f_dis <- apply(feature_vectors - cluster_center[,1], 2, function(x) sqrt(sum(x^2))) # distance between the first cluster center and every pixel
  s_dis <- apply(feature_vectors - cluster_center[,2], 2, function(x) sqrt(sum(x^2))) # distance between the second cluster center and every pixel
  
  f_mask <- f_dis < s_dis # the pixel is closer to the first cluster center
  f_mask <- matrix(f_mask, nrow = img_dims[1], ncol = img_dims[2])
  
  f_mean <- mean(image_diff[f_mask]) # average of first class's pixel
  s_mean <- mean(image_diff[!f_mask]) # average of second class's pixel
  
  # the cluster whose pixels have higher average value in the difference image is assigned as the wc class
  if (f_mean > s_mean) {
    image_diff[f_mask] <- 255
    image_diff[!f_mask] <- 0
  } else {
    image_diff[f_mask] <- 0
    image_diff[!f_mask] <- 255
  }
  
  return(image_diff)
}

diff_image <- function(image_before, image_after, is_abs = TRUE, is_multi_channel = FALSE) {
  dim_img <-  dim(image_before)
  img_diff <- as.matrix(before_img, dtype = "numeric") - as.matrix(image_after, dtype = "numeric")
  if (is_multi_channel) {
    img_diff <- sqrt(rowSums(img_diff^2))
  } else {
    if (is_abs) {
      img_diff <- abs(img_diff)
    }
  }
  return(matrix(img_diff, nrow=dim_img[1], ncol=dim_img[2]))
}


pca_k_means <- function(img_diff, block_size=4, eig_space_dim=3){
  # Generar vector de bloque.
  block_vectors <- gene_block(img_diff, block_sz=block_size,
                              gene_block_vec=TRUE) %>% t() 
  # número de vectores, shape: (H * H, (height * width) / (H * H))
  n_vec <- ncol(block_vectors)
  
  avg_vec <- apply(block_vectors, 1, mean) %>% matrix(nrow=length(block_vectors), ncol=1) # el vector promedio del conjunto
  vec_diff <- block_vectors - avg_vec
  cov_mat <- crossprod(vec_diff) / n_vec # la matriz de covarianza
  
  eig <- eigen(cov_mat) # obtener los valores y vectores propios de la matriz de covarianza
  
  # seleccionar S vectores propios para generar espacio vectorial
  eig_space <- eig$vectors[, 1:eig_space_dim] # shape: (H * H, S)
  
  # generar bloque para cada píxel
  block_img <- gene_block(img_diff, block_sz=block_size, gene_block_vec=FALSE) %>% t() # shape: (H * H, height * width)
  
  # el vector de características en la ubicación espacial (i, j)
  feature_vec <- eig_space %*% (block_img - avg_vec) # shape: (S, height * width)
  
  # generar dos centros de clúster usando K-means
  cluster_center <- kmeans(feature_vec, centers=2)$centers %>% t() # shape: (S, 2)
  
  # generar mapa de cambio
  change_map <- gene_change_map(img_diff, feature_vec, cluster_center)
  return(change_map)
}

library(png)
library(magick)
before_img <- readPNG("Dataset/PCAKmeans/forest_1986.png")[,,1:3]

library(imager)
before_img <- load.image("Dataset/PCAKmeans/forest_1986.png")[,,1:4]
after_img <- load.image("Dataset/PCAKmeans/forest_1992.png")[,,1:4]
eig_dim = 10
block_sz = 4

diff_img = diff_image(before_img, after_img, is_abs=T, is_multi_channel=T)
change_img = pca_k_means(diff_img, block_size=block_sz, eig_space_dim=eig_dim)
imageio.imwrite('results/PCAKmeans_forest.png', change_img)
r <- writePNG(diff_img)
plot(r)

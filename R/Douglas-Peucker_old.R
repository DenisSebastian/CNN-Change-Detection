library(imager)



# Funciones ---------------------------------------------------------------

# Algoritmo de Douglas-Peucker para reducir el número de puntos en una imagen binaria
dp <- function(img, epsilon) {
  # Obtener las coordenadas de los píxeles que son iguales a 1 (es decir, los puntos de la imagen)
  points <- which(img == 1, arr.ind = TRUE)
  
  # Aplicar el algoritmo de Douglas-Peucker
  dp_impl <- function(points, epsilon) {
    # Encontrar el punto más lejano al segmento que conecta los extremos
    dmax <- 0
    index <- 0
    n <- nrow(points)
    for (i in 2:(n-1)) {
      d <- abs((points[i,2] - points[1,2]) * (points[n,1] - points[1,1]) - (points[n,2] - points[1,2]) * (points[i,1] - points[1,1]))
      if (d > dmax) {
        index <- i
        dmax <- d
      }
    }
    
    # Si la distancia máxima es mayor que epsilon, dividir la línea y llamar recursivamente a la función para cada mitad
    if (dmax > epsilon) {
      r1 <- dp_impl(points[1:index,], epsilon)
      r2 <- dp_impl(points[index:n,], epsilon)
      result <- rbind(r1[-nrow(r1),], r2)
    } else {
      result <- points[c(1,n),]
    }
    
    return(result)
  }
  
  reduced_points <- dp_impl(points, epsilon)
  
  # Crear una matriz de ceros del mismo tamaño que la imagen original
  reduced_img <- matrix(0, nrow = nrow(img), ncol = ncol(img))
  
  # Establecer los píxeles correspondientes a los puntos seleccionados a 1
  for (i in 1:nrow(reduced_points)) {
    reduced_img[reduced_points[i,1], reduced_points[i,2]] <- 1
  }
  
  return(reduced_img)
}


# Cargar la imagen
img <- load.image("images/fotos_denis.jpg")
# img <- boats

# Convertir la imagen a escala de grises
gray_img <- grayscale(img)

# Seleccionar puntos representativos de la imagen
sample_points <- sample(as.numeric(gray_img), size = 500)

# Calcular el umbral triangular
t <- (min(sample_points) + max(sample_points) + 2 * mean(sample_points)) / 4

# Segmentar la imagen
segmented_img <- gray_img > t

# Mostrar la imagen binarizada
binary_img <- gray_img > t
plot(binary_img, main = "Imagen binarizada")

# Reducir el número de puntos utilizando el algoritmo de Douglas-Peucker
reduced_img <- dp(segmented_img, 5)

# Mostrar la imagen original y la imagen segmentada
par(mfrow=c(1,2))
plot(img, main = "Imagen original")
plot(reduced_img, main = "Imagen segmentada")

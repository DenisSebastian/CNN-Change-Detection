
from PIL import Image

# Abrir la imagen .tif con Pillow
with Image.open('Dataset/S1_sample/tif/1_2.tif') as img:
    # Guardar la imagen en formato .png con Pillow
    img.save('Dataset/S1_sample/png/1_2.png')





    
    

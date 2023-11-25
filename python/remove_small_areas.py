import osgeo.gdal
import numpy as np
import sys, os 
rootpath = os.path.join(os.getcwd(), 'python/')
sys.path.append(rootpath)

import imageio.v2 as imageio
import cv2  

image = imageio.imread("results/EDI_2.png")
image =np.array(image, dtype=np.uint8)


area_thresh =10

def remove_small_regions(mask, area_thresh, mode):
  import cv2 
  assert mode in ["holes", "islands"]
  mask_mod  =np.array(mask, dtype=np.uint8)
  correct_holes = mode == "holes"
  working_mask = (mask_mod).astype(np.uint8)
  n_labels, regions, stats, _ = cv2.connectedComponentsWithStats(working_mask, 8)
  sizes = stats[:, -1][1:]  # Row 0 is background label
  small_regions = [i + 1 for i, s in enumerate(sizes) if s < area_thresh]
  if len(small_regions) == 0:
      return mask, False
  fill_labels = [0] + small_regions
  if not correct_holes:
      fill_labels = [i for i in range(n_labels) if i not in fill_labels]
      # If every region is below threshold, keep largest
      if len(fill_labels) == 0:
          fill_labels = [int(np.argmax(sizes)) + 1]
  mask_fil = np.isin(regions, fill_labels)
  image = np.array(mask, dtype=np.uint8)
  image = cv2.cvtColor(image, cv2.COLOR_GRAY2RGB)
  image_new = np.where(mask_fil[..., None], 0, image)
  return image_new
  

image = imageio.imread("results/pca_kmenas.png")  
results = remove_small_regions(mask=image,area_thresh=4, mode="holes")
cv2.imwrite('results/remove_min_area.png', results)
  

mode= "islands"
correct_holes = mode == "holes"
working_mask = (correct_holes ^ image).astype(np.uint8)
n_labels, regions, stats, _ = cv2.connectedComponentsWithStats(working_mask, 8)
sizes = stats[:, -1][1:]  # Row 0 is background label
small_regions = [i + 1 for i, s in enumerate(sizes) if s < area_thresh]

      
fill_labels = [0] + small_regions
if not correct_holes:
  fill_labels = [i for i in range(n_labels) if i not in fill_labels]
  # If every region is below threshold, keep largest
  if len(fill_labels) == 0:
    fill_labels = [int(np.argmax(sizes)) + 1]
    
    
mask = np.isin(regions, fill_labels)

image = cv2.cvtColor(image, cv2.COLOR_GRAY2RGB)
r = np.where(mask[..., None], image, 0)
cv2.imwrite('results/remove_min_area.png', r)

import osgeo.gdal
import numpy as np
import sys, os 
rootpath = os.path.join(os.getcwd(), 'python/')
sys.path.append(rootpath)

from PCAKmeans.algorithm import pca_k_means
from PCAKmeans.algorithm import zero_pad
from PCAKmeans.util import diff_image
from PCAKmeans.util import zero_pad

import imageio.v2 as imageio

def main():
    before_img = imageio.imread("data/turberas/tif/ST_021/MEDIAN_VH_2016_asf_s1_grd.tif")
    after_img = imageio.imread("data/turberas/tif/ST_021/MEDIAN_VH_2023_asf_s1_grd.tif")
    eig_dim = 8
    block_sz = 3


    diff_img = diff_image(before_img, after_img, is_abs=True, is_multi_channel=False)
    # diff_img = imageio.imread("results/ST_003/EDI_1T.png")
    diff_img = np.array(diff_img, dtype=np.uint8)
    change_img = pca_k_means(diff_img, block_size=block_sz, eig_space_dim=eig_dim)
    imageio.imwrite('results/ST_021/PCAK_VH_ODC.png', diff_img)


print("FIN")

if __name__ == '__main__':
    main()

# def remove_small_regions(
#     mask: np.ndarray, area_thresh: float, mode: str
# ) -> Tuple[np.ndarray, bool]:
#     """
#     Removes small disconnected regions and holes in a mask. Returns the
#     mask and an indicator of if the mask has been modified.
#     """
#     import cv2  # type: ignore
# 
#     assert mode in ["holes", "islands"]
#     correct_holes = mode == "holes"
#     working_mask = (correct_holes ^ mask).astype(np.uint8)
#     n_labels, regions, stats, _ = cv2.connectedComponentsWithStats(working_mask, 8)
#     sizes = stats[:, -1][1:]  # Row 0 is background label
#     small_regions = [i + 1 for i, s in enumerate(sizes) if s < area_thresh]
#     if len(small_regions) == 0:
#         return mask, False
#     fill_labels = [0] + small_regions
#     if not correct_holes:
#         fill_labels = [i for i in range(n_labels) if i not in fill_labels]
#         # If every region is below threshold, keep largest
#         if len(fill_labels) == 0:
#             fill_labels = [int(np.argmax(sizes)) + 1]
#     mask = np.isin(regions, fill_labels)
#     return mask, True


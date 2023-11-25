import numpy as np
from sklearn.cluster import KMeans


def zero_pad(img, pad):
    """
     Pad with zeros all images of the data set X. The padding is applied to the height and width of an image.

    :param X: python numpy array of shape (m, n_H, n_W, n_C) representing a batch of m images
    :param pad: integer, amount of padding around each image on vertical and horizontal dimensions
    :return: padded image of shape (m, n_H + 2*pad, n_W + 2*pad, n_C)
    """

    img_pad = np.pad(img, ((pad[0, 0], pad[0, 1]), (pad[1, 0], pad[1, 1])), 'constant')

    return img_pad

def gene_block(img, block_sz=4, gene_block_vec=True):
    """
    Generate block vector or block pixel.
    :param img: row image
    :param block_sz: block size
    :param gene_block_vec: generate block vector(True) or block pixel(False)
    :return: block vector or block pixel
    """
    vectors = []
    if gene_block_vec:
        # generate xd(y, x)
        n_y = np.int64(img.shape[0] / block_sz)
        n_x = np.int64(img.shape[1] / block_sz)

        for y in range(n_y):
            for x in range(n_x):
                vert_start = y * block_sz
                vert_end = y * block_sz + block_sz
                horiz_start = x * block_sz
                horiz_end = x * block_sz + block_sz
                vec = img[vert_start:vert_end, horiz_start:horiz_end].flatten()
                vectors.append(vec)
    else:
        # generate xd(i, j)

        left_pad = np.int_(np.ceil(block_sz / 2)) - 1
        right_pad = block_sz - np.int_(np.ceil(block_sz / 2))
        up_pad = np.int_(np.ceil(block_sz / 2)) - 1
        down_pad = block_sz - np.int_(np.ceil(block_sz / 2))
        pad = np.array([[up_pad, down_pad], [left_pad, right_pad]])
        pad_img = zero_pad(img, pad)  # pad image because the margin of image also need block vector

        n_y = img.shape[0]
        n_x = img.shape[1]

        for y in range(up_pad, up_pad + n_y):
            for x in range(left_pad, left_pad + n_x):
                vert_start = y - np.int_(np.ceil(block_sz / 2)) + 1
                vert_end = y + block_sz - np.int_(np.ceil(block_sz / 2)) + 1
                horiz_start = x - np.int_(np.ceil(block_sz / 2)) + 1
                horiz_end = x + block_sz - np.int_(np.ceil(block_sz / 2)) + 1
                vec = pad_img[vert_start:vert_end, horiz_start:horiz_end].flatten()
                vectors.append(vec)
    return np.array(vectors)


def gene_change_map(image_diff, feature_vectors, cluster_center):
    """
    Generate change map.
    :param image_diff: the difference image
    :param feature_vectors: feature vectors
    :param cluster_center: cluster centers generated by k-means algorithm
    :return: change map
    """
    img_height, img_width = image_diff.shape
    f_dis = np.linalg.norm(feature_vectors - cluster_center[:, 0].reshape(-1, 1), axis=0,
                           keepdims=True)  # the dis between the first cluster center and every pixel

    s_dis = np.linalg.norm(feature_vectors - cluster_center[:, 1].reshape(-1, 1), axis=0,
                           keepdims=True)  # the dis between the second cluster center and every pixel

    f_mask = f_dis < s_dis  # the pixel is closer to the first cluster center
    f_mask = f_mask.reshape([img_height, img_width])

    f_mean = image_diff[f_mask].mean()  # average of first class's pixel
    s_mean = image_diff[f_mask == False].mean()  # average of second class's pixel

    # the cluster whose pixels have higher average value in the difference image is assigned as the wc class
    if f_mean > s_mean:
        image_diff[f_mask] = 255
        image_diff[f_mask == False] = 0
    else:
        image_diff[f_mask] = 0
        image_diff[f_mask == False] = 255
    return image_diff


def pca_k_means(img_diff, block_size=4, eig_space_dim=3):
    # generate block vector.
    block_vectors = gene_block(img_diff, block_sz=block_size,
                               gene_block_vec=True).T  # number of vectors, shape: (H * H, (height * width) / (H * H))
    n_vec = block_vectors.shape[1]

    avg_vec = np.mean(block_vectors, axis=1, keepdims=True)  # the average vector of the set
    vec_diff = block_vectors - avg_vec
    cov_mat = np.dot(vec_diff, vec_diff.T) / n_vec  # the covariance matrix

    eig_val, eig_vec = np.linalg.eig(cov_mat)  # get eigenvalue and eigenvector of covariance matrix

    # select S eigenvector for generate vector space
    eig_space = eig_vec[:, 0:eig_space_dim]  # shape: (H * H, S)

    # generate block for every pixel
    block_img = gene_block(img_diff, block_sz=block_size, gene_block_vec=False).T  # shape: (H * H, height * width)

    # the feature vector at spatial location (i, j)
    feature_vec = np.dot(eig_space.T, (block_img - avg_vec))  # shape: (S, height * width)

    # generate two cluster centers using K-means
    cluster_center = KMeans(n_clusters=2).fit(feature_vec.T).cluster_centers_.T  # shape: (S, 2)

    # generate change map
    change_map = gene_change_map(img_diff, feature_vec, cluster_center)
    return change_map


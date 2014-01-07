from PIL import Image
import math


def calc_mean_grad(data, block_size):
    res = [0, 0, 0]
    for i in range(block_size):
        c1 = data[i]
        c2 = data[i+block_size]

        for j in range(3):
            res[j] = res[j] + c1[j] - c2[j]

    return [x/block_size for x in res]


def calc_dissimilarity(img1, img2, block_size):
    data1 = img1.getdata()
    data2 = img2.getdata()

    mean = calc_mean_grad(data1, block_size)

    res = 0
    for i in range(block_size):
        c1 = data1[i]
        c2 = data2[-(block_size-i)]

        for j in range(3):
            res += (c1[j] - c2[j] - mean[j])**2

    return math.sqrt(res)


def calc_compatibility(img1, img2, block_size):
    d1 = calc_dissimilarity(img1, img2, block_size)

    flipped1 = img1.transpose(Image.FLIP_TOP_BOTTOM)
    flipped2 = img2.transpose(Image.FLIP_TOP_BOTTOM)

    d2 = calc_dissimilarity(flipped2, flipped1, block_size)

    return d1+d2


def get_weight(img1, img2, angle):
    img1 = img1.rotate(angle)
    img2 = img2.rotate(angle)

    return calc_compatibility(img1, img2, img1.size[0])
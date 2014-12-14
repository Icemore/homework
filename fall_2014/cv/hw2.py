import numpy as np
import cv2

def filter(img, ksizeX, ksizeY, sigmaX, sigmaY, lapKsize, threshold, morfKsize):
    grayscale = cv2.cvtColor(img, cv2.COLOR_RGB2GRAY)
    blurred = cv2.GaussianBlur(grayscale, (ksizeX, ksizeY), sigmaX, sigmaY=sigmaY)
    cv2.imwrite("1_blurred.bmp", blurred)

    edges = cv2.Laplacian(blurred, cv2.CV_64F, ksize=lapKsize)
    edges[edges < 0] = 0
    edges_abs = cv2.convertScaleAbs(edges)
    cv2.imwrite("2_edges.bmp", edges_abs)

    ret, binary = cv2.threshold(edges_abs, threshold, 255, cv2.THRESH_BINARY)
    cv2.imwrite("3_binary.bmp", binary)

    kernel = cv2.getStructuringElement(cv2.MORPH_RECT, (morfKsize, morfKsize))
    closed = cv2.morphologyEx(binary, cv2.MORPH_CLOSE, kernel)
    cv2.imwrite("4_closed.bmp", closed)

    return closed

img = cv2.imread("text.bmp")
components = filter(img, ksizeX=5, ksizeY=1, sigmaX=0, sigmaY=0, lapKsize=5, threshold=0, morfKsize=3)

h, w = img.shape[:2]
mask = np.zeros((h + 2, w + 2), np.uint8)
mask[:] = 0

for i in xrange(h):
    for j in xrange(w):
        if components[i, j] != 0 and mask[i + 1, j + 1] == 0:
            r, (x, y, rw, rh) = cv2.floodFill(components, mask, (j, i), (255, 255, 255), flags=cv2.FLOODFILL_MASK_ONLY)
            cv2.rectangle(img, (x, y), (x + rw, y + rh), (0, 0, 255), 1)

cv2.imwrite("5_res.bmp", img)
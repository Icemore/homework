import numpy as np
import cv2

def filter(img, ksizeX, ksizeY, sigmaX, sigmaY, laplaceKsize, threshold):
    blurred = cv2.GaussianBlur(img, (ksizeX, ksizeY), sigmaX, sigmaY=sigmaY)

    edges = cv2.Laplacian(blurred, cv2.CV_64F, ksize=laplaceKsize)
    edges_abs = cv2.convertScaleAbs(edges)

    ret, binary = cv2.threshold(edges_abs, threshold, 255, cv2.THRESH_BINARY)
    return binary


img = cv2.imread("text.bmp", cv2.CV_LOAD_IMAGE_GRAYSCALE)
res = filter(img, 9, 1, 5, 1, 5, 0)
cv2.imwrite("out.bmp", res)
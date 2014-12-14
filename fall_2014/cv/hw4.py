import cv2
import numpy as np

img = cv2.imread('mandril.bmp', cv2.IMREAD_GRAYSCALE)

rows, cols = img.shape
crow, ccol = rows/2, cols/2
rotMat = cv2.getRotationMatrix2D((crow, ccol), 45, 0.5)

rotImg = cv2.warpAffine(img, rotMat, img.shape)

sift = cv2.SIFT()
kp1, des1 = sift.detectAndCompute(img, None)
kp2, des2 = sift.detectAndCompute(rotImg, None)

FLANN_INDEX_KDTREE = 0
index_params = dict(algorithm=FLANN_INDEX_KDTREE, trees=5)
search_params = dict(checks=50)
flann = cv2.FlannBasedMatcher(index_params, search_params)

matches = flann.match(des1, des2)

eps = 1
correct_matches = 0
for match in matches:
    p1 = kp1[match.queryIdx].pt
    p2 = kp2[match.trainIdx].pt
    expected = np.dot(rotMat, np.array([p1[0], p1[1], 1]))

    if np.linalg.norm(p2 - expected) < eps:
        correct_matches += 1

print "Points on original image: %s" % len(matches)
print "Correct matches: %s (%.2f%%)" % (correct_matches, correct_matches * 100.0 / len(matches))




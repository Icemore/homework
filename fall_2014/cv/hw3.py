import numpy as np
import cv2


def do_fft(img, cut_size):
    fft = np.fft.fft2(img)
    fft_shift = np.fft.fftshift(fft)

    rows, cols = img.shape
    crow, ccol = rows/2, cols/2
    fft_shift[crow-cut_size:crow+cut_size, ccol-cut_size:ccol+cut_size] = 0

    fft_ishift = np.fft.ifftshift(fft_shift)
    fft_res = np.fft.ifft2(fft_ishift)
    return np.abs(fft_res)


def do_laplacian(img, ksize):
    edges = cv2.Laplacian(img, cv2.CV_64F, ksize=ksize)
    return edges


img = cv2.imread("mandril.bmp", cv2.IMREAD_GRAYSCALE)

fft_res = do_fft(img, cut_size=30)
laplacian_res = do_laplacian(img, ksize=3)

cv2.imwrite("fft_res.bmp", fft_res)
cv2.imwrite("laplacian_res.bmp", laplacian_res)


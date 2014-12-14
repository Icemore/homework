import numpy as np
import cv2


def detect_harris(img):
    params = dict(maxCorners=100,
                  qualityLevel=0.3,
                  minDistance=7,
                  blockSize=7)
    return cv2.goodFeaturesToTrack(img, useHarrisDetector=True, **params)


def detect_fast(img):
    maxPoints = 100

    fast = cv2.FastFeatureDetector()
    kp = fast.detect(img)
    kp = sorted(kp, key=lambda k: -k.response)

    res = [[[k.pt[0], k.pt[1]]] for k in kp[:maxPoints]]
    res = np.array(res, np.float32)

    return res

def run(detector, name):
    cap = cv2.VideoCapture('sequence.mpg')
    video_params = dict(fourcc=int(cap.get(cv2.cv.CV_CAP_PROP_FOURCC)),
                        fps=int(cap.get(cv2.cv.CV_CAP_PROP_FPS)),
                        frameSize=(int(cap.get(cv2.cv.CV_CAP_PROP_FRAME_WIDTH)),
                                    int(cap.get(cv2.cv.CV_CAP_PROP_FRAME_HEIGHT))))

    writer = cv2.VideoWriter('%s.avi' % name, **video_params)

    ret, old_frame = cap.read()
    old_frame = cv2.cvtColor(old_frame, cv2.COLOR_BGR2GRAY)
    p0 = detector(old_frame)

    while True:
        ret, frame = cap.read()

        if not ret:
            break

        frame_gray = cv2.cvtColor(frame, cv2.COLOR_BGR2GRAY)
        p1, st, err = cv2.calcOpticalFlowPyrLK(old_frame, frame_gray, p0)

        good_new = p1[st == 1]

        for p in good_new:
            x, y = p.ravel()
            cv2.circle(frame, (x, y), 5, (255, 0, 0), -1)

        writer.write(frame)

        old_frame = frame_gray
        p0 = good_new.reshape(-1, 1, 2)

    cap.release()
    writer.release()

run(detect_harris, "harris")
run(detect_fast, "fast")

import cv2
import numpy as np


def predict(image):
    nparr = np.fromstring(image, np.uint8)
    img = cv2.imdecode(nparr, cv2.IMREAD_COLOR)
    gray_image = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)
    res, im_png = cv2.imencode(".png", gray_image)

    return im_png

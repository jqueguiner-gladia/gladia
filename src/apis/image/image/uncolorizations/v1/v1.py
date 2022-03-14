import cv2
import numpy as np


def predict(image):
    img = cv2.imdecode(
        np.fromstring(image, np.uint8),
        cv2.IMREAD_COLOR
    )

    gray_image = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)

    is_successful, im_png = cv2.imencode(".png", gray_image)

    if is_successful:
        return im_png

    raise Exception("Error encoding image")

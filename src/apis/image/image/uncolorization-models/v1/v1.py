import cv2
import numpy as np


def predict(image: bytes) -> np.ndarray:
    """
    Take an image as input and return it as grayscale

    Args:
        image (bytes): image to convert to grayscale

    Returns:
        numpy.ndarray: grayscale image
    """

    img = cv2.imdecode(np.fromstring(image, np.uint8), cv2.IMREAD_COLOR)

    gray_image = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)

<<<<<<< HEAD
    _, im_png = cv2.imencode(
        ".png", gray_image, [int(cv2.IMWRITE_PNG_COMPRESSION), 9]
    )
=======
    _, im_png = cv2.imencode(".png", gray_image, [int(cv2.IMWRITE_PNG_COMPRESSION), 9])
>>>>>>> 9fa8c3deb097c735c5f04d2ca513df0c7875df89

    return im_png

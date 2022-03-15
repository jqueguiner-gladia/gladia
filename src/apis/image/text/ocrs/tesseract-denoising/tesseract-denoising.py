import re
import cv2
import pytesseract
import numpy as np


def bytes_image_to_gray_np_image(image: bytes) -> np.ndarray:
    """
    Convert a bytes image to a gray image stored in a np.ndarray

    :param image: image to convert
    :return: gray image as a np.ndarray
    """

    np_image = cv2.imdecode(
        buf=np.fromstring(image, np.uint8),
        flags=cv2.IMREAD_COLOR
    )

    return cv2.cvtColor(np_image, cv2.COLOR_BGR2GRAY)


def predict(image: bytes, source_language: str) -> [str]:
    """
    Call the tesseract ocr, apply a median blurring filter to the input image and return the text detected in the image

    :param image: image to provide to the ocr
    :param source_language: [UNUSED] language of the text to be searched
    :return: characters found in the image
    """

    del source_language

    gray_image = bytes_image_to_gray_np_image(image)
    
    gray_thresh = cv2.medianBlur(gray_image, 3)
    
    text = str(pytesseract.image_to_string(gray_thresh))

    out = re.sub(r'[\x00-\x08\x0b\x0c\x0e-\x1f\x7f-\xff]', '', text)

    return [out.strip()]

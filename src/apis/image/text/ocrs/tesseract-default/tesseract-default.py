import re
import cv2
import pytesseract
import numpy as np


def bytes_image_to_gray_cv2_image(image):
    img_np = cv2.imdecode(
        buf=np.fromstring(image, np.uint8),
        flags=cv2.IMREAD_COLOR
    )

    return cv2.cvtColor(img_np, cv2.COLOR_BGR2GRAY)


def predict(image, source_language):
    del source_language

    gray_image = bytes_image_to_gray_cv2_image(image)

    gray_thresh = cv2.threshold(
        src=gray_image,
        thresh=0,
        maxval=255,
        type=cv2.THRESH_BINARY | cv2.THRESH_OTSU,
    )[1]
    
    text = pytesseract.image_to_string(gray_thresh)

    out = re.sub(r'[\x00-\x08\x0b\x0c\x0e-\x1f\x7f-\xff]', '', text)

    return [out.strip()]

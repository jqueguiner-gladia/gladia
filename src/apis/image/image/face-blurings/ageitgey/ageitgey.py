import io

import face_recognition
from icecream import ic
from skimage.filters import gaussian
from unifai_api_utils.file_management import input_to_files
from unifai_api_utils.image_management import blur_image
from PIL import Image
import cv2

@input_to_files
def predict(image):
    sigma = 50

    image = face_recognition.load_image_file(image)

    locations = face_recognition.face_locations(image)
    
    for location in locations:
        startY = location[0]
        endY = location[2]
        startX = location[1]
        endX = location[3]
        image = blur_image(image, startX, endX, startY, endY, sigma=sigma)

    res, im_png = cv2.imencode(".png", image)

    return im_png

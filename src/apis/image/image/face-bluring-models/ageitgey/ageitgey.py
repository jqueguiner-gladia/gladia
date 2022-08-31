from io import BytesIO

import face_recognition
from gladia_api_utils.file_management import input_to_files
from gladia_api_utils.image_management import blur_image
from gladia_api_utils.io import np_to_img_pil
from gladia_api_utils.io import _open


@input_to_files
def predict(image: bytes) -> BytesIO:
    """
    Call the model returning the image with the faces blured

    Args:
        image (bytes): Image to blur

    Returns:
        BytesIO: Image with the faces blured
    """

    sigma = 50
    width, height = _open(image).size

    image = face_recognition.load_image_file(image)

    locations = face_recognition.face_locations(image)

    for location in locations:
        (startY, endY) = location[0:2]
        (startX, endX) = location[2:4]

        image = blur_image(image, startX, endX, startY, endY, sigma=sigma)

    image = np_to_img_pil(image)

    image.resize((width, height))

    return image

import os
import onnxruntime as ort

from PIL import Image
from numpy import ndarray
from numpy import asarray as as_nparray
from typing import Tuple

from gladia_api_utils.io import _open
from gladia_api_utils.image_management import draw_segment
from gladia_api_utils.model_management import download_models


models_to_download = {
    "mobile-net": {
        "url": "https://huggingface.co/Gladiaio/databuzzword_mobile-net_onnx",
        "output_path": "models",
    }
}

models_path = download_models(models_to_download)


def run(image: Image, fast: bool = True) -> Tuple[Image.Image, ndarray]:
    """
    Call the model to return the image without its background

    :param image: Image to remove the background from
    :param fast: unused
    :return: image without its background
    """

    del fast

    INPUT_SIZE = 513
    MODEL_PATH = os.path.join(models_path["mobile-net"]["output_path"], "databuzzword_mobile-net_onnx.onnx")
    INPUT_TENSOR_NAME = "ImageTensor:0"

    width, height = image.size
    resize_ratio = 1.0 * INPUT_SIZE / max(width, height)
    target_size = (int(resize_ratio * width), int(resize_ratio * height))
    resized_image = image.convert("RGB").resize(target_size, Image.ANTIALIAS)

    ort_sess = ort.InferenceSession(MODEL_PATH)
    seg_map = ort_sess.run(None, {INPUT_TENSOR_NAME: [as_nparray(resized_image)]})[0][0]

    return resized_image, seg_map


def predict(image) -> Image:
    """
    Call the model to return the image without its background

    :param image: Image to remove the background from
    :return: image without its background
    """

    image = _open(image)
    resized_im, seg_map = run(image, True)

    img = draw_segment(resized_im, seg_map)

    return img

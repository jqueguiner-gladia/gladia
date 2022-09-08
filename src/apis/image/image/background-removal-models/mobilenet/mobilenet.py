from typing import Tuple

import onnxruntime as ort
from gladia_api_utils.image_management import draw_segment
from gladia_api_utils.io import _open
from gladia_api_utils.model_management import download_model
from numpy import asarray as as_nparray
from numpy import ndarray
from PIL import Image

MODEL_PATH = download_model(
    url="https://huggingface.co/Gladiaio/databuzzword_mobile-net_onnx/resolve/main/databuzzword_mobile-net_onnx.onnx",
    output_path="mobile-net_onnx.onnx",
    uncompress_after_download=False,
)


def run(image: Image, fast: bool = True) -> Tuple[Image.Image, ndarray]:
    """
    Call the model to return the image without its background

    Args:
        image (Image): Image to remove the background from
        fast (bool): If True, uses a faster but less accurate model

    Returns:
        Image: Image without its background
    """

    del fast

    INPUT_SIZE = 513

    INPUT_TENSOR_NAME = "ImageTensor:0"

    width, height = image.size
    resize_ratio = 1.0 * INPUT_SIZE / max(width, height)
    target_size = (int(resize_ratio * width), int(resize_ratio * height))
    resized_image = image.convert("RGB").resize(target_size, Image.ANTIALIAS)

    ort_sess = ort.InferenceSession(MODEL_PATH)
    seg_map = ort_sess.run(None, {INPUT_TENSOR_NAME: [as_nparray(resized_image)]})[0][0]

    return resized_image, seg_map


def predict(image: bytes) -> Image:
    """
    Call the model to return the image without its background

    Args:
        image (bytes): Image to remove the background from

    Returns:
        Image: Image without its background
    """

    image = _open(image).convert("RGB")
    resized_im, seg_map = run(image, True)

    img = draw_segment(resized_im, seg_map)

    return img

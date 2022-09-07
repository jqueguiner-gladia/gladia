import gc
from pathlib import Path
from unittest import result

import torch
from deoldify import device, visualize
from deoldify.device_id import DeviceId
from gladia_api_utils.io import _open
from gladia_api_utils.model_management import download_model
from gladia_api_utils.system import get_random_available_gpu_id
from PIL import Image

MODEL_PATH = download_model(
    url="https://huggingface.co/databuzzword/deoldify-artistic/resolve/main/ColorizeArtistic_gen.pth",
    output_path="ColorizeStable_gen.pth",
    uncompress_after_download=False,
)


def predict(image: bytes) -> Image:
    """
    Call the model to return the image colorized

    Args:
        image (bytes): Image to colorize

    Returns:
        Image: Colorized image
    """

    gpu_id = get_random_available_gpu_id()

    device_to_use = (
        getattr(DeviceId, f"GPU{gpu_id}") if gpu_id is not None else DeviceId.CPU
    )

    device.set(device=device_to_use)

    render_factor = 30

    image = _open(image).convert("RGB")
    width, height = image.size

    image_colorizer = visualize.get_image_colorizer(
        root_folder=Path(MODEL_PATH).parent,
        render_factor=render_factor,
        artistic=True,
    )

    result = image_colorizer.get_transformed_image(
        path=image, render_factor=render_factor
    )

    del image_colorizer
    gc.collect()
    torch.cuda.empty_cache()

    result.resize((width, height))

    return result

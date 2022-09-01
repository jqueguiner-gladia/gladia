from os import path
from pathlib import Path

from deoldify import device, visualize
from deoldify.device_id import DeviceId
from gladia_api_utils.io import _open
from gladia_api_utils.model_management import download_models
from gladia_api_utils.system import get_random_available_gpu_id
from PIL import Image
from torch.cuda import is_available as cuda_is_available


def predict(image: bytes) -> Image:
    """
    Call the model to return the image colorized

    :param image: image to colorize
    :return: colorized image
    """

    urls = {
        "deoldify-artistic": {
            "url": "https://huggingface.co/databuzzword/deoldify-artistic",
            "output_path": "models",
        }
    }

    models_path = download_models(urls)

    current_model_path = path.join(models_path["deoldify-artistic"]["output_path"])

    gpu_id = get_random_available_gpu_id()

    device_to_use = (
        getattr(DeviceId, f"GPU{gpu_id}") if gpu_id is not None else DeviceId.CPU
    )

    device.set(device=device_to_use)

    render_factor = 30

    image = _open(image).convert("RGB")

    image_colorizer = visualize.get_image_colorizer(
        root_folder=Path(current_model_path).parent,
        render_factor=render_factor,
        artistic=True,
    )

    result = image_colorizer.get_transformed_image(
        path=image, render_factor=render_factor
    )

    del image_colorizer

    return result

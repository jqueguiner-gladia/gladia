import os

import deoldify
from deoldify import device
from deoldify.device_id import DeviceId
from deoldify import visualize

from pathlib import Path

from icecream import ic

from gladia_api_utils.io import _open
from gladia_api_utils.model_management import download_models
import numpy as np

device.set(device=DeviceId.GPU0)


urls = {
    "deoldify-artistic": {
        "url": "https://huggingface.co/databuzzword/deoldify-artistic",
        "output_path": "models",
    }
}

models_path = download_models(urls)

current_model_path = os.path.join(models_path["deoldify-artistic"]["output_path"])

def predict(image):
    render_factor = 30

    image = _open(image)

    image_colorizer = visualize.get_image_colorizer(
        root_folder=Path(current_model_path).parent,
        render_factor=render_factor,
        artistic=True,
    )

    result = image_colorizer.get_transformed_image(
        path=image, render_factor=render_factor
    )

    return result
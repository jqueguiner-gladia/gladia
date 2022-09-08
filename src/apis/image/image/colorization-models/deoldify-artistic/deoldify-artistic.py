import gc
from pathlib import Path

import torch
from deoldify import device
from deoldify.device_id import DeviceId
from deoldify.visualize import get_artistic_image_colorizer
from gladia_api_utils.file_management import create_random_directory, delete_directory
from gladia_api_utils.io import _open
from gladia_api_utils.model_management import download_model
from gladia_api_utils.system import get_random_available_gpu_id
from PIL import Image


def predict(image: bytes) -> Image:
    """
    Call the model to return the image colorized

    Args:
        image (bytes): Image to colorize

    Returns:
        Image: Colorized image
    """

    model_path = download_model(
        url="https://huggingface.co/databuzzword/deoldify-artistic/resolve/main/ColorizeArtistic_gen.pth",
        output_path="models/ColorizeArtistic_gen.pth",
    )

    gpu_id = get_random_available_gpu_id()

    device_to_use = (
        getattr(DeviceId, f"GPU{gpu_id}") if gpu_id is not None else DeviceId.CPU
    )

    device.set(device=device_to_use)

    render_factor = 35

    image = _open(image).convert("RGB")
    width, height = image.size

    result_directory = create_random_directory("/tmp/deoldify-artistic/results")

    # this package is based on fastai which is shadowy
    # when saving Learners, it saves the whole state of the learner
    # in a directory called models and you can't change that
    # this is the reason why we need to set the output_path models/ColorizeStable_gen.pth
    # and then call parent.parent to get the parent directory of the model
    image_colorizer = get_artistic_image_colorizer(
        root_folder=Path(model_path).parent.parent,
        render_factor=render_factor,
        results_dir=result_directory,
        weights_name="ColorizeArtistic_gen",
    )

    result = image_colorizer.get_transformed_image(
        path=image, render_factor=render_factor
    )

    del image_colorizer
    gc.collect()
    torch.cuda.empty_cache()

    delete_directory(result_directory)
    result.resize((width, height))

    return result

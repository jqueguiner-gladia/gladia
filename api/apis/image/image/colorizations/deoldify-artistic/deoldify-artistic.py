# import the necessary packages
import io
import os
import ssl
import sys
import traceback
from os import path
from pathlib import Path

import deoldify
import fastai
import numpy as np
import requests
import torch
from ai_api_utils.io import _open
from ai_api_utils.model_management import download_models
from deoldify.visualize import *
from icecream import ic


torch.backends.cudnn.benchmark = True
os.environ["CUDA_VISIBLE_DEVICES"] = "0"

urls = {
    "deoldify-artistic": {
        "url": "https://huggingface.co/databuzzword/deoldify-artistic",
        "output_path": "models",
    }
}

models_path = download_models(urls)

current_model_path = os.path.join(models_path["deoldify-artistic"]["output_path"])

# define a predict function as an endpoint
def predict(image):
    render_factor = 30

    image = _open(image)

    image_colorizer = get_image_colorizer(
        root_folder=Path(current_model_path).parent,
        render_factor=render_factor,
        artistic=True,
    )

    result = image_colorizer.get_transformed_image(
        path=image, render_factor=render_factor
    )

    return result

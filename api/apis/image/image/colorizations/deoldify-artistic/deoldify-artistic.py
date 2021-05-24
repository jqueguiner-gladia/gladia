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

# Handle switch between GPU and CPU
if torch.cuda.is_available():
    torch.backends.cudnn.benchmark = True
    os.environ["CUDA_VISIBLE_DEVICES"] = "0"
else:
    del os.environ["CUDA_VISIBLE_DEVICES"]


urls = {
    "deoldify-artistic": {
        "url": "https://huggingface.co/databuzzword/deoldify-artistic",
        "output_path": "models",
    }
}

models_path = download_models(urls)

current_model_path = os.path.join(models_path["deoldify-artistic"]["output_path"])

# define a predict function as an endpoint
def predict(content, options={}):
    if "render_factor" in options:
        render_factor = int(options["render_factor"])
    else:
        render_factor = 30

    image = _open(content)

    image_colorizer = get_image_colorizer(
        root_folder=Path(current_model_path).parent,
        render_factor=render_factor,
        artistic=True,
    )

    result = image_colorizer.get_transformed_image_from_bytes(
        image=image, render_factor=render_factor
    )

    return result

from collections import OrderedDict

import cv2
import numpy as np
import requests
import torch
import torch.nn.functional as F
import torchvision.transforms.functional as TF
from gladia_api_utils.io import _open
from gladia_api_utils.model_management import download_models
from natsort import natsorted
from PIL import Image
from skimage import img_as_ubyte

from apis.image.image.deblurring_models.CMFNet.model.CMFNet import CMFNet


def load_checkpoint(model: torch.nn.Module, weights: str) -> None:
    """
    Loads a checkpoint into a model

    Args:
        model (torch.nn.Module): Model to load weights to.
        weights (str): Path to checkpoint.

    Returns:
        None
    """
    checkpoint = torch.load(weights, map_location=torch.device("cpu"))
    try:
        model.load_state_dict(checkpoint["state_dict"])
    except RuntimeError:
        state_dict = checkpoint["state_dict"]
        new_state_dict = OrderedDict()
        for k, v in state_dict.items():
            name = k[7:]  # remove `module.`
            new_state_dict[name] = v
        model.load_state_dict(new_state_dict)


def predict(image: bytes) -> Image:
    """
    deblurs an image using CMFNet

    Args:
        image (bytes): Image to deblur.

    Returns:
        Image: Deblurred image.
    """

    model_url = {
        "model": {
            "url": "https://github.com/FanChiMao/CMFNet/releases/download/v0.0/deblur_GoPro_CMFNet.pth",
            "output_path": "deblur_GoPro_CMFNet.pth",
        },
    }

    image = _open(image).convert("RGB")

    model_path = download_models(model_url)["model"]["output_path"]

    basewidth = 512
    wpercent = basewidth / float(image.size[0])
    hsize = int((float(image.size[1]) * float(wpercent)))
    image = image.resize((basewidth, hsize), Image.BILINEAR)

    model = CMFNet()
    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    model = model.to(device)
    model.eval()
    load_checkpoint(model, model_path)

    mul = 8

    img = image
    input_ = TF.to_tensor(img).unsqueeze(0).to(device)

    # Pad the input if not_multiple_of 8
    h, w = input_.shape[2], input_.shape[3]
    H, W = ((h + mul) // mul) * mul, ((w + mul) // mul) * mul
    padh = H - h if h % mul != 0 else 0
    padw = W - w if w % mul != 0 else 0
    input_ = F.pad(input_, (0, padw, 0, padh), "reflect")

    with torch.no_grad():
        restored = model(input_)

    restored = torch.clamp(restored, 0, 1)
    restored = restored[:, :, :h, :w]
    restored = restored.permute(0, 2, 3, 1).cpu().detach().numpy()
    restored = img_as_ubyte(restored[0])

    return Image.fromarray(restored)

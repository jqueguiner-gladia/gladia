from typing import Dict, List, Union

from gladia_api_utils.io import _open
from gladia_api_utils.TorchvisionModelHelper import TorchvisionModel
from importlib_metadata import version


def predict(
    image: bytes, top_k: int = 1
) -> Dict[str, Union[str, Dict[str, float]]]:
    img = _open(image)

    model = TorchvisionModel(
        model_name="wide_resnet101_2",
        weights="Wide_ResNet101_2_Weights",
        weights_version="IMAGENET1K_V2",
    )
    output = model(img, top_k)

    return {
        "prediction": output["prediction"],
        "prediction_raw": output["prediction_raw"],
    }

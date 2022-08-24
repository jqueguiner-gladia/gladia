from gladia_api_utils.io import _open
from gladia_api_utils.TorchvisionModelHelper import TorchvisionModel
from typing import Dict, List, Union


def predict(image: bytes, top_k: int = 1) -> Dict[str, Union[List[Dict[str, Union[str, float]]], Dict[str, float]]]:
    img = _open(image)

    model = TorchvisionModel(
        model_name="mobilenet_v3_large",
        weights="MobileNet_V3_Large_Weights",
        weights_version="IMAGENET1K_V1",
    )
    output = model(img, top_k)

    return { "prediction": output["prediction"], "prediction_raw": output["prediction_raw"] }

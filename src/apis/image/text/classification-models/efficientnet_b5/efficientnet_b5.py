from typing import Dict, List, Union

from gladia_api_utils.io import _open
from gladia_api_utils.TorchvisionModelHelper import TorchvisionModel


def predict(
    image: bytes, top_k: int = 1
) -> Dict[str, Union[str, Dict[str, float]]]:
    img = _open(image)

    model = TorchvisionModel(
        model_name="efficientnet_b5", weights="EfficientNet_B5_Weights"
    )
    output = model(img, top_k)

    return {
        "prediction": output["prediction"],
        "prediction_raw": output["prediction_raw"],
    }

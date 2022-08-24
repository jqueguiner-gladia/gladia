from typing import Dict, List, Union

from gladia_api_utils.io import _open
from gladia_api_utils.TorchvisionModelHelper import TorchvisionModel


def predict(
    image: bytes, top_k: int = 1
) -> Dict[str, Union[List[Dict[str, Union[str, float]]], Dict[str, float]]]:
    img = _open(image)

    model = TorchvisionModel(model_name="vgg13_bn", weights="VGG13_BN_Weights")
    output = model(img, top_k)

    return {
        "prediction": output["prediction"],
        "prediction_raw": output["prediction_raw"],
    }

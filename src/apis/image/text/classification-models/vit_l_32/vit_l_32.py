from typing import Dict, List, Union

from gladia_api_utils.io import _open
from gladia_api_utils.TorchvisionModelHelper import TorchvisionModel


def predict(
    image: bytes, top_k: int = 1
) -> Dict[str, Union[str, Dict[str, float]]]:
    img = _open(image)

    model = TorchvisionModel(model_name="vit_l_32", weights="ViT_L_32_Weights")
    output = model(img, top_k)

    return {
        "prediction": output["prediction"],
        "prediction_raw": output["prediction_raw"],
    }

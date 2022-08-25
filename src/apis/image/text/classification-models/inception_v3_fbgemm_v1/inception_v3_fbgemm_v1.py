from typing import Dict, List, Union

from gladia_api_utils.io import _open
from gladia_api_utils.TorchvisionModelHelper import TorchvisionModel


def predict(
    image: bytes, top_k: int = 1
) -> Dict[str, Union[str, Dict[str, float]]]:
    img = _open(image)

    model = TorchvisionModel(
        model_name="inception_v3",
        weights="Inception_V3_QuantizedWeights",
        weights_version="IMAGENET1K_FBGEMM_V1",
        quantized=True,
    )
    output = model(img, top_k)

    return {
        "prediction": output["prediction"],
        "prediction_raw": output["prediction_raw"],
    }

from typing import Dict, List, Union

import easyocr
import numpy as np
from gladia_api_utils.io import _open


def predict(image: bytes, source_language: str) -> Dict[str, Union[str, List[str]]]:
    """
    Call the EasyOcr package and return the text detected in the image by the ocr

    Args:
        image (bytes): The image to be processed
        source_language (str): The language of the image

    Returns:
        Dict[str, Union[str, List[str]]]: The text detected in the image by the ocr
    """

    image = _open(image)
    image = np.array(image)
    reader = easyocr.Reader([source_language], gpu=True)
    text = reader.readtext(image, detail=False)
    plain_text = "\n".join(text)

    return {"prediction": plain_text, "prediction_raw": text}

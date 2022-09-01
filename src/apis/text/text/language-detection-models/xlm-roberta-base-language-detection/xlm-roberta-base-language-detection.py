from typing import Dict, Union

import numpy as np
from gladia_api_utils.triton_helper import (
    TritonClient,
    check_if_model_needs_to_be_preloaded,
    data_processing,
)


def softmax(x):
    return np.exp(x) / np.sum(np.exp(x), axis=0)


def predict(text: str) -> Dict[str, Union[str, Dict[str, float]]]:
    """
    From a given text, return a json scoring the probability of the given text to be of a certain language

    Args:
        text (str): The text to detect the language of

    Returns:
        Dict[str, Union[str, Dict[str, float]]]: The language of the text and the probability of the text to be of that language
    """

    MODEL_NAME = "language-detection_papluca_xlm-roberta-base-language-detection_tensorrt_inference"
    MODEL_SUB_PARTS = [
        "language-detection_papluca_xlm-roberta-base-language-detection_tensorrt_model",
        "language-detection_papluca_xlm-roberta-base-language-detection_tensorrt_tokenize",
    ]

    LANGUAGES = [
        "ja",
        "nl",
        "ar",
        "pl",
        "de",
        "it",
        "pt",
        "tr",
        "es",
        "hi",
        "el",
        "ur",
        "bg",
        "en",
        "fr",
        "zh",
        "ru",
        "th",
        "sw",
        "vi",
    ]

    client = TritonClient(
        model_name=MODEL_NAME,
        sub_parts=MODEL_SUB_PARTS,
        output_name="output",
        preload_model=check_if_model_needs_to_be_preloaded(MODEL_NAME),
    )

    np_output = data_processing.text_to_numpy(text)

    client.set_input(name="TEXT", shape=np_output.shape, datatype="BYTES")

    output = list(softmax(client(np_output)[0][0]))

    prediction = LANGUAGES[output.index(max(output))]
    prediction_raw = dict(zip(LANGUAGES, output))

    return {"prediction": prediction, "prediction_raw": prediction_raw}

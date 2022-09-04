from typing import Dict, Union

import numpy as np
import truecase
from gladia_api_utils.triton_helper import (
    TritonClient,
    check_if_model_needs_to_be_preloaded,
)
from transformers import BertTokenizer


def predict(text: str) -> Dict[str, Union[str, Dict[str, float]]]:
    """
    Detect hate from a given text

    Args:
        text (str): The text to be detect hate in

    Returns:
        Dict[str, Union[str, Dict[str, float]]]: The level of hate in the text (normal, hate-speech, offensive)
    """

    LABELS = ["hate-speech", "normal", "offensive"]
    MODEL_NAME = "hate-speech-detection_bert-base-uncased-hatexplain_base_traced"
    TOKENIZER_NAME = "Hate-speech-CNERG/bert-base-uncased-hatexplain"

    client = TritonClient(
        model_name=MODEL_NAME,
        preload_model=check_if_model_needs_to_be_preloaded(MODEL_NAME),
    )

    tokenizer = BertTokenizer.from_pretrained(TOKENIZER_NAME)

    input_ids = tokenizer(
        truecase.get_true_case(text),
        return_tensors="pt",
        max_length=256,
        padding="max_length",
    ).input_ids

    client.set_input(shape=(1, 256), datatype="INT32")
    output = client(input_ids.detach().numpy().astype(np.int32))[0]

    out = LABELS[np.argmax(output)]

    del tokenizer

    return {"prediction": out, "prediction_raw": dict(zip(LABELS, output[0]))}

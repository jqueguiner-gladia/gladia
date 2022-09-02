import json
from typing import Dict, List, Union

import truecase
from gladia_api_utils.triton_helper import (
    TritonClient,
    check_if_model_needs_to_be_preloaded,
    data_processing,
)


def predict(text: str) -> Dict[str, Union[List[Dict[str, Union[str, float]]], str]]:
    """
    Apply NER on the given task and return each token within the sentence associated to its label.

    **Labels**:
    O : Outside of a named entity
    B-MISC : Beginning of a miscellaneous entity right after another miscellaneous entity
    I-MISC : Miscellaneous entity
    B-PER : Beginning of a person's name right after another person's name
    I-PER : Person's name
    B-ORG : Beginning of an organisation right after another organisation
    I-ORG : Organisation
    B-LOC : Beginning of a location right after another location
    I-LOC : Location

    Args:
        text (str): The text to apply NER on

    Returns:
        Dict[str, Union[List[Dict[str, Union[str, float]]], str]]: The text with the NER applied (O, B-MISC, I-MISC, B-PER, I-PER, B-ORG, I-ORG, B-LOC, I-LOC)
    """

    MODEL_NAME = "named-entity-recognition_dbmdz_bert-large-cased-finetuned-conll03-english_tensorrt_inference"
    MODEL_SUB_PARTS = [
        "named-entity-recognition_dbmdz_bert-large-cased-finetuned-conll03-english_tensorrt_model",
    ]

    client = TritonClient(
        model_name=MODEL_NAME,
        sub_parts=MODEL_SUB_PARTS,
        output_name="output",
        preload_model=check_if_model_needs_to_be_preloaded(MODEL_NAME),
    )

    np_output = data_processing.text_to_numpy(truecase.get_true_case(text))

    client.set_input(name="TEXT", shape=np_output.shape, datatype="BYTES")

    prediction_raw = client(np_output)
    prediction_decode = prediction_raw[0].decode("utf8")
    prediction = json.loads(prediction_decode)[0]

    return {"prediction": prediction, "prediction_raw": prediction_raw}

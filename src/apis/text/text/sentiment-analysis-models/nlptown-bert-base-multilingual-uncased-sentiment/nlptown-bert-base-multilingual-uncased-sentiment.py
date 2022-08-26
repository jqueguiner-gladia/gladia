from typing import Dict, List, Union

import truecase
from gladia_api_utils.triton_helper import (
    TritonClient,
    check_if_model_needs_to_be_preloaded,
    data_processing,
)


def predict(text: str) -> Dict[str, Union[str, List[float]]]:
    """
    From a given, classify it between 1 (hate) and 5 (love).

    :param text: text to analyze
    :return: text score [1;5]
    """

    MODEL_NAME = "sentiment-analyses_nlptown_bert-base-multilingual-uncased-sentiment_tensorrt_inference"
    MODEL_SUB_PARTS = [
        "sentiment-analyses_nlptown_bert-base-multilingual-uncased-sentiment_tensorrt_model",
        "sentiment-analyses_nlptown_bert-base-multilingual-uncased-sentiment_tensorrt_tokenize",
    ]

    client = TritonClient(
        model_name=MODEL_NAME,
        sub_parts=MODEL_SUB_PARTS,
        output_name="output",
        preload_model=check_if_model_needs_to_be_preloaded(MODEL_NAME),
    )

    numpy_input = data_processing.text_to_numpy(truecase.get_true_case(text))

    client.set_input(name="TEXT", shape=numpy_input.shape, datatype="BYTES")

    output = client(numpy_input)[0][0]

    # output in theory is more than negative / neutral / positive
    # but we want to return a score sentiment
    # so we need to normalize the output
    # it should have been
    # rating = {0: "hate", 1: "negative", 2: "neutral", 3: "positive", 4: "love"}

    rating = {0: "negative", 1: "negative", 2: "neutral", 3: "positive", 4: "positive"}
    label = rating[output.index(max(output))].upper()

    return {"prediction": label, "prediction_raw": output}

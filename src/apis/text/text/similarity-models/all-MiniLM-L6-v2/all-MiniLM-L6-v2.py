from typing import Dict

from gladia_api_utils.triton_helper import (
    TritonClient,
    check_if_model_needs_to_be_preloaded,
    data_processing,
)
from torch import mm, tensor
from torch.nn.functional import normalize


def cos_sim(first_sentence_embedding: list, second_sentence_embedding: list) -> float:
    """
    For two given sentences embeddings a and b, computes the cosine similarity

    Args:
        first_sentence_embedding (list): first sentence embedding to compare
        second_sentence_embedding (list): second sentence embedding to compare

    Returns:
        float: cosine similarity score between 0 and 1
    """
    return mm(
        normalize(tensor(first_sentence_embedding), p=2, dim=1),
        normalize(tensor(second_sentence_embedding), p=2, dim=1).transpose(0, 1),
    )


def predict(sentence_1: str, sentence_2: str) -> Dict[float, float]:
    """
    For two given sentences, say whether they are similar or not.
    The similarity is computed with the cosine similarity.

    Args:
        sentence_1 (str): first sentence to compare
        sentence_2 (str): second sentence to compare

    Returns:
        Dict[str, float]: the similarity score between 0 and 1
    """

    MODEL_NAME = "sentence-transformers_all-MiniLM-L6-v2_tensorrt_inference"
    MODEL_SUB_PARTS = [
        "sentence-transformers_all-MiniLM-L6-v2_tensorrt_model",
        "sentence-transformers_all-MiniLM-L6-v2_tensorrt_tokenize",
    ]

    client = TritonClient(
        model_name=MODEL_NAME,
        sub_parts=MODEL_SUB_PARTS,
        output_name="output",
        preload_model=check_if_model_needs_to_be_preloaded(MODEL_NAME),
    )

    sentence_1_preprocessed = data_processing.text_to_numpy(sentence_1)
    sentence_2_preprocessed = data_processing.text_to_numpy(sentence_2)

    client.set_input(name="TEXT", shape=sentence_1_preprocessed.shape, datatype="BYTES")
    sentence_1_embeddings = client(sentence_1_preprocessed)[0]

    client.set_input(name="TEXT", shape=sentence_2_preprocessed.shape, datatype="BYTES")
    sentence_2_embeddings = client(sentence_2_preprocessed)[0]

    cosine_scores = cos_sim(sentence_1_embeddings, sentence_2_embeddings)

    return {"prediction": cosine_scores.item(), "prediction_raw": cosine_scores.item()}

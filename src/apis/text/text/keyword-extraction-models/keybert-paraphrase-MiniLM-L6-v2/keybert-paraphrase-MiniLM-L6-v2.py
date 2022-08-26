from typing import Dict, Union

import truecase
from gladia_api_utils.triton_helper import (
    TritonClient,
    check_if_model_needs_to_be_preloaded,
    data_processing,
)
from numpy import array as nparray
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.metrics.pairwise import cosine_similarity


def select_key_words(text_embeddings, vocabulary_embeddings, vocabulary, top_n=1):

    distances = cosine_similarity(text_embeddings, vocabulary_embeddings)
    top_n_indexes = distances.argsort()[0][-top_n:]
    all_indexes = distances.argsort()[0]
    prediction = [(vocabulary[index], distances[0][index]) for index in top_n_indexes][
        ::-1
    ]
    prediction_raw = {vocabulary[index]: distances[0][index] for index in all_indexes}
    prediction_raw = dict(reversed(list(prediction_raw.items())))

    return {"prediction": prediction[0][0], "prediction_raw": prediction_raw}


# TODO : check if num_seq > 128 and raise error if this is the case
def predict(text: str) -> Dict[str, Union[str, Dict[str, float]]]:
    """
    Extract keywords from a given sentence

    :param text: sentence to extract the keywords from
    :return: keywords founded in the sentence
    """

    MODEL_NAME = "sentence-transformers_paraphrase-MiniLM-L6-v2_tensorrt_inference"
    MODEL_SUB_PARTS = [
        "sentence-transformers_paraphrase-MiniLM-L6-v2_tensorrt_model",
        "sentence-transformers_paraphrase-MiniLM-L6-v2_tensorrt_tokenize",
    ]

    client = TritonClient(
        model_name=MODEL_NAME,
        sub_parts=MODEL_SUB_PARTS,
        output_name="output",
        preload_model=check_if_model_needs_to_be_preloaded(MODEL_NAME),
    )

    text_preprocessed = data_processing.text_to_numpy(truecase.get_true_case(text))
    client.set_input(name="TEXT", shape=text_preprocessed.shape, datatype="BYTES")
    text_embeddings = nparray(
        client(text_preprocessed, load_model=True, unload_model=False)[0]
    )

    count_vectorizer = CountVectorizer(
        ngram_range=(1, 1),  # consider only unigrams.
    )
    count_vectorizer.fit_transform([text])  # learn the vocabulary dictionary
    vocabulary = count_vectorizer.get_feature_names_out()  # returns the vocabulary set
    del count_vectorizer

    vocabulary_embeddings = []
    for idx, word in enumerate(vocabulary):
        word_preprocessed = data_processing.text_to_numpy(word)
        client.set_input(name="TEXT", shape=word_preprocessed.shape, datatype="BYTES")
        vocabulary_embeddings.append(
            client(
                word_preprocessed,
                load_model="false",
                unload_model=idx == len(vocabulary) - 1,
            )[0][0]
        )
    vocabulary_embeddings = nparray(vocabulary_embeddings)

    return select_key_words(text_embeddings, vocabulary_embeddings, vocabulary)

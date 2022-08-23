def predict(text: str) -> dict:
    """
    Extract keywords from a given sentence

    :param text: sentence to extract the keywords from
    :return: keywords founded in the sentence
    """

    from keybert import KeyBERT
    from sentence_transformers import SentenceTransformer

    model = SentenceTransformer("paraphrase-multilingual-MiniLM-L12-v2")
    kw_model = KeyBERT(model=model)

    out = kw_model.extract_keywords(text, keyphrase_ngram_range=(1, 1), stop_words=None, top_n=25)
    prediction_raw = { keyword[0]: keyword[1] for keyword in out}

    del model

    return { "prediction": out[0][0], "prediction_raw": prediction_raw}

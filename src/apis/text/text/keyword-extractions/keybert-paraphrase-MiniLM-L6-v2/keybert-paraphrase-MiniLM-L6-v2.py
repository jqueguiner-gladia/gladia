
def predict(text: str) -> [(str, float)]:
    """
    Extract keywords from a given sentence

    :param text: sentence to extract the keywords from
    :return: keywords founded in the sentence
    """
    from keybert import KeyBERT
    from sentence_transformers import SentenceTransformer

    sentence_model = SentenceTransformer("paraphrase-MiniLM-L6-v2")
    kw_model = KeyBERT(model=sentence_model)

    return kw_model.extract_keywords(text, keyphrase_ngram_range=(1, 1), stop_words=None)

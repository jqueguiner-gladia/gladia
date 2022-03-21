from happytransformer import HappyWordPrediction


def predict(sentence: str) -> str:
    """
    For a given sentence, predict the next word.

    :param sentence: sentence to continue
    :return: word predicted
    """

    happy_wp = HappyWordPrediction("BERT", "bert-base-uncased")

    result = happy_wp.predict_mask(f"{sentence} [MASK]")

    return result[0].token

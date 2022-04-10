
def predict(sentence: str) -> str:
    """
    For a given sentence, predict the next word.

    :param sentence: sentence to continue
    :return: word predicted
    """

    from happytransformer import HappyNextSentence

    happy_wp = HappyWordPrediction("ROBERTA", "roberta-base")

    result = happy_wp.predict_mask(f"{sentence} [MASK]")

    return result[0].token

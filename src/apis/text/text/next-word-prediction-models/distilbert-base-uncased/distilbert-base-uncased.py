def predict(sentence: str) -> dict:
    """
    For a given sentence, predict the next word.

    :param sentence: sentence to continue
    :return: word predicted and score
    """

    from happytransformer import HappyWordPrediction

    happy_wp = HappyWordPrediction("DISTILBERT", "distilbert-base-uncased")

    result = happy_wp.predict_mask(f"{sentence} [MASK]", top_k=25)

    prediction_raw = {word.token : word.score for word in result}
    prediction = result[0].token

    del happy_wp

    return {"prediction": prediction, "prediction_raw": prediction_raw}

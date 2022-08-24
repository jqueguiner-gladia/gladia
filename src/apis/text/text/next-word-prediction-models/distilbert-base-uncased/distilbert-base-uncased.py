from typing import Dict, Union


def predict(sentence: str) -> Dict[str, Union[str, Dict[str, float]]]:
    """
    For a given sentence, predict the next word.

    :param sentence: sentence to continue
    :return: word predicted and score
    """

    from happytransformer import HappyWordPrediction

    NB_RESULTS = 25

    happy_wp = HappyWordPrediction("DISTILBERT", "distilbert-base-uncased")

    result = happy_wp.predict_mask(f"{sentence} [MASK]", top_k=NB_RESULTS)

    prediction_raw = {word.token: word.score for word in result}
    prediction = result[0].token

    del happy_wp

    return {"prediction": prediction, "prediction_raw": prediction_raw}

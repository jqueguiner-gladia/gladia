import numpy as np
import truecase
from transformers import pipeline


def predict(text: str) -> dict:
    """
    For a given text, predict if it's POSITIVE, NEUTRAL or NEGATIVE

    :param text: text to analyze.
    :return: Dict formatted dict containing the label (POSITIVE/NEUTRAL/NEGATIVE) with it score
    """

    classifier = pipeline("zero-shot-classification")
    prediction = classifier(
        truecase.get_true_case(text),
        candidate_labels=["POSITIVE", "NEUTRAL", "NEGATIVE"],
    )

    label = prediction["labels"][np.argmax(prediction["scores"])]

    return {"prediction": label, "prediction_raw": prediction}

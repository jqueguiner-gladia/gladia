from typing import Dict, List, Union


def predict(
    text: str,
) -> Dict[str, Union[str, Dict[str, Union[str, List[str], List[float]]]]]:
    """
    For a given text, predict if it's POSITIVE or NEGATIVE

    :param text: text to analyze.
    :return: JSON formatted str containing the label (POSITIVE/NEGATIVE) with it score
    """

    happy_tc = HappyTextClassification(
        "DISTILBERT", "distilbert-base-uncased", num_labels=2
    )
    result = happy_tc.classify_text(text)
    prediction = "POSITIVE" if result.label == "LABEL_0" else "NEGATIVE"
    prediction_raw = {"label": result.label, "score": result.score}

    return {"prediction": prediction, "prediction_raw": prediction_raw}

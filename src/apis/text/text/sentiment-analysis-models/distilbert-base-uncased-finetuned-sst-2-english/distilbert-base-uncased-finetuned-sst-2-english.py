from typing import Dict, Union

def predict(text: str) -> Dict[str, Union[str, Dict[str, Union[str, float]]]]:

    """
    For a given text, predict if it's POSITIVE or NEGATIVE

    :param text: text to analyze.
    :return: JSON formatted str containing the label (POSITIVE/NEGATIVE) with it score
    """

    happy_tc = HappyTextClassification(
        model_type="DISTILBERT",
        model_name="distilbert-base-uncased-finetuned-sst-2-english",
    )
    result = happy_tc.classify_text(text)
    prediction_raw = {"label": result.label, "score": result.score}

    return {"prediction": result.label, "prediction_raw": prediction_raw}

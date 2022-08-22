import json


def predict(text: str) -> str:
    """
    From a given text, return a json scoring the probability of the given text to be of a certain language

    :param text: text to analyze
    :return: json scoring the chance of the text to be in each language
    """

    from LanguageIdentifier import rank

    prediction_raw = []

    for lang, score in rank(text):
        prediction_raw.append({lang: score})

    prediction = prediction_raw[prediction_raw.keys().index(max(prediction_raw.values()))]

    return { "prediction": prediction, "prediction_raw": prediction_raw}


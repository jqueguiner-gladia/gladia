from typing import Dict, Union


def predict(text: str) -> Dict[str, Union[str, Dict[str, float]]]:
    """
    From a given text, return a json scoring the probability of the given text to be of a certain language

    :param text: text to analyze
    :return: json scoring the chance of the text to be in each language
    """

    from LanguageIdentifier import rank

    prediction_raw = {}
    for lang, score in rank(text):
        prediction_raw[lang] = score
        
    prediction = max(zip(prediction_raw.values(), prediction_raw.keys()))[1]

    return { "prediction": prediction, "prediction_raw": prediction_raw}


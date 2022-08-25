import json

from LanguageIdentifier import rank


def predict(text: str) -> str:
    """
    From a given text, return a json scoring the probability of the given text to be of a certain language

    :param text: text to analyze
    :return: json scoring the chance of the text to be in each language
    """

    output = []

    for lang, score in rank(text):
        output.append({"language": lang, "score": score})

    return json.dumps(output)

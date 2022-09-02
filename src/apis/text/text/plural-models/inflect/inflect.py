from typing import Dict, Union

import inflect


def predict(word: str, count: Union[int, str]) -> Dict[str, str]:
    """
    Apply the `inflect.engine().plural` function to accord the `word` in plural regarding its `count`.\n

    As specified in `infect` documentation, the function return as follows:\n

    If count is one of the following words, then return text:
        1, a, an, one, each, every, this, that

    Args:
        word (str): The word to be pluralized.
        count (Union[int, str]): The count of the word.

    Returns:
        Dict[str, str]: The pluralized word.
    """

    p = inflect.engine()

    # if singular_noun is False
    # it means it is a singular noun
    if p.singular_noun(word) == False:
        result = p.plural(word, count)
    else:
        result = word

    return {"prediction": result, "prediction_raw": result}

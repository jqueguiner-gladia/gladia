import inflect


def predict(word: str, count: int) -> str:
    """
    Apply the `inflect.engine().plural` function to accord the `word` in plural regarding its `count`.\n

    As specified in `infect` documentation, the function return as follows:\n

    If count is one of the following words, then return text:
        1, a, an, one, each, every, this, that


    :param word: word to return the plural of
    :param count: count of `word`
    :return: word accorded with count
    """

    p = inflect.engine()

    return p.plural(word, count)

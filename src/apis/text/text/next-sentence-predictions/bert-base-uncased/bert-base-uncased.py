def predict(sentence_1: str, sentence_2: str) -> float:
    """
    Tell for a given sencence_2, whether or not it follows sentence_1

    :param sentence_1: preceding sentence
    :param sentence_2: sentence to check
    :return: confidence score, >= 0.5 if sentence_2 follows sentence_1, else < 0
    """

    from happytransformer import HappyNextSentence

    happy_ns = HappyNextSentence("BERT", "bert-base-uncased")

    result = happy_ns.predict_next_sentence(sentence_1, sentence_2)

    return result

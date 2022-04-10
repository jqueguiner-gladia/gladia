import nltk

from nltk.stem import WordNetLemmatizer

def predict(sentence: str) -> [str]:
    """
    Lemmatize a given sentence

    :param sentence: sentence to lemmatize
    :return: lemmatized sentence
    """

    lemmatizer = WordNetLemmatizer()

    word_list = nltk.word_tokenize(sentence)

    return [lemmatizer.lemmatize(w) for w in word_list]

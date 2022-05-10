import nltk

from nltk.stem import WordNetLemmatizer

nltk.download('punkt')
nltk.download('wordnet')
nltk.download('omw-1.4')

def predict(sentence: str) -> [str]:
    """
    Lemmatize a given sentence

    :param sentence: sentence to lemmatize
    :return: lemmatized sentence
    """

    lemmatizer = WordNetLemmatizer()

    word_list = nltk.word_tokenize(sentence)

    return [lemmatizer.lemmatize(w) for w in word_list]

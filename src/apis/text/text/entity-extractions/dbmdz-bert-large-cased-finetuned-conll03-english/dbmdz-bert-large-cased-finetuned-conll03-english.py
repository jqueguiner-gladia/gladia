from transformers import pipeline


def predict(input_string: str) -> [dict]:
    """
    Extract the named entity from a given string

    :param input_string: string to extract the entities from
    :return: entities founded in the string
    """

    ner_pipeline = pipeline("ner")
    entities = ner_pipeline(input_string)
    
    return entities

from transformers import pipeline

def predict(input_string):

    ner_pipeline = pipeline("ner")
    entities = ner_pipeline(input_string)
    
    return entities
import json

from happytransformer import HappyTextClassification


def predict(text):
    happy_tc = HappyTextClassification("DISTILBERT", "distilbert-base-uncased", num_labels=2)    
    result = happy_tc.classify_text(text)

    return json.dumps({"label": "POSITIVE" if result.label == "LABEL_0"  else "NEGATIVE", "score": result.score})

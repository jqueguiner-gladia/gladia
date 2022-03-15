import json

from happytransformer import HappyTextClassification


def predict(text):
    happy_tc = HappyTextClassification(model_type="DISTILBERT",  model_name="distilbert-base-uncased-finetuned-sst-2-english")
    result = happy_tc.classify_text(text)

    return json.dumps({"label": result.label, "score": result.score})

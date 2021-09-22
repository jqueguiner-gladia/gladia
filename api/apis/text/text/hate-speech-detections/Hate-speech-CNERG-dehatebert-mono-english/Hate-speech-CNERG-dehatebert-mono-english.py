from happytransformer import HappyTextClassification
import json

def predict(text):
    happy_tc = HappyTextClassification("BERT", "Hate-speech-CNERG/dehatebert-mono-english", 2)

    result = happy_tc.classify_text(text)

    return json.dumps({'label': 'NOT HATE' if result.label == 'LABEL_0' else 'HATE', 'score': result.score})
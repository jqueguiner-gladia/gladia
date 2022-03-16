import torch

from transformers import BertTokenizer, BertForSequenceClassification


def predict(text):
    labels = ["hate-speech", "normal", "offensive"]
    model_name = 'Hate-speech-CNERG/bert-base-uncased-hatexplain'

    device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')

    model = BertForSequenceClassification.from_pretrained(model_name)
    model.to(device)

    tokenizer = BertTokenizer.from_pretrained(model_name)

    inputs = tokenizer(text, return_tensors="pt")

    outputs = model(**inputs)

    return labels[torch.argmax(outputs.logits, dim=1)]

import numpy as np
import redisai
import torch
from transformers import AutoTokenizer, BertForQuestionAnswering
from icecream import ic

r = redisai.Client(host="localhost", port=6379)

model_file = "traced_bert_qa.pt"
bert_name = "bert-large-uncased-whole-word-masking-finetuned-squad"


def predict(content, source, target):
    # ic(r.modelscan())
    if not r.exists("bert-qa"):
        pass
    # a = r.modelget('bert-qa', meta_only=True)
    # ic(a)
    return ""

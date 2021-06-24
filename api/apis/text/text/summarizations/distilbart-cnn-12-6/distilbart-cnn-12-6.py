import os
import pickle
import time
from pathlib import Path
from typing import List

import redis
from ai_api_utils.model_management import download_model
from ai_api_utils.system import path_to_absolute
from icecream import ic
from transformers import AutoTokenizer, AutoModelForSeq2SeqLM





def predict(input_string, source_language, min_length, max_length):

    tokenizer = AutoTokenizer.from_pretrained("sshleifer/distilbart-cnn-12-6")

    model = AutoModelForSeq2SeqLM.from_pretrained("sshleifer/distilbart-cnn-12-6")

    inputs = tokenizer(input_string, return_tensors="pt")

    inputs = tokenizer.encode(input_string, return_tensors="pt", max_length=max_length, truncation=True)
    outputs = model.generate(inputs, max_length=max_length, min_length=min_length, length_penalty=2.0, num_beams=4, early_stopping=True)
    
    return tokenizer.decode(outputs[0]).replace('<s>','').replace('</s>','').strip()
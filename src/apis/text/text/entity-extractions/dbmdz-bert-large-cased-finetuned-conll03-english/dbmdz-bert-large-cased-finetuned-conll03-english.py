import os
import numpy as np
import tritonclient.http as tritonclient

from time import sleep
from warnings import warn
from transformers import AutoTokenizer
from gladia_api_utils.download_active_models import download_triton_model


def softmax(x):
    return np.exp(x) / np.sum(np.exp(x))


def predict(input_string: str) -> [dict]:
    """
    Extract the named entity from a given string

    :param input_string: string to extract the entities from
    :return: entities founded in the string
    """

    LABELS = ["O", "B-MISC", "I-MISC", "B-PER", "I-PER", "B-ORG", "I-ORG", "B-LOC", "I-LOC"]
    MODEL_NAME = "ner_bert-large-cased-finetuned-conll03-english_base_traced"
    TOKENIZER_NAME = 'dbmdz/bert-large-cased-finetuned-conll03-english'
    TRITON_SEVER_URL = os.getenv("TRITON_SERVER_URL", default='localhost:8000')

    triton_model_name = open(os.path.join(os.path.split(__file__)[0], ".git_path")).read().split("/")[-1]
    if not os.path.exists(os.path.join(os.getenv('TRITON_MODELS_PATH'), triton_model_name)):
        warn('Downloading model from hugging-face, to prevent lazy downloading please specify TRITON_LAZY_DOWNLOAD=False')
    
        download_triton_model(
            triton_models_dir=os.getenv('TRITON_MODELS_PATH'),
            git_path=".git_path"
        )

        sleep(15)

    client = tritonclient.InferenceServerClient(url=TRITON_SEVER_URL, verbose=False)

    tokenizer = AutoTokenizer.from_pretrained(TOKENIZER_NAME)

    input_ids = tokenizer(input_string, return_tensors="pt", max_length=256, padding="max_length").input_ids

    input0 = tritonclient.InferInput('input__0', (1, 256), 'INT32')
    input0.set_data_from_numpy(input_ids.detach().numpy().astype(np.int32))

    output0 = tritonclient.InferRequestedOutput('output__0')

    response = client.infer(MODEL_NAME, model_version='1', inputs=[input0], outputs=[output0])
    outputs = response.as_numpy("output__0")[0]

    predictions = []
    for (idx, output), token in zip(enumerate(outputs), tokenizer.batch_decode(input_ids.permute(dims=(1,0)))):
        if token in ["[CLS]", "[SEP]", "[PAD]"]:
            continue
            
        label = LABELS[np.argmax(output)]

        if label == "O":
            continue
        
        pred = {
            'entity': label,
            'score': np.max(softmax(output)),
            'index': idx,
            'word': token
        }

        predictions.append(pred)

    return predictions

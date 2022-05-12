import os
import numpy as np
import tritonclient.http as tritonclient

from time import sleep
from warnings import warn
from transformers import BertTokenizer
from gladia_api_utils.download_active_models import download_triton_model


def predict(text: str) -> str:
    """
    Detect hate from a given text

    :param text: text to analyze
    :return: normal, hate-speech or offensive regarding the level of hate in the text
    """

    LABELS = ["hate-speech", "normal", "offensive"]
    MODEL_NAME = "hate-speech-detection_bert-base-uncased-hatexplain_base_traced"
    TOKENIZER_NAME = 'Hate-speech-CNERG/bert-base-uncased-hatexplain'
    TRITON_SEVER_URL = os.getenv("TRITON_SERVER_URL", default='localhost:8000')

    triton_model_name = open(".git_path").read().split("/")[-1]
    if not os.path.exists(os.path.join(os.getenv('TRITON_MODELS_PATH'), triton_model_name)):
        warn('Downloading model from hugging-face, to prevent lazy downloading please specify TRITON_LAZY_DOWNLOAD=False')
    
        download_triton_model(
            triton_models_dir=os.getenv('TRITON_MODELS_PATH'),
            git_path=".git_path"
        )

        sleep(15)

    client = tritonclient.InferenceServerClient(url=TRITON_SEVER_URL, verbose=False)

    tokenizer = BertTokenizer.from_pretrained(TOKENIZER_NAME)

    input_ids = tokenizer(text, return_tensors="pt", max_length=256, padding="max_length").input_ids

    input0 = tritonclient.InferInput('input__0', (1, 256), 'INT32')
    input0.set_data_from_numpy(input_ids.detach().numpy().astype(np.int32))

    output0 = tritonclient.InferRequestedOutput('output__0')

    response = client.infer(MODEL_NAME, model_version='1', inputs=[input0], outputs=[output0])
    output = response.as_numpy("output__0")[0]

    return LABELS[np.argmax(output)]

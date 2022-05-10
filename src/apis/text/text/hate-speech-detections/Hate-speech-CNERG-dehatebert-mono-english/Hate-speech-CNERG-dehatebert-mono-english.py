import os
import numpy as np
import tritonclient.http as tritonhttpclient

from transformers import BertTokenizer


def predict(text: str) -> str:
    """
    Detect hate from a given text

    :param text: text to analyze
    :return: normal, hate-speech or offensive regarding the level of hate in the text
    """

    LABELS = ["hate-speech", "normal", "offensive"]
    MODEL_NAME = "bert-base-uncased-hatexplain"
    TOKENIZER_NAME = 'Hate-speech-CNERG/bert-base-uncased-hatexplain'
    TRITON_SEVER_URL = os.getenv("TRITON_SEVER_URL", default='localhost:8000')

    triton_client = tritonhttpclient.InferenceServerClient(url=TRITON_SEVER_URL, verbose=False)

    tokenizer = BertTokenizer.from_pretrained(TOKENIZER_NAME)

    input_ids = tokenizer(text, return_tensors="pt", max_length=256, padding="max_length").input_ids

    input0 = tritonhttpclient.InferInput('input__0', (1, 256), 'INT32')
    input0.set_data_from_numpy(input_ids.detach().numpy().astype(np.int32), binary_data=False)

    output0 = tritonhttpclient.InferRequestedOutput('output__0',  binary_data=False)

    response = triton_client.infer(MODEL_NAME, model_version='1', inputs=[input0], outputs=[output0])
    output = response.as_numpy("output__0")[0]

    return LABELS[np.argmax(output)]

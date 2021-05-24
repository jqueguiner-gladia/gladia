import gradio as gr
import requests
import json

import os
import io
from PIL import Image
import matplotlib.pyplot as plt

headers = {}


sotas = requests.get(f"http://$IP:$PORT/openapi.json", headers=headers)
sotas = sotas.json()
apis = sotas["paths"]
apis = [apis[0]]
for path, api in apis.items():
    split_path = path.split("/")
    split_path.pop(0)
    split_path.pop()
    print(split_path)

    input_type, output_type, task = split_path
    response = requests.get(
        f"http://$IP:$PORT/{input_type}/{output_type}/{task}/",
        headers=headers,
    )
    versions = response.json()

    inputs = gr.inputs.Image(label=f"Input Image for {task}")

    outputs = list()
    for version in versions:
        outputs.append(
            gr.outputs.Image(label=f"{task} for version {version}", type="pil")
        )

    def predict(im):

        for version in versions:
            params = (("model", version),)

            byte_io = io.BytesIO()
            plt.imsave(byte_io, im, format="png")
            byte_io.seek(0)
            files = {
                "file": (
                    "11A3FE95-49A7-4003-827C-C77B267FA862.png",
                    byte_io,
                    "image/png",
                )
            }

            response = requests.get(
                f"http://$IP:$PORT/{input_type}/{output_type}/{task}/",
                headers=headers,
            )

            outputs.append(Image.open(io.BytesIO(response.content)))
        return outputs

    iface = gr.Interface(
        fn=predict,
        inputs=inputs,
        outputs=outputs,
        server_port=7860,
        server_name=os.environ["IP"],
        examples=[["1.jpg"], ["2.jpg"]],
    )
iface.launch()

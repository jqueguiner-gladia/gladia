import io
import warnings

import stability_sdk.interfaces.gooseai.generation.generation_pb2 as generation
from gladia_api_utils import SECRETS
from PIL import Image
from stability_sdk import client


def predict(
    prompt="A high tech solarpunk utopia in the Amazon rainforest",
    samples=1,
    steps=40,
    scale=7.0,
    seed=396916372,
) -> Image:

    stability_api = client.StabilityInference(
        key=SECRETS["STABILITY_KEY"],
        verbose=True,
    )

    answers = stability_api.generate(prompt=prompt, samples=samples, steps=steps)

    # TODO implement multiple samples

    for resp in answers:
        for artifact in resp.artifacts:
            if artifact.finish_reason == generation.FILTER:
                warnings.warn(
                    "Your request activated the API's safety filters and could not be processed."
                    "Please modify the prompt and try again."
                )
                img = Image.open("unsafe.png")
            if artifact.type == generation.ARTIFACT_IMAGE:
                img = Image.open(io.BytesIO(artifact.binary))

    return img

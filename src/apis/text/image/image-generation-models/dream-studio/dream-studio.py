import io
import warnings

import stability_sdk.interfaces.gooseai.generation.generation_pb2 as generation
from gladia_api_utils import SECRETS
from PIL import Image
from stability_sdk import client
from logging import getLogger

logger = getLogger(__name__)


def predict(
    prompt="A high tech solarpunk utopia in the Amazon rainforest",
    samples=1,
    steps=40,
    scale=7.0,
    seed=396916372,
) -> Image:
    """
    Generate an image using the generation service from the dream studio project.
    Returns a SFW PIL image.
    if NSFW will return a NSFW Warning PIL image.
    to be used this function you need to have a valid STABILITY_API_KEY set in the environment variables.
    get the STABILITY_API_KEY at https://beta.dreamstudio.ai/dream/membership

    Args:
        prompt (str): The prompt to use for the generation service
        samples (int): The number of samples to generate from the generation service. >1 Not supported so far (default: 1)
        steps (int): The number of steps to use for the generation service (higher is better)
        scale (float): The scale to use for the generation service (recommended between 0.0 and 15.0)
        seed (int): The seed to use for the generation service (default: 396916372)

    Returns:
        Image: The generated image
    """

    stability_api = client.StabilityInference(
        key=SECRETS["STABILITY_KEY"],
        verbose=True,
    )

    answers = stability_api.generate(prompt=prompt, samples=samples, steps=steps)

    # TODO implement multiple samples

    for resp in answers:
        for artifact in resp.artifacts:
            if artifact.finish_reason == generation.FILTER:
                logger.warning(
                    "Your request activated the API's safety filters and could not be processed."
                    "Please modify the prompt and try again."
                )
                img = Image.open("unsafe.png")
            if artifact.type == generation.ARTIFACT_IMAGE:
                img = Image.open(io.BytesIO(artifact.binary))

    return img

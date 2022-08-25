from gladia_api_utils import SECRETS
from PIL import Image


def predict(
    prompt="A high tech solarpunk utopia in the Amazon rainforest",
    samples=1,
    steps=40,
    scale=7.5,
    seed=396916372,
) -> Image:

    import torch
    from diffusers import StableDiffusionPipeline
    from torch import autocast

    model_id = "CompVis/stable-diffusion-v1-4"
    device = "cuda"

    pipe = StableDiffusionPipeline.from_pretrained(
        model_id,
        use_auth_token=SECRETS["HUGGINGFACE_ACCESS_TOKEN"],
        revision="fp16",
        torch_dtype=torch.float16,
    ).to(device)

    generator = torch.Generator(device=device).manual_seed(seed)

    with autocast("cuda"):
        images_list = pipe(
            [prompt] * samples,
            num_inference_steps=steps,
            guidance_scale=scale,
            generator=generator,
        )

    # TODO implement NSFW filter
    # {'sample': [<PIL.Image.Image image mode=RGB size=512x512 at 0x7F546A97A070>], 'nsfw_content_detected': [False]}
    # TODO implement multiple samples

    return images_list["sample"][0]

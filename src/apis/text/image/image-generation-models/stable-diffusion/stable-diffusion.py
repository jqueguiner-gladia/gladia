import os

from PIL import Image


def predict(
    prompt="A high tech solarpunk utopia in the Amazon rainforest",
    samples=1,
    steps=40,
    scale=7.5,
    seed=396916372,
) -> Image:
    import torch
    from datasets import load_dataset
    from diffusers import StableDiffusionPipeline
    from torch import autocast

    model_id = "CompVis/stable-diffusion-v1-4"
    device = "cuda"

    pipe = StableDiffusionPipeline.from_pretrained(
        model_id,
        use_auth_token=os.getenv("HUGGINGFACE_ACCESS_TOKEN"),
        revision="fp16",
        torch_dtype=torch.float16,
    )
    pipe = pipe.to(device)

    # word_list_dataset = load_dataset("stabilityai/word-list", data_files="list.txt", use_auth_token=os.getenv('HUGGINGFACE_ACCESS_TOKEN'))
    # word_list = word_list_dataset["train"]['text']

    # for filter in word_list:
    #    if re.search(rf"\b{filter}\b", prompt):
    #        raise Exception("Unsafe content found. Please try again with different prompts.")

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

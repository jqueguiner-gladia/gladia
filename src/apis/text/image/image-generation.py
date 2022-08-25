from fastapi import APIRouter
from gladia_api_utils.submodules import TaskRouter

inputs = [
    {
        "type": "text",
        "name": "prompt",
        "example": "the Eiffel tower landing on the moon",
        "placeholder": "Prompt to generate image from",
    },
    {
        "type": "int",
        "name": "samples",
        "default": 1,
        "example": 1,
        "placeholder": "Number of predictions",
    },
    {
        "type": "int",
        "name": "steps",
        "default": 40,
        "example": 40,
        "placeholder": "Number of steps",
    },
    {
        "type": "int",
        "name": "seed",
        "default": 396916372,
        "example": 396916372,
        "placeholder": "Seed for predictions",
    },
]

output = {"name": "generatedimage", "type": "image", "example": ""}

router = APIRouter()

TaskRouter(router=router, input=inputs, output=output, default_model="stable-diffusion")

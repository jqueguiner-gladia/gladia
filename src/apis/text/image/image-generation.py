from fastapi import APIRouter
from gladia_api_utils.submodules import TaskRouter

inputs = [
    {
        "type": "string",
        "name": "prompt",
        "example": "the Eiffel tower landing on the moon",
        "placeholder": "Prompt to generate image from",
    },
    {
        "type": "integer",
        "name": "samples",
        "default": 1,
        "example": 1,
        "placeholder": "Number of predictions",
    },
    {
        "type": "integer",
        "name": "steps",
        "default": 40,
        "example": 40,
        "placeholder": "Number of steps",
    },
    {
        "type": "integer",
        "name": "seed",
        "default": 396916372,
        "example": 396916372,
        "placeholder": "Seed for predictions",
    },
]

output = {"name": "generatedimage", "type": "image", "example": ""}

router = APIRouter()

TaskRouter(router=router, input=inputs, output=output, default_model="stable-diffusion")

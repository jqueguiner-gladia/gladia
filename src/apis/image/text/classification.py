from fastapi import APIRouter
from gladia_api_utils.submodules import TaskRouter

inputs = [
    {
        "type": "image",
        "name": "image",
        "example": "http://files.gladia.io/test/test.png",
        "placeholder": "Image to classify",
    },
    {
        "type": "int",
        "name": "top_k",
        "default": 1,
        "example": 1,
        "placeholder": "Top K",
    },
]

output = {
    "name": "classified_image",
    "type": "str",
    "example": "bow tie",
}

router = APIRouter()

TaskRouter(router=router, input=inputs, output=output, default_model="alexnet")

from fastapi import APIRouter
from gladia_api_utils.submodules import TaskRouter

inputs = [
    {
        "type": "image",
        "name": "image",
        "example": "http://files.gladia.io/examples/image/text/classification/image.png",
        "examples": [
            "http://files.gladia.io/examples/image/text/classification/image.gif",
            "http://files.gladia.io/examples/image/text/classification/image.jpg",
            "http://files.gladia.io/examples/image/text/classification/image.png"
        ],
        "placeholder": "Image to classify",
    },
    {
        "type": "integer",
        "name": "top_k",
        "default": 1,
        "example": 1,
        "placeholder": "Top K",
    },
]

output = {
    "name": "classified_image",
    "type": "string",
    "example": "bow tie",
}

router = APIRouter()

TaskRouter(router=router, input=inputs, output=output, default_model="alexnet")

from fastapi import APIRouter
from gladia_api_utils.submodules import TaskRouter

inputs = [
    {
        "type": "image",
        "name": "image",
        "example": "http://files.gladia.io/test/test.png",
        "examples": [
            "http://files.gladia.io/examples/image/image/face-bluring/face-bluring.gif",
            "http://files.gladia.io/examples/image/image/face-bluring/face-bluring.jpg",
            "http://files.gladia.io/examples/image/image/face-bluring/face-bluring.png"
        ],
        "placeholder": "Image to blur face from",
    }
]

output = {"name": "blured_image", "type": "image", "example": "image"}

router = APIRouter()

TaskRouter(router=router, input=inputs, output=output, default_model="ageitgey")

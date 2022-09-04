from fastapi import APIRouter
from gladia_api_utils.submodules import TaskRouter

inputs = [
    {
        "type": "image",
        "name": "image",
        "example": "http://files.gladia.io/examples/image/image/uncolorization/landscape-pics-wallpapers.png",
        "examples": [
            "http://files.gladia.io/examples/image/image/uncolorization/landscape-pics-wallpapers.gif",
            "http://files.gladia.io/examples/image/image/uncolorization/landscape-pics-wallpapers.jpg",
            "http://files.gladia.io/examples/image/image/uncolorization/landscape-pics-wallpapers.png",
        ],
        "placeholder": "Image to uncolorize",
    }
]

output = {"name": "uncolorized_image", "type": "image", "example": "uncolorized_image"}

router = APIRouter()

TaskRouter(router=router, input=inputs, output=output, default_model="v1")

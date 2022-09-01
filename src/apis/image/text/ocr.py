from fastapi import APIRouter
from gladia_api_utils.submodules import TaskRouter

inputs = [
    {
        "type": "image",
        "name": "image",
        "example": "http://files.gladia.io/examples/image/text/ocr/testocr.png",
        "examples": [
            "http://files.gladia.io/examples/image/text/ocr/testocr.gif",
            "http://files.gladia.io/examples/image/text/ocr/testocr.jpg",
            "http://files.gladia.io/examples/image/text/ocr/testocr.png"
        ],
        "placeholder": "Image to extract text from",
    },
    {
        "type": "string",
        "name": "source_language",
        "default": "en",
        "example": "en",
        "placeholder": "Source language",
    },
]

output = {"name": "extracted_text", "type": "string", "example": "extracted_text"}

router = APIRouter()

TaskRouter(router=router, input=inputs, output=output, default_model="easy-ocr")

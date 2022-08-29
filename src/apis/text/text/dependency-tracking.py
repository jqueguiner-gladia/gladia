from fastapi import APIRouter
from gladia_api_utils.submodules import TaskRouter

router = APIRouter()

inputs = [
    {
        "type": "string",
        "name": "input_string",
        "example": "Text to analyzed",
        "placeholder": "Insert the text to analyze here",
    }
]

output = {"name": "analyzed_text", "type": "string", "example": "analyzed_text"}


TaskRouter(router=router, input=inputs, output=output, default_model="LAL-Parser")

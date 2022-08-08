from fastapi import APIRouter
from gladia_api_utils.submodules import TaskRouter

router = APIRouter()

inputs = [
    {
        "type": "text",
        "name": "sentence",
        "default": "Text to corrcte",
        "placeholder": "Insert the text to correct",
    }
]

output = {"name": "corrected_text", "type": "str", "example": "corrected_text"}

TaskRouter(
    router=router,
    input=inputs,
    output=output,
    default_model="flexudy-t5-base-multi-sentence-doctor",
)

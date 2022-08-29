from fastapi import APIRouter
from gladia_api_utils.submodules import TaskRouter

router = APIRouter()

inputs = [
    {
        "type": "string",
        "name": "sentence",
        "example": "Text to corrcte",
        "placeholder": "Insert the text to correct",
    }
]

output = {"name": "corrected_text", "type": "string", "example": "corrected_text"}

TaskRouter(
    router=router,
    input=inputs,
    output=output,
    default_model="flexudy-t5-base-multi-sentence-doctor",
)

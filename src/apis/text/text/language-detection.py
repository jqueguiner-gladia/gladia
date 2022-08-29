from fastapi import APIRouter
from gladia_api_utils.submodules import TaskRouter

router = APIRouter()

inputs = [
    {
        "type": "string",
        "name": "text",
        "example": "Input text to perform language detection on",
        "placeholder": "Insert the text to perform language detection on",
    }
]

output = {"name": "generated_text", "type": "string", "example": "en"}

TaskRouter(
    router=router,
    input=inputs,
    output=output,
    default_model="xlm-roberta-base-language-detection",
)

from fastapi import APIRouter
from gladia_api_utils.submodules import TaskRouter

router = APIRouter()

inputs = [
    {
        "type": "string",
        "name": "sentence",
        "example": "I think therefore I",
        "placeholder": "Insert the text to find the next word from.",
    },
    {
        "type": "integer",
        "name": "top_k",
        "default": 3,
        "example": 3,
        "placeholder": "Top K",
    },
]

output = {"name": "next_word", "type": "string", "example": "next word"}


TaskRouter(
    router=router, input=inputs, output=output, default_model="distilbert-base-uncased"
)

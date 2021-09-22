from fastapi import APIRouter
from ai_api_utils.submodules import TaskRouter

router = APIRouter()

input = [
    {
        "type": "text",
        "name": "sentence",
        "default": "I think therefore I",
        "placeholder": "I think therefore I",
        "tooltip": "Insert the text to find the next word from.",
    },
]

output = [
    {
        "type": str,
        "name": "answer",
    }
]


TaskRouter(router=router, input=input, output=output, default_model="distilbert-base-uncased")

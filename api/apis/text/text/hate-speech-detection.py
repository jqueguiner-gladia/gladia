from fastapi import APIRouter
from ai_api_utils.submodules import TaskRouter

router = APIRouter()

input = [
    {
        "type": "text",
        "name": "text",
        "default": "GPT-2 was created by OpenAI",
        "placeholder": "GPT-2 was created by OpenAI",
        "tooltip": "Insert the text to classify as hate or not",
    },
]

output = [
    {
        "type": str,
        "name": "classified text.",
    }
]

TaskRouter(router=router, input=input, output=output, default_model="Hate-speech-CNERG-dehatebert-mono-english")

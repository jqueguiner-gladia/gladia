from fastapi import APIRouter
from ai_api_utils.submodules import TaskRouter

router = APIRouter()

input = [
    {
        "type": "text",
        "name": "text",
        "default": "Input text to start generation from",
        "placeholder": "Input text to start generation from",
        "tooltip": "Insert the text to generate from",
    },
]

output = [
    {
        "type": str,
        "name": "generated_text",
    }
]

TaskRouter(router=router, input=input, output=output, default_model="EleutherAI-gpt-neo-2_7B")

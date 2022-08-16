from fastapi import APIRouter
from gladia_api_utils.submodules import TaskRouter

router = APIRouter()

inputs = [
    {
        "type": "text",
        "name": "text",
        "example": "Input text to start generation from",
        "placeholder": "Insert the text to generate from",
    }
]

output = {"name": "generated_text", "type": "str", "example": "generated_text"}

TaskRouter(
    router=router, input=inputs, output=output, default_model="EleutherAI-gpt-neo-2_7B"
)

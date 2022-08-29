from fastapi import APIRouter
from gladia_api_utils.submodules import TaskRouter

router = APIRouter()

inputs = [
    {
        "type": "string",
        "name": "text",
        "example": "I like you. I love you",
        "placeholder": "Insert the text to anlayse sentiment from",
    }
]

output = {"name": "results", "type": "string", "example": "results"}

TaskRouter(
    router=router,
    input=inputs,
    output=output,
    default_model="mrm8488-t5-base-finetuned-emotion",
)

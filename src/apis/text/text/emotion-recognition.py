from fastapi import APIRouter
from gladia_api_utils.submodules import TaskRouter
from pydantic import BaseModel

router = APIRouter()

inputs = [
    {
        "type": "text",
        "name": "text",
        "example": "I like you. I love you",
        "placeholder": "Insert the text to anlayse sentiment from",
    }
]

output = {"name": "results", "type": "str", "example": "results"}

TaskRouter(
    router=router,
    input=inputs,
    output=output,
    default_model="mrm8488-t5-base-finetuned-emotion",
)

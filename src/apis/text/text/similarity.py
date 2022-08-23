from fastapi import APIRouter
from gladia_api_utils.submodules import TaskRouter

router = APIRouter()

inputs = [
    {
        "type": "text",
        "name": "sentence_1",
        "example": "I like banana",
        "placeholder": "Insert the first text to compare here",
    },
    {
        "type": "text",
        "name": "sentence_2",
        "example": "I hate banana",
        "placeholder": "Insert the second text to compare here",
    },
]

output = {"name": "similarity", "type": "float", "example": "0.6724814772605896"}

TaskRouter(router=router, input=inputs, output=output, default_model="all-MiniLM-L6-v2")

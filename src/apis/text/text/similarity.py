from fastapi import APIRouter
from gladia_api_utils.submodules import TaskRouter

router = APIRouter()

inputs = [
    {
        "type": "text",
        "name": "sentence_1",
        "default": "I like Python because I can build AI applications",
        "placeholder": "Insert the first text to compare here",
    },
    {
        "type": "text",
        "name": "sentence_2",
        "default": "Second sentence to compare to",
        "placeholder": "Insert the second text to compare here",
    },
]

output = {"name": "similarity", "type": "float", "example": "similarity"}

TaskRouter(router=router, input=inputs, output=output, default_model="all-MiniLM-L6-v2")

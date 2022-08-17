from fastapi import APIRouter
from gladia_api_utils.submodules import TaskRouter

router = APIRouter()

inputs = [
    {
        "type": "text",
        "name": "word",
        "example": "cat",
        "placeholder": "Insert the word to pluralize here",
    },
    {
        "type": "int",
        "name": "count",
        "default": 2,
        "example": 2,
        "placeholder": "Insert the number associated to the word to pluralize",
    },
]

output = {"name": "pluralized_text", "type": "str", "example": "cats"}

TaskRouter(router=router, input=inputs, output=output, default_model="inflect")

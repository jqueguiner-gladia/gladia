from fastapi import APIRouter
from gladia_api_utils.submodules import TaskRouter

router = APIRouter()

inputs = [
    {
        "type": "text",
        "name": "sentence_1",
        "default": "",
        "placeholder": "",
    },
    {
        "type": "text",
        "name": "sentence_2",
        "default": "",
        "placeholder": "",
    },
]

output = {"name": "similarity", "type": "float", "example": "similarity"}

TaskRouter(router=router, input=inputs, output=output, default_model="all-MiniLM-L6-v2")

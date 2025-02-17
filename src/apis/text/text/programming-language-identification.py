from fastapi import APIRouter
from gladia_api_utils.submodules import TaskRouter

router = APIRouter()

inputs = [
    {
        "type": "string",
        "name": "text",
        "example": "def is_palendrome(s):",
        "placeholder": "Input code to get programing language from",
    }
]

output = {"name": "classified_code", "type": "array", "example": "classified_code"}

TaskRouter(router=router, input=inputs, output=output, default_model="aliostad")

from fastapi import APIRouter
from ai_api_utils.submodules import TaskRouter

router = APIRouter()

input = [
    {
        "type": "text",
        "name": "text",
        "default": "def is_palendrome(s):",
        "placeholder": "Input code to get programing language from",
        "tooltip": "Input code to get programing language from",
    },
]

output = [
    {
        "type": str,
        "name": "classified_code",
    }
]

TaskRouter(router=router, input=input, output=output, default_model="aliostad")

from fastapi import APIRouter
from ai_api_utils.submodules import TaskRouter

router = APIRouter()

input = [
    {
        "type": "text",
        "name": "code_snippet",
        "default": "def is_palendrome(s):",
        "placeholder": "Input code to start generation from",
        "tooltip": "Insert the code to generate from",
    },
]

output = [
    {
        "type": str,
        "name": "generated_code",
    }
]

TaskRouter(router=router, input=input, output=output, default_model="sentdex-GPyT")

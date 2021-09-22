from fastapi import APIRouter
from ai_api_utils.submodules import TaskRouter

router = APIRouter()

input = [
    {
        "type": "text",
        "name": "text",
        "default": "Лорем ипсум долор сит амет",
         "tooltip": "Insert the text to transliterate here",
    },
    {
        "type": "text",
        "name": "language",
        "default": "ru",
        "tooltip": "Insert the language code here",
    },
]

output = [
    {
        "type": str,
        "name": "transliterated text.",
    }
]

TaskRouter(router=router, input=input, output=output, default_model="transliterate")

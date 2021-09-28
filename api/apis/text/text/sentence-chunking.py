from fastapi import APIRouter
from ai_api_utils.submodules import TaskRouter

router = APIRouter()

input = [
    {
        "type": "text",
        "name": "sentence",
        "default": "My name is Clara and I live in Berkeley.",
        "placeholder": "Sentence to build the chunking tree on",
        "tooltip": "Insert the sentence to build the chunking tree on",
    },
]

output = [
    {
        "type": str,
        "name": "chunking tree",
    }
]


TaskRouter(router=router, input=input, output=output, default_model="conll2000")

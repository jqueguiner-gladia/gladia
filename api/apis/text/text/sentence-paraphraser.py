from fastapi import APIRouter
from ai_api_utils.submodules import TaskRouter

router = APIRouter()

input = [
    {
        "type": "text",
        "name": "context",
        "default": "Once, a group of frogs was roaming around the forest in search of water.",
        "placeholder": "Once, a group of frogs was roaming around the forest in search of water.",
        "tooltip": "Insert the text to paraphrase here",
    },
]

output = [
    {
        "type": str,
        "name": "paraphrased_text",
    }
]


TaskRouter(router=router, input=input, output=output, default_model="ramsrigouthamg-t5-large-paraphraser-diverse-high-quality")

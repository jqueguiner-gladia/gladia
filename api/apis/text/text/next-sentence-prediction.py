from fastapi import APIRouter
from ai_api_utils.submodules import TaskRouter

router = APIRouter()

input = [
    {
        "type": "text",
        "name": "sentence_1",
        "default": "I like you.",
        "placeholder": "I like you. I love you.",
        "tooltip": "Insert the first sentence",
    },
    {
        "type": "text",
        "name": "sentence_2",
        "default": "But it's not about you.",
        "placeholder": "But it's not about you.",
        "tooltip": "Insert the second sentence to estimate the probability from",
    },
]

output = [
    {
        "type": list,
        "name": "results",
    }
]

TaskRouter(router=router, input=input, output=output, default_model="bert-base-uncased")

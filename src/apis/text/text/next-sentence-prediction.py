from fastapi import APIRouter
from gladia_api_utils.submodules import TaskRouter

router = APIRouter()

inputs = [
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

output = {"name": "results", "type": "list", "example": "results"}

TaskRouter(
    router=router, input=inputs, output=output, default_model="bert-base-uncased"
)

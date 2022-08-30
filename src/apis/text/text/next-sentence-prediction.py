from fastapi import APIRouter
from gladia_api_utils.submodules import TaskRouter

router = APIRouter()

inputs = [
    {
        "type": "string",
        "name": "sentence_1",
        "example": "I like you.",
        "placeholder": "Insert the first sentence",
    },
    {
        "type": "string",
        "name": "sentence_2",
        "example": "But it's not about you.",
        "placeholder": "Insert the second sentence to estimate the probability from",
    },
]

output = {
    "name": "next_sentence_probability",
    "type": "number",
    "example": "0.999984622001648",
}

TaskRouter(
    router=router, input=inputs, output=output, default_model="bert-base-uncased"
)

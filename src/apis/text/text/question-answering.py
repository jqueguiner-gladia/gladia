from fastapi import APIRouter
from gladia_api_utils.submodules import TaskRouter

router = APIRouter()

inputs = [
    {
        "type": "string",
        "name": "context",
        "example": "My name is Clara and I live in Berkeley.",
        "placeholder": "Insert the text to extract answer from",
    },
    {
        "type": "string",
        "name": "question",
        "example": "What's my name?",
        "placeholder": "Insert the question to be answered",
    },
]

output = {"name": "answer", "type": "string", "example": "answer"}


TaskRouter(
    router=router,
    input=inputs,
    output=output,
    default_model="deepset_bert-base-cased-squad2",
)

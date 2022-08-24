from fastapi import APIRouter
from gladia_api_utils.submodules import TaskRouter

router = APIRouter()

inputs = [
    {
        "type": "text",
        "name": "text",
        "example": "I like you. I love you",
        "placeholder": "Insert the text to anlayse sentiment from",
    }
]

output = {"name": "sentiment", "type": "str", "example": "POSITIVE"}

TaskRouter(
    router=router,
    input=inputs,
    output=output,
    default_model="nlptown-bert-base-multilingual-uncased-sentiment",
)

from fastapi import APIRouter
from gladia_api_utils.submodules import TaskRouter

router = APIRouter()

inputs = [
    {
        "type": "string",
        "name": "text",
        "example": "i would like to find a flight from charlotte to las vegas that makes a stop in st. louis",
        "placeholder": "Insert the text to extract intent and slot from",
    }
]

output = {"name": "analyzed_text", "type": "array", "example": "analyzed_text"}


# TaskRouter(router=router, input=inputs, output=output, default_model="jointBERT-bert-atis")

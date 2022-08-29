from fastapi import APIRouter
from gladia_api_utils.submodules import TaskRouter

router = APIRouter()

inputs = [
    {
        "type": "string",
        "name": "input_string",
        "example": "Text to translate",
        "placeholder": "Insert the text to translate here",
    },
    {
        "type": "string",
        "name": "source_language",
        "example": "en",
        "placeholder": "Use the ISO 2 letters representation for source language",
    },
    {
        "type": "string",
        "name": "target_language",
        "example": "fr",
        "placeholder": "Use the ISO 2 letters representation for target language",
    },
]

output = {"name": "translated_text", "type": "string", "example": "translated_text"}

TaskRouter(router=router, input=inputs, output=output, default_model="Helsinki-NLP")

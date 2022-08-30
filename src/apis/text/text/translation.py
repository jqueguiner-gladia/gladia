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
        "example": "eng",
        "placeholder": "Use the ISO 3 letters (ISO 639-3) representation for source language",
    },
    {
        "type": "string",
        "name": "target_language",
        "example": "fra",
        "placeholder": "Use the ISO 3 letters (ISO 639-3) representation for target language",
    },
]

output = {
    "name": "translated_text",
    "type": "str",
    "example": '{"prediction": "Le texte à traduire",  "prediction_raw": "Le texte à traduire"}',
}

TaskRouter(
    router=router,
    input=inputs,
    output=output,
    default_model="facebook-nllb-200-distilled-600M",
)

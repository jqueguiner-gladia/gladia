from fastapi import APIRouter
from ai_api_utils.submodules import TaskRouter

router = APIRouter()

input = [
    {
        "type": "text",
        "name": "input_string",
        "default": "Text to summarize",
        "placeholder": "Text to summarize",
        "tooltip": "Insert the text to summarize here",
    },
    {
        "type": "text",
        "name": "source_language",
        "default": "en",
        "tooltip": "Please use the ISO 2 letters representation for source language",
    },
    {
        "type": "integer",
        "name": "max_length",
        "default": 512,
        "tooltip": "Maximum lenght of the summary",
    },
    {
        "type": "integer",
        "name": "min_length",
        "default": 40,
        "tooltip": "Minimum lenght of the summary",
    }
]

output = [
    {
        "type": str,
        "name": "summarized text.",
    }
]

TaskRouter(router=router, input=input, output=output, default_model="distilbart-cnn-12-6")

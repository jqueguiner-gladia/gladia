from fastapi import APIRouter
from gladia_api_utils.submodules import TaskRouter

from gladia_api_utils

router = APIRouter()

inputs = [
    {
        "type": "string",
        "name": "text",
        "example": "The Crown is a historical drama streaming television series about the reign of Queen Elizabeth II, created and principally written by Peter Morgan, and produced by Left Bank Pictures and Sony Pictures Television for Netflix.",
        "placeholder": "Insert the text to summarize here",
    }
]

output = {"name": "keywords", "type": "string", "example": "crown"}

TaskRouter(
    router=router,
    input=inputs,
    output=output,
    default_model="keybert-paraphrase-MiniLM-L6-v2",
)

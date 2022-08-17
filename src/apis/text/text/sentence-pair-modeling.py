from fastapi import APIRouter
from gladia_api_utils.submodules import TaskRouter

router = APIRouter()

inputs = [
    {
        "type": "text",
        "name": "sentence",
        "example": "Once, a group of frogs was roaming around the forest in search of water.",
        "placeholder": "Insert the sentence to perform the Pairwise Sentence Scoring Tasks",
    }
]

output = {"name": "analyzed_sentence", "type": "str", "example": "analyzed_sentence"}

TaskRouter(
    router=router, input=inputs, output=output, default_model="UKPLab-all-MiniLM-L6-v2"
)

from fastapi import APIRouter
from gladia_api_utils.submodules import TaskRouter

router = APIRouter()

inputs = [
    {
        "type": "text",
        "name": "text",
        "default": "The Crown is a historical drama streaming television series about the reign of Queen Elizabeth II, created and principally written by Peter Morgan, and produced by Left Bank Pictures and Sony Pictures Television for Netflix.",
        "placeholder": "Insert the text to summarize here",
    }
]

output = {
    "name": "keywords",
    "type": "list",
    "example": '[["crown", 0.5544], ["queen", 0.4428], ["kingdom", 0.4382], ["macmillan", 0.4289], ["prince", 0.3954]]',
}

TaskRouter(
    router=router,
    input=inputs,
    output=output,
    default_model="keybert-paraphrase-MiniLM-L6-v2",
)

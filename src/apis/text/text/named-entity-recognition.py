from fastapi import APIRouter
from gladia_api_utils.submodules import TaskRouter

router = APIRouter()

inputs = [
    {
        "type": "text",
        "name": "text",
        "example": "Hugging Face Inc. is a company based in New York City. Its headquarters are in DUMBO, therefore very close to the Manhattan Bridge.",
        "placeholder": "Insert the text to anlayse sentiment from",
    }
]

output = {
    "name": "recognized_entities",
    "type": "list",
    "example": '{"prediction":[{"entity_group": "ORG", "score": 0.5587025284767151, "word": "Gladia", "start": 26, "end": 32}]',
}

TaskRouter(
    router=router,
    input=inputs,
    output=output,
    default_model="dbmdz-bert-large-cased-finetuned-conll03-english",
)

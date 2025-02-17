from fastapi import APIRouter
from gladia_api_utils.submodules import TaskRouter

router = APIRouter()

inputs = [
    {
        "type": "string",
        "name": "input_string_language_1",
        "example": "Sentence from first language",
        "placeholder": "Insert the Sentence from first language",
    },
    {
        "type": "string",
        "name": "input_string_language_2",
        "example": "来自 第一 语言的 句子",
        "placeholder": "Insert the Sentence from second language",
    },
]

output = {
    "name": "word_aligment",
    "type": "array",
    "example": '[{"source": "Sentence","target": "来自"}]',
}

TaskRouter(
    router=router,
    input=inputs,
    output=output,
    default_model="bert-base-multilingual-cased",
)

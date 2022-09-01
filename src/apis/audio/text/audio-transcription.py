from fastapi import APIRouter
from gladia_api_utils.submodules import TaskRouter

inputs = [
    {
        "type": "audio",
        "name": "audio",
        "example": "http://files.gladia.io/test/test.mp3",
        "examples": [
            "http://files.gladia.io/examples/audio/text/audio-transcription/audio.m4a",
            "http://files.gladia.io/examples/audio/text/audio-transcription/audio.mp3",
            "http://files.gladia.io/examples/audio/text/audio-transcription/audio.wav",
        ],
        "placeholder": "Audio to transcribe",
    },
    {
        "type": "string",
        "name": "language",
        "default": "en",
        "example": "en",
        "placeholder": "Language of the audio",
    },
]

output = {
    "name": "transcription",
    "type": "string",
    "example": "I'm telling you that this is the tools i've seen so far.",
}

router = APIRouter()

TaskRouter(
    router=router, input=inputs, output=output, default_model="coqui_english_huge_vocab"
)

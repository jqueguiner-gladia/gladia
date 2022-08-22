from fastapi.responses import Response
from typing import Any, Optional


class ImageResponse(Response):
    media_type = "image/*"

    schema = {"type": "string", "format": "binary", "data_type": "image"}


class AudioResponse(Response):
    media_type = "audio/*"

    schema = {"type": "string", "format": "binary", "data_type": "audio"}


class VideoResponse(Response):
    media_type = "video"

    schema = {"type": "string", "format": "binary", "data_type": "video"}

class JsonResponse(Response):
    media_type = "application/json"

    schema = {
        "type": Any, 
        "prediction": Any, 
        "prediction_raw": Any
    }

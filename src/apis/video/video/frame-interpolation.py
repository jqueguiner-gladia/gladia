from fastapi import APIRouter
from gladia_api_utils.submodules import TaskRouter

inputs = [
    {
        "type": "video",
        "name": "video",
        "default": "video to interpolate",
        "placeholder": "File to the video to interpolate from",
    }
]

output = {
    "name": "interpolated_video",
    "type": "video",
    "example": "interpolated_video",
}

router = APIRouter()

TaskRouter(
    router=router,
    input=input,
    output=output,
    default_model="deep-animation-video-interpolation-in-the-wild",
)

from fastapi import APIRouter

from ai_api_utils.submodules import TaskRouter

router = APIRouter()
TaskRouter(
    router=router,
    input="video",
    output="video",
    default_model="deep-animation-video-interpolation-in-the-wild",
)

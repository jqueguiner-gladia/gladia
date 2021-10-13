from fastapi import APIRouter

from unifai-api-utils.submodules import TaskRouter

router = APIRouter()
TaskRouter(
    router=router,
    input="image",
    output="image",
    default_model="bringing-old-photos-back-to-life",
)

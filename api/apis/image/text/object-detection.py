
from fastapi import APIRouter

from ai_api_utils.submodules import TaskRouter

router = APIRouter()
TaskRouter(router=router, input="image", output="text", default_model="detr")
from fastapi import APIRouter
from ai_api_utils.submodules import TaskRouter

router = APIRouter()

input = [
    {
        "type": "text",
        "name": "code_snippet",
        "default": """
def recur_fibo(n):
   if n <= 1:
       return n
   else:
       return(recur_fibo(n-1) + recur_fibo(n-2))
""",
        "placeholder": "Code snippet to analyze",
        "tooltip": "Insert the code snippet to analyze",
    }
]

output = [
    {
        "type": str,
        "name": "programming-language",
    }
]


TaskRouter(router=router, input=input, output=output, default_model="aliostad")

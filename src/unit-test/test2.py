import requests

url = "https://v2-aipi.gladia.io"
path = "/text/text/next-word-prediction/"
header = {}

def get_task_models_to_test(url, path, header={}, specific_models=[], default_models_only=False):
    endpoints = requests.get(f"{url}/openapi.json", headers=header).json()

    task_response = requests.get(f"{url}{path}", headers=header).json()
    if specific_models != []:
        models = [
            model for model in task_response["models"] if model in specific_models
        ]
    elif default_models_only:
        details = endpoints["paths"][path]
        models = [details["post"]["parameters"][0]["schema"]["default"]]
    else:
        models = [key for key in task_response["models"].keys()]
    return models


sepcific_models = ['roberta-bacsdsse', 'coucou']
models = get_task_models_to_test(url, path, specific_models=sepcific_models, default_models_only=True)
print(models)
import json
import requests
import csv
from pprint import pprint
data={}
with open('unit-test/latency_diagnostic.json') as json_file:
    data = json.load(json_file)

def get_default_models():
    default_models = []
    response = requests.get("https://v2-aipi.gladia.io/openapi.json")
    endpoints = response.json()
    for task, details in endpoints["paths"].items():
        default_model = details["post"]["parameters"][0]["schema"]["default"]
        default_models.append(
            (task, default_model)
        )
    return default_models

default_models = get_default_models()
data_csv = []
data_csv.append(["task", "model", "is_default_model", "latency_with_loading (s)"])
model_latency_with_loading = {}
jpg_latency_without_loading = []
png_latency_without_loading = []
for task, model_dict in data.items():
    for model, value in model_dict.items():
        latency_with_loading = value["requests"][0]["duration"]
        model_latency_with_loading[model]=latency_with_loading
        is_default_model = True if (task, model) in default_models else False
        data_csv.append([task, model, is_default_model, latency_with_loading])
        for test in value["requests"]:
            if test["file"].get("image", None):
                if test["file"]["image"][0] == "test.jpg":
                    jpg_latency_without_loading.append(test["duration"])
                else:
                    png_latency_without_loading.append(test["duration"])

print("Model latency with loading time:")
pprint(model_latency_with_loading)
print("Total time of jpg requests:")
print(f"{round(sum(jpg_latency_without_loading), 2)}s")
print("Total time of png requests")
print(f"{round(sum(png_latency_without_loading), 2)}s")
print("Average time of jpg requests")
print(f"{round(sum(jpg_latency_without_loading)/len(jpg_latency_without_loading), 2)}s")
print("Average time of png requests")
print(f"{round(sum(png_latency_without_loading)/len(png_latency_without_loading), 2)}s")

with open('unit-test/latency_diagnostic.csv', 'w', encoding='UTF8', newline='') as f:
    writer = csv.writer(f)
    writer.writerows(data_csv)